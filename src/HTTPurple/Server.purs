module HTTPurple.Server
  ( ClosingHandler(..)
  , ListenOptions
  , ListenOptionsR
  , RoutingSettings
  , RoutingSettingsR
  , ServerM
  , defaultMiddlewareErrorHandler
  , serve
  , serveExtended
  ) where

import Prelude

import Control.Monad.Cont (runContT)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Options ((:=))
import Data.Posix.Signal (Signal(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Choice ((|||))
import Effect (Effect)
import Effect.Aff (Aff, catchError, message, runAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (error)
import Effect.Exception (Error)
import HTTPurple.NodeMiddleware (MiddlewareResult(..), NextInvocation(..), NodeMiddlewareStack(..))
import HTTPurple.Record.Extra as Extra
import HTTPurple.Request (ExtRequestNT, Request, RequestR, ExtRequest, fromHTTPRequest, fromHTTPRequestExt, fromHTTPRequestUnit)
import HTTPurple.Response (Response, ResponseM, internalServerError, notFound, send)
import Justifill (justifill)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.HTTP (ListenOptions, Request, Response, createServer) as HTTP
import Node.HTTP (close, listen)
import Node.HTTP.Secure (cert, certString, key, keyString)
import Node.HTTP.Secure (createServer) as HTTPS
import Node.Process (onSignal)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (merge)
import Routing.Duplex as RD
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

-- | The `ServerM` is just an `Effect` containing a callback to close the
-- | server. This type is the return type of the HTTPurple serve and related
-- | methods.
type ServerM = Effect (Effect Unit -> Effect Unit)

data ClosingHandler = DefaultClosingHandler | NoClosingHandler

type ListenOptionsR =
  ( hostname :: Maybe String
  , port :: Maybe Int
  , backlog :: Maybe Int
  , closingHandler :: Maybe ClosingHandler
  , notFoundHandler :: Maybe (Request Unit -> ResponseM)
  , onStarted :: Maybe (Effect Unit)
  , certFile :: Maybe String
  , keyFile :: Maybe String
  )

type ListenOptions = { | ListenOptionsR }

type RoutingSettingsR route =
  ( route :: RD.RouteDuplex' route
  , router :: Request route -> ResponseM
  )

type RoutingSettings route = { | RoutingSettingsR route }

type ExtRoutingSettingsR route output r =
  ( route :: RD.RouteDuplex' route
  , router :: ExtRequest route output -> ResponseM
  | r
  )

type MiddlewareSettingsR :: Row Type -> Row Type -> Row Type
type MiddlewareSettingsR input output =
  ( nodeMiddleware :: NodeMiddlewareStack input output
  )

type ExtRoutingSettings :: Type -> Row Type -> Row Type -> Type
type ExtRoutingSettings route input output = { | ExtRoutingSettingsR route output + MiddlewareSettingsR input output }

-- | Given a router, handle unhandled exceptions it raises by
-- | responding with 500 Internal Server Error.
onError500 :: forall request. (request -> ResponseM) -> request -> ResponseM
onError500 router request =
  catchError (router request) \err -> do
    liftEffect $ error $ message err
    internalServerError "Internal server error"

defaultMiddlewareErrorHandler :: Error -> Request Unit -> Aff Response
defaultMiddlewareErrorHandler err _ = do
  liftEffect $ error $ message err
  internalServerError "Internal server error"

-- | This function takes a method which takes a `Request` and returns a
-- | `ResponseM`, an HTTP `Request`, and an HTTP `Response`. It runs the
-- | request, extracts the `Response` from the `ResponseM`, and sends the
-- | `Response` to the HTTP `Response`.
handleRequest ::
  forall route.
  { route :: RD.RouteDuplex' route
  , router :: Request route -> ResponseM
  , notFoundHandler :: Request Unit -> ResponseM
  } ->
  HTTP.Request ->
  HTTP.Response ->
  Effect Unit
handleRequest { route, router, notFoundHandler } request httpresponse =
  void $ runAff (\_ -> pure unit) $ fromHTTPRequest route request
    >>= (notFoundHandler ||| onError500 router)
    >>= send httpresponse

handleRequestUnit ::
  (Request Unit -> ResponseM) ->
  HTTP.Request ->
  HTTP.Response ->
  Effect Unit
handleRequestUnit router request httpresponse =
  void $ runAff (\_ -> pure unit) $ fromHTTPRequestUnit request
    >>= (onError500 router)
    >>= send httpresponse

handleExtRequestNT ::
  forall ctx ctxRL thru route.
  Union ctx thru ctx =>
  RowToList ctx ctxRL =>
  Extra.Keys ctxRL =>
  Nub (RequestR route ctx) (RequestR route ctx) =>
  { route :: RD.RouteDuplex' route
  , router :: ExtRequestNT route ctx -> ResponseM
  , notFoundHandler :: Request Unit -> ResponseM
  } ->
  HTTP.Request ->
  HTTP.Response ->
  Aff Unit
handleExtRequestNT { route, router, notFoundHandler } req resp = do
  httpurpleReq <- fromHTTPRequestExt route (Proxy :: Proxy ctx) req
  httpurpleResp <- (notFoundHandler ||| onError500 router) httpurpleReq
  send resp httpurpleResp

handleExtRequestNTWithMiddleware ::
  forall input output outputRL thru route.
  Union output thru output =>
  RowToList output outputRL =>
  Extra.Keys outputRL =>
  Nub (RequestR route output) (RequestR route output) =>
  { route :: RD.RouteDuplex' route
  , nodeMiddleware :: NodeMiddlewareStack input output
  , router :: ExtRequestNT route output -> ResponseM
  , notFoundHandler :: Request Unit -> ResponseM
  } ->
  HTTP.Request ->
  HTTP.Response ->
  Effect Unit
handleExtRequestNTWithMiddleware { route, nodeMiddleware: NodeMiddlewareStack middleware, router, notFoundHandler } req resp = void $ runAff (\_ -> pure unit) $ do
  eff <- liftEffect $ flip runContT (coerce >>> pure) $ middleware (MiddlewareResult { request: req, response: resp, middlewareResult: NotCalled })
  executeHandler eff
  where

  executeHandler :: MiddlewareResult output -> Aff Unit
  executeHandler (MiddlewareResult { request, response, middlewareResult: ProcessingFailed error }) =
    liftEffect $ handleRequestUnit (defaultMiddlewareErrorHandler error) request response
  executeHandler (MiddlewareResult { request, response, middlewareResult: ProcessingSucceeded }) =
    handleExtRequestNT { route, router, notFoundHandler } request response
  executeHandler (MiddlewareResult { middlewareResult: NotCalled }) =
    pure unit

defaultNotFoundHandler :: forall route. Request route -> ResponseM
defaultNotFoundHandler = const notFound

justifillListenOptions ::
  forall from fromRL via missing missingList.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR) =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  { | from } ->
  ListenOptions
justifillListenOptions = justifill

-- | Given a `ListenOptions` and a `RoutingSettings`, creates and
-- | runs a HTTPurple server.
serve ::
  forall route from fromRL via missing missingList.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR) =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  { | from } ->
  RoutingSettings route ->
  ServerM
serve inputOptions { route, router } = do
  let
    filledOptions :: ListenOptions
    filledOptions = justifillListenOptions inputOptions

    hostname = fromMaybe defaultHostname filledOptions.hostname
    port = fromMaybe defaultPort filledOptions.port
    onStarted = fromMaybe (defaultOnStart hostname port) filledOptions.onStarted

    options :: HTTP.ListenOptions
    options =
      { hostname
      , port
      , backlog: filledOptions.backlog
      }

    routingSettings =
      { route
      , router
      , notFoundHandler: fromMaybe defaultNotFoundHandler filledOptions.notFoundHandler
      }

    sslOptions = { certFile: _, keyFile: _ } <$> filledOptions.certFile <*> filledOptions.keyFile
  server <- case sslOptions of
    Just { certFile, keyFile } ->
      do
        cert' <- readTextFile UTF8 certFile
        key' <- readTextFile UTF8 keyFile
        let sslOpts = key := keyString key' <> cert := certString cert'
        HTTPS.createServer sslOpts (handleRequest routingSettings)
    Nothing -> HTTP.createServer (handleRequest routingSettings)
  listen server options onStarted
  let closingHandler = close server
  registerClosingHandler filledOptions.closingHandler closingHandler

asExtended
  :: forall route ext m
  . (ExtRequest route ext -> m Response)
  -> ExtRequestNT route ext
  -> m Response
asExtended = lcmap unwrap

serveExtended ::
  forall route from fromRL via missing missingList input output outputRL thru.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR) =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  Union output thru output =>
  RowToList output outputRL =>
  Extra.Keys outputRL =>
  Nub (RequestR route output) (RequestR route output) =>
  { | from } ->
  ExtRoutingSettings route input output ->
  ServerM
serveExtended inputOptions settings = do
  let
    filledOptions :: ListenOptions
    filledOptions = justifillListenOptions inputOptions

    hostname = fromMaybe defaultHostname filledOptions.hostname
    port = fromMaybe defaultPort filledOptions.port
    onStarted = fromMaybe (defaultOnStart hostname port) filledOptions.onStarted

    options :: HTTP.ListenOptions
    options =
      { hostname
      , port
      , backlog: filledOptions.backlog
      }

    extendedSettings = settings { router = asExtended settings.router }

    routingSettings ::
      { route :: RD.RouteDuplex' route
      , nodeMiddleware :: NodeMiddlewareStack input output
      , router :: ExtRequestNT route output -> ResponseM
      , notFoundHandler :: Request Unit -> ResponseM
      }
    routingSettings = merge extendedSettings { notFoundHandler: fromMaybe defaultNotFoundHandler filledOptions.notFoundHandler }

    sslOptions = { certFile: _, keyFile: _ } <$> filledOptions.certFile <*> filledOptions.keyFile
  server <- case sslOptions of
    Just { certFile, keyFile } ->
      do
        cert' <- readTextFile UTF8 certFile
        key' <- readTextFile UTF8 keyFile
        let sslOpts = key := keyString key' <> cert := certString cert'
        HTTPS.createServer sslOpts (handleExtRequestNTWithMiddleware routingSettings)
    Nothing -> HTTP.createServer (handleExtRequestNTWithMiddleware routingSettings)
  listen server options onStarted
  let closingHandler = close server
  registerClosingHandler filledOptions.closingHandler closingHandler

registerClosingHandler :: Maybe ClosingHandler -> (Effect Unit -> Effect Unit) -> ServerM
registerClosingHandler (Just NoClosingHandler) closingHandler = pure closingHandler
registerClosingHandler _ closingHandler = do
  onSignal SIGINT $ closingHandler $ log "Aye, stopping service now. Goodbye!"
  onSignal SIGTERM $ closingHandler $ log "Arrgghh I got stabbed in the back 🗡 ... good...bye..."
  pure closingHandler

defaultHostname :: String
defaultHostname = "0.0.0.0"

defaultPort :: Int
defaultPort = 8080

defaultOnStart :: String -> Int -> Effect Unit
defaultOnStart hostname port = log $ "HTTPurple 🪁 up and running on http://" <> hostname <> ":" <> show port
