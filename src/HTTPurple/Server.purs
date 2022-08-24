module HTTPurple.Server
  ( ClosingHandler(..)
  , ListenOptions
  , ListenOptionsR
  , BasicRoutingSettings
  , ExtRoutingSettings
  , MiddlewareSettingsR
  , RoutingSettingsR
  , ServerM
  , defaultMiddlewareErrorHandler
  , serve
  , serveNodeMiddleware
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
import HTTPurple.Request (ExtRequest, ExtRequestNT, Request, RequestR, fromHTTPRequestExt, fromHTTPRequestUnit)
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

type RoutingSettingsR route output r =
  ( route :: RD.RouteDuplex' route
  , router :: ExtRequest route output -> ResponseM
  | r
  )

type MiddlewareSettingsR :: Row Type -> Row Type -> Row Type
type MiddlewareSettingsR input output =
  ( nodeMiddleware :: NodeMiddlewareStack input output
  )

type BasicRoutingSettings route = { | RoutingSettingsR route () () }

type ExtRoutingSettings :: Type -> Row Type -> Row Type -> Type
type ExtRoutingSettings route input output = { | RoutingSettingsR route output + MiddlewareSettingsR input output }

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

-- | handle requests without a routing adt.
handleRequestUnit ::
  (Request Unit -> ResponseM) ->
  HTTP.Request ->
  HTTP.Response ->
  Effect Unit
handleRequestUnit router request httpresponse =
  void $ runAff (\_ -> pure unit) $ fromHTTPRequestUnit request
    >>= (onError500 router)
    >>= send httpresponse

-- | This function takes a record containing the 
-- |  * route - the routing adt
-- |  * router - the request handler (a method which takes a `Request` and returns a
-- | `ResponseM`)
-- |  * notFoundHandler - a handler to handle 404s 
-- | as well as  an HTTP `Request` and an HTTP `Response`. It runs the
-- | request, extracts the `Response` from the `ResponseM`, and sends the
-- | `Response` to the HTTP `Response`.
handleExtRequest ::
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
handleExtRequest { route, router, notFoundHandler } req resp = do
  httpurpleReq <- fromHTTPRequestExt route (Proxy :: Proxy ctx) req
  httpurpleResp <- (notFoundHandler ||| onError500 router) httpurpleReq
  send resp httpurpleResp

handleExtRequestWithMiddleware ::
  forall input output outputRL thru route.
  Union output thru output =>
  RowToList output outputRL =>
  Extra.Keys outputRL =>
  Nub (RequestR route output) (RequestR route output) =>
  { route :: RD.RouteDuplex' route
  , nodeMiddleware :: Maybe (NodeMiddlewareStack input output)
  , router :: ExtRequestNT route output -> ResponseM
  , notFoundHandler :: Request Unit -> ResponseM
  } ->
  HTTP.Request ->
  HTTP.Response ->
  Effect Unit

handleExtRequestWithMiddleware { route, nodeMiddleware: Nothing, router, notFoundHandler } req resp = void $ runAff (\_ -> pure unit) $
  handleExtRequest { route, router, notFoundHandler } req resp
handleExtRequestWithMiddleware { route, nodeMiddleware: Just (NodeMiddlewareStack middleware), router, notFoundHandler } req resp = void $ runAff (\_ -> pure unit) $ do
  eff <- liftEffect $ flip runContT (coerce >>> pure) $ middleware (MiddlewareResult { request: req, response: resp, middlewareResult: NotCalled })
  executeHandler eff
  where

  executeHandler :: MiddlewareResult output -> Aff Unit
  executeHandler (MiddlewareResult { request, response, middlewareResult: ProcessingFailed error }) =
    liftEffect $ handleRequestUnit (defaultMiddlewareErrorHandler error) request response
  executeHandler (MiddlewareResult { request, response, middlewareResult: ProcessingSucceeded }) =
    handleExtRequest { route, router, notFoundHandler } request response
  executeHandler (MiddlewareResult { middlewareResult: NotCalled }) =
    pure unit

defaultNotFoundHandler :: forall route. Request route -> ResponseM
defaultNotFoundHandler = const notFound

asExtended ::
  forall route ext m.
  (ExtRequest route ext -> m Response) ->
  ExtRequestNT route ext ->
  m Response
asExtended = lcmap unwrap

serveInternal ::
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
  { route :: RD.RouteDuplex' route
  , nodeMiddleware :: Maybe (NodeMiddlewareStack input output)
  , router :: ExtRequestNT route output -> ResponseM
  } ->
  ServerM
serveInternal inputOptions settings = do
  let
    filledOptions :: ListenOptions
    filledOptions = justifill inputOptions

    hostname = fromMaybe defaultHostname filledOptions.hostname
    port = fromMaybe defaultPort filledOptions.port
    onStarted = fromMaybe (defaultOnStart hostname port) filledOptions.onStarted

    options :: HTTP.ListenOptions
    options =
      { hostname
      , port
      , backlog: filledOptions.backlog
      }

    routingSettings = merge settings { notFoundHandler: fromMaybe defaultNotFoundHandler filledOptions.notFoundHandler }

    sslOptions = { certFile: _, keyFile: _ } <$> filledOptions.certFile <*> filledOptions.keyFile
  server <- case sslOptions of
    Just { certFile, keyFile } ->
      do
        cert' <- readTextFile UTF8 certFile
        key' <- readTextFile UTF8 keyFile
        let sslOpts = key := keyString key' <> cert := certString cert'
        HTTPS.createServer sslOpts (handleExtRequestWithMiddleware routingSettings)
    Nothing -> HTTP.createServer (handleExtRequestWithMiddleware routingSettings)
  listen server options onStarted
  let closingHandler = close server
  registerClosingHandler filledOptions.closingHandler closingHandler

serve ::
  forall route from fromRL via missing missingList.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR) =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  { | from } ->
  BasicRoutingSettings route ->
  ServerM
serve inputOptions { route, router } = do
  let
    extendedSettings = { route, router: asExtended router, nodeMiddleware: Nothing }

  serveInternal inputOptions extendedSettings

serveNodeMiddleware ::
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
serveNodeMiddleware inputOptions { route, router, nodeMiddleware } = do
  let
    extendedSettings = { route, router: asExtended router, nodeMiddleware: Just nodeMiddleware }

  serveInternal inputOptions extendedSettings

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
