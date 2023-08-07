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
import HTTPurple.Request (ExtRequest, ExtRequestNT, Request, RequestR, fromHTTPRequestExt, fromHTTPRequestUnit)
import HTTPurple.Response (Response, ResponseM, internalServerError, notFound, send)
import Justifill (justifill)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Node.EventEmitter as EE
import Node.FS.Sync (readFile)
import Node.HTTP as HTTP
import Node.HTTP.Server as HServer
import Node.HTTP.Types (IMServer, IncomingMessage, ServerResponse)
import Node.HTTPS as HTTPS
import Node.Net.Server (listenTcp, listeningH)
import Node.Net.Server as NServer
import Node.Process (mkSignalH)
import Node.Process as Process
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (merge)
import Record.Studio.Keys (class Keys, class KeysRL)
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
  IncomingMessage IMServer ->
  ServerResponse ->
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
  Keys ctx =>
  Nub (RequestR route ctx) (RequestR route ctx) =>
  { route :: RD.RouteDuplex' route
  , router :: ExtRequestNT route ctx -> ResponseM
  , notFoundHandler :: Request Unit -> ResponseM
  } ->
  IncomingMessage IMServer ->
  ServerResponse ->
  Aff Unit
handleExtRequest { route, router, notFoundHandler } req resp = do
  httpurpleReq <- fromHTTPRequestExt route (Proxy :: Proxy ctx) req
  httpurpleResp <- (notFoundHandler ||| onError500 router) httpurpleReq
  send resp httpurpleResp

handleRequest ::
  forall ctx ctxRL thru route.
  Union ctx thru ctx =>
  RowToList ctx ctxRL =>
  Keys ctx =>
  Nub (RequestR route ctx) (RequestR route ctx) =>
  { route :: RD.RouteDuplex' route
  , router :: ExtRequestNT route ctx -> ResponseM
  , notFoundHandler :: Request Unit -> ResponseM
  } ->
  IncomingMessage IMServer ->
  ServerResponse ->
  Effect Unit
handleRequest settings request response = void $ runAff (\_ -> pure unit) $ handleExtRequest settings request response

handleExtRequestWithMiddleware ::
  forall input output outputRL thru route.
  Union output thru output =>
  RowToList output outputRL =>
  Keys output =>
  Nub (RequestR route output) (RequestR route output) =>
  { route :: RD.RouteDuplex' route
  , nodeMiddleware :: NodeMiddlewareStack input output
  , router :: ExtRequestNT route output -> ResponseM
  , notFoundHandler :: Request Unit -> ResponseM
  } ->
  IncomingMessage IMServer ->
  ServerResponse ->
  Effect Unit
handleExtRequestWithMiddleware { route, nodeMiddleware: NodeMiddlewareStack middleware, router, notFoundHandler } req resp = void $ runAff (\_ -> pure unit) $ do
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
  KeysRL outputRL =>
  Nub (RequestR route output) (RequestR route output) =>
  { | from } ->
  Maybe (NodeMiddlewareStack input output) ->
  { route :: RD.RouteDuplex' route
  , router :: ExtRequestNT route output -> ResponseM
  } ->
  ServerM
serveInternal inputOptions maybeNodeMiddleware settings = do
  let
    filledOptions :: ListenOptions
    filledOptions = justifill inputOptions

    host = fromMaybe defaultHostname filledOptions.hostname
    port = fromMaybe defaultPort filledOptions.port
    onStarted = fromMaybe (defaultOnStart host port) filledOptions.onStarted

    options =
      { host
      , port
      , backlog: fromMaybe 511 filledOptions.backlog
      }

    routingSettings = merge settings { notFoundHandler: fromMaybe defaultNotFoundHandler filledOptions.notFoundHandler }

    handler = case maybeNodeMiddleware of
      Just nodeMiddleware -> handleExtRequestWithMiddleware $ merge routingSettings { nodeMiddleware }
      Nothing -> handleRequest routingSettings
    sslOptions = { certFile: _, keyFile: _ } <$> filledOptions.certFile <*> filledOptions.keyFile
  netServer <- case sslOptions of
    Just { certFile, keyFile } -> do
      cert' <- readFile certFile
      key' <- readFile keyFile
      server <- HTTPS.createSecureServer'
        { key: [ key' ]
        , cert: [ cert' ]
        }
      server # EE.on_ HServer.requestH handler
      pure $ HServer.toNetServer server
    Nothing -> do
      server <- HTTP.createServer
      server # EE.on_ HServer.requestH handler
      pure $ HServer.toNetServer server
  netServer # EE.on_ listeningH onStarted
  listenTcp netServer options
  let closingHandler = NServer.close netServer
  liftEffect $ registerClosingHandler filledOptions.closingHandler (\eff -> eff *> closingHandler)

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
    extendedSettings = { route, router: asExtended router }

  serveInternal inputOptions Nothing extendedSettings

serveNodeMiddleware ::
  forall route from fromRL via missing missingList input output outputRL thru.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR) =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  Union output thru output =>
  RowToList output outputRL =>
  KeysRL outputRL =>
  Nub (RequestR route output) (RequestR route output) =>
  { | from } ->
  ExtRoutingSettings route input output ->
  ServerM
serveNodeMiddleware inputOptions { route, router, nodeMiddleware } = do
  let
    extendedSettings = { route, router: asExtended router }

  serveInternal inputOptions (Just nodeMiddleware) extendedSettings

registerClosingHandler :: Maybe ClosingHandler -> (Effect Unit -> Effect Unit) -> ServerM
registerClosingHandler (Just NoClosingHandler) closingHandler = pure closingHandler
registerClosingHandler _ closingHandler = do
  Process.process # EE.on_ (mkSignalH SIGINT) (closingHandler $ log "Aye, stopping service now. Goodbye!")
  Process.process # EE.on_ (mkSignalH SIGTERM) (closingHandler $ log "Arrgghh I got stabbed in the back 🗡 ... good...bye...")
  pure closingHandler

defaultHostname :: String
defaultHostname = "0.0.0.0"

defaultPort :: Int
defaultPort = 8080

defaultOnStart :: String -> Int -> Effect Unit
defaultOnStart hostname port = log $ "HTTPurple 🪁 up and running on http://" <> hostname <> ":" <> show port
