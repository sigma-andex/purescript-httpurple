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
  , serve'
  , serveNodeMiddleware
  ) where

import Prelude

import Control.Monad.Cont (runContT)
import Control.Monad.Error.Class (class MonadError)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Posix.Signal (Signal(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Choice ((|||))
import Effect (Effect)
import Effect.Aff (Aff, catchError, launchAff_, message)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Console (error)
import Effect.Exception (Error)
import HTTPurple.NodeMiddleware (MiddlewareResult(..), NextInvocation(..), NodeMiddlewareStack(..))
import HTTPurple.Request (ExtRequest, ExtRequestNT, Request, RequestR, fromHTTPRequestExt, fromHTTPRequestUnit)
import HTTPurple.Response (Response, internalServerError, notFound, send)
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

type ListenOptionsR m =
  ( hostname :: Maybe String
  , port :: Maybe Int
  , backlog :: Maybe Int
  , closingHandler :: Maybe ClosingHandler
  , notFoundHandler :: Maybe (Request Unit -> m Response)
  , onStarted :: Maybe (Effect Unit)
  , certFile :: Maybe String
  , keyFile :: Maybe String
  )

type ListenOptions m = { | ListenOptionsR m }

type RoutingSettingsR m route output r =
  ( route :: RD.RouteDuplex' route
  , router :: ExtRequest route output -> m Response
  | r
  )

type MiddlewareSettingsR :: Row Type -> Row Type -> Row Type
type MiddlewareSettingsR input output =
  ( nodeMiddleware :: NodeMiddlewareStack input output
  )

type BasicRoutingSettings m route = { | RoutingSettingsR m route () () }

type ExtRoutingSettings :: (Type -> Type) -> Type -> Row Type -> Row Type -> Type
type ExtRoutingSettings m route input output = { | RoutingSettingsR m route output + MiddlewareSettingsR input output }

-- | Given a router, handle unhandled exceptions it raises by
-- | responding with 500 Internal Server Error.
onError500 :: forall m request. MonadError Error m => MonadAff m => (request -> m Response) -> request -> m Response
onError500 router request =
  catchError (router request) \err -> do
    liftEffect $ error $ message err
    internalServerError "Internal server error"

defaultMiddlewareErrorHandler :: forall m. MonadAff m => Error -> Request Unit -> m Response
defaultMiddlewareErrorHandler err _ = do
  liftEffect $ error $ message err
  internalServerError "Internal server error"

-- | handle requests without a routing adt.
handleRequestUnit ::
  forall m.
  MonadError Error m =>
  MonadAff m =>
  (Request Unit -> m Response) ->
  IncomingMessage IMServer ->
  ServerResponse ->
  m Unit
handleRequestUnit router request httpresponse =
  void $ fromHTTPRequestUnit request
    >>= (onError500 router)
    >>= send httpresponse

-- | This function takes a record containing the 
-- |  * route - the routing adt
-- |  * router - the request handler (a method which takes a `Request` and returns a
-- | `m Response`)
-- |  * notFoundHandler - a handler to handle 404s 
-- | as well as  an HTTP `Request` and an HTTP `Response`. It runs the
-- | request, extracts the `Response` from the `m Response`, and sends the
-- | `Response` to the HTTP `Response`.
handleExtRequest ::
  forall m ctx ctxRL thru route.
  MonadError Error m =>
  MonadAff m =>
  Union ctx thru ctx =>
  RowToList ctx ctxRL =>
  Keys ctx =>
  Nub (RequestR route ctx) (RequestR route ctx) =>
  { route :: RD.RouteDuplex' route
  , router :: ExtRequestNT route ctx -> m Response
  , notFoundHandler :: Request Unit -> m Response
  } ->
  IncomingMessage IMServer ->
  ServerResponse ->
  m Unit
handleExtRequest { route, router, notFoundHandler } req resp = do
  httpurpleReq <- fromHTTPRequestExt route (Proxy :: Proxy ctx) req
  httpurpleResp <- (notFoundHandler ||| onError500 router) httpurpleReq
  send resp httpurpleResp

handleRequest ::
  forall ctx ctxRL thru route m.
  MonadAff m =>
  MonadError Error m =>
  Union ctx thru ctx =>
  RowToList ctx ctxRL =>
  Keys ctx =>
  Nub (RequestR route ctx) (RequestR route ctx) =>
  { route :: RD.RouteDuplex' route
  , router :: ExtRequestNT route ctx -> m Response
  , notFoundHandler :: Request Unit -> m Response
  } ->
  IncomingMessage IMServer ->
  ServerResponse ->
  m Unit
handleRequest settings request response = void $ handleExtRequest settings request response

handleExtRequestWithMiddleware ::
  forall input output outputRL thru route m.
  MonadAff m =>
  MonadError Error m =>
  Union output thru output =>
  RowToList output outputRL =>
  Keys output =>
  Nub (RequestR route output) (RequestR route output) =>
  { route :: RD.RouteDuplex' route
  , nodeMiddleware :: NodeMiddlewareStack input output
  , router :: ExtRequestNT route output -> m Response
  , notFoundHandler :: Request Unit -> m Response
  } ->
  IncomingMessage IMServer ->
  ServerResponse ->
  m Unit
handleExtRequestWithMiddleware { route, nodeMiddleware: NodeMiddlewareStack middleware, router, notFoundHandler } req resp = void $ do
  eff <- liftEffect $ flip runContT (coerce >>> pure) $ middleware (MiddlewareResult { request: req, response: resp, middlewareResult: NotCalled })
  executeHandler eff
  where

  executeHandler :: MiddlewareResult output -> m Unit
  executeHandler (MiddlewareResult { request, response, middlewareResult: ProcessingFailed error }) =
    handleRequestUnit (defaultMiddlewareErrorHandler error) request response
  executeHandler (MiddlewareResult { request, response, middlewareResult: ProcessingSucceeded }) =
    handleExtRequest { route, router, notFoundHandler } request response
  executeHandler (MiddlewareResult { middlewareResult: NotCalled }) =
    pure unit

defaultNotFoundHandler :: forall route m. MonadAff m => Request route -> m Response
defaultNotFoundHandler = const notFound

asExtended ::
  forall route ext m.
  (ExtRequest route ext -> m Response) ->
  ExtRequestNT route ext ->
  m Response
asExtended = lcmap unwrap

serveInternal ::
  forall m route from fromRL via missing missingList input output outputRL thru.
  MonadAff m =>
  MonadError Error m =>
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR m) =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  Union output thru output =>
  RowToList output outputRL =>
  KeysRL outputRL =>
  Nub (RequestR route output) (RequestR route output) =>
  (m Unit -> Aff Unit) ->
  { | from } ->
  Maybe (NodeMiddlewareStack input output) ->
  { route :: RD.RouteDuplex' route
  , router :: ExtRequestNT route output -> m Response
  } ->
  ServerM
serveInternal performM inputOptions maybeNodeMiddleware settings = do
  let
    filledOptions :: ListenOptions m
    filledOptions = justifill inputOptions

    host = fromMaybe defaultHostname filledOptions.hostname
    port = fromMaybe defaultPort filledOptions.port
    onStarted = fromMaybe (defaultOnStart host port) filledOptions.onStarted

    options =
      { host
      , port
      , backlog: fromMaybe 511 filledOptions.backlog
      }

    routingSettings = merge settings { notFoundHandler: fromMaybe defaultNotFoundHandler $ filledOptions.notFoundHandler }

    handler req rep = launchAff_ $ performM $ case maybeNodeMiddleware of
      Just nodeMiddleware -> handleExtRequestWithMiddleware (merge routingSettings { nodeMiddleware }) req rep
      Nothing -> handleRequest routingSettings req rep
    sslOptions = { certFile: _, keyFile: _ } <$> filledOptions.certFile <*> filledOptions.keyFile
  netServer <- case sslOptions of
    Just { certFile, keyFile } -> do
      cert' <- liftEffect $ readFile certFile
      key' <- liftEffect $ readFile keyFile
      server <- liftEffect $ HTTPS.createSecureServer'
        { key: [ key' ]
        , cert: [ cert' ]
        }
      liftEffect $ EE.on_ HServer.requestH handler server
      pure $ HServer.toNetServer server
    Nothing -> do
      server <- liftEffect $ HTTP.createServer
      liftEffect $ EE.on_ HServer.requestH handler server
      pure $ HServer.toNetServer server
  liftEffect $ EE.on_ listeningH onStarted netServer
  liftEffect $ listenTcp netServer options
  let closingHandler = NServer.close netServer
  srv <- registerClosingHandler filledOptions.closingHandler (\eff -> eff *> closingHandler)
  liftEffect srv

serve ::
  forall route from fromRL via missing missingList.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR Aff) =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  { | from } ->
  BasicRoutingSettings Aff route ->
  ServerM
serve inputOptions { route, router } = do
  let
    extendedSettings = { route, router: asExtended router }
  serveInternal identity inputOptions Nothing extendedSettings

--| `serve` generalized to any MonadAff
--|
--| ```
--| module Main where
--|
--| import Prelude hiding ((/))
--| import HTTPurple
--|
--| import Effect (Effect)
--| import Effect.Aff (Aff, launchAff_)
--| import Effect.Console (log)
--| import Control.Monad.Logger.Trans (LoggerT)
--|
--| type M = LoggerT Aff
--|
--| data Route = Hello String
--|
--| route :: RouteDuplex'
--| route = mkRoute { "Hello": "hello" / segment }
--|
--| router :: ExtRequest Route () -> Response M
--| router {route: Hello m} = ok $ "hi, " <> m <> "!"
--|
--| main :: Effect Unit
--| main =
--|  let
--|    launchM m = runLoggerT m (liftEffect <<< log)
--|  in
--|    serve' launchM {port: 8080} {route, router}
--| ```
serve' ::
  forall m route from fromRL via missing missingList.
  MonadAff m =>
  MonadError Error m =>
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR m) =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  (m Unit -> Aff Unit) ->
  { | from } ->
  BasicRoutingSettings m route ->
  ServerM
serve' ma inputOptions { route, router } = do
  let
    extendedSettings = { route, router: asExtended router }
  serveInternal ma inputOptions Nothing extendedSettings

serveNodeMiddleware ::
  forall route from fromRL via missing missingList input output outputRL thru.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR Aff) =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  Union output thru output =>
  RowToList output outputRL =>
  KeysRL outputRL =>
  Nub (RequestR route output) (RequestR route output) =>
  { | from } ->
  ExtRoutingSettings Aff route input output ->
  ServerM
serveNodeMiddleware inputOptions { route, router, nodeMiddleware } = do
  let
    extendedSettings = { route, router: asExtended router }

  serveInternal identity inputOptions (Just nodeMiddleware) extendedSettings

registerClosingHandler :: forall m. MonadEffect m => Maybe ClosingHandler -> (Effect Unit -> Effect Unit) -> m ServerM
registerClosingHandler (Just NoClosingHandler) closingHandler = pure $ pure closingHandler
registerClosingHandler _ closingHandler = pure $ liftEffect do
  Process.process # EE.on_ (mkSignalH SIGINT) (closingHandler $ log "Aye, stopping service now. Goodbye!")
  Process.process # EE.on_ (mkSignalH SIGTERM) (closingHandler $ log "Arrgghh I got stabbed in the back 🗡 ... good...bye...")
  pure closingHandler

defaultHostname :: String
defaultHostname = "0.0.0.0"

defaultPort :: Int
defaultPort = 8080

defaultOnStart :: forall m. MonadEffect m => String -> Int -> m Unit
defaultOnStart hostname port = liftEffect $ log $ "HTTPurple 🪁 up and running on http://" <> hostname <> ":" <> show port
