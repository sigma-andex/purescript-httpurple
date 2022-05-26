module HTTPurple.Server
  ( ClosingHandler(..)
  , ListenOptions
  , ListenOptionsR
  , RoutingSettings
  , RoutingSettingsR
  , ServerM
  , serve
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Options ((:=))
import Data.Posix.Signal (Signal(..))
import Data.Profunctor.Choice ((|||))
import Effect (Effect)
import Effect.Aff (catchError, message, runAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (error)
import HTTPurple.Request (Request, fromHTTPRequest)
import HTTPurple.Response (ResponseM, internalServerError, notFound, send)
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
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Routing.Duplex as RD

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

-- | Given a router, handle unhandled exceptions it raises by
-- | responding with 500 Internal Server Error.
onError500 :: forall route. (Request route -> ResponseM) -> Request route -> ResponseM
onError500 router request =
  catchError (router request) \err -> do
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
  case filledOptions.closingHandler of
    Just NoClosingHandler -> pure closingHandler
    _ -> do
      onSignal SIGINT $ closingHandler $ log "Ok, ok, stopping the service now. Goodbye!"
      onSignal SIGTERM $ closingHandler $ log "Arrgghh I got stabbed in the back 🗡 ... good...bye..."
      pure closingHandler

defaultHostname :: String
defaultHostname = "0.0.0.0"

defaultPort :: Int
defaultPort = 8080

defaultOnStart :: String -> Int -> Effect Unit
defaultOnStart hostname port = log $ "HTTPurple 🪁 up and running on http://" <> hostname <> ":" <> show port
