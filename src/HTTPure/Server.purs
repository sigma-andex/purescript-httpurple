module HTTPure.Server
  ( ServerM
  , serve
  , serve'
  -- , serveSecure
  -- , serveSecure'
  ) where

import Prelude

import Data.Maybe (Maybe(Nothing), maybe)
import Data.Options ((:=), Options)
import Data.Profunctor.Choice ((|||))
import Effect (Effect)
import Effect.Aff (catchError, message, runAff)
import Effect.Class (liftEffect)
import Effect.Console (error)
import HTTPure.Request (Request, fromHTTPRequest)
import HTTPure.Response (ResponseM, internalServerError, notFound, send)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.HTTP (ListenOptions, listen, close)
import Node.HTTP (Request, Response, createServer) as HTTP
import Node.HTTP.Secure (SSLOptions, key, keyString, cert, certString)
import Node.HTTP.Secure (createServer) as HTTPS
import Routing.Duplex as RD

-- | The `ServerM` is just an `Effect` containing a callback to close the
-- | server. This type is the return type of the HTTPure serve and related
-- | methods.
type ServerM = Effect (Effect Unit -> Effect Unit)

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

-- | Given a `ListenOptions` object, a function mapping `Request` to
-- | `ResponseM`, and a `ServerM` containing effects to run on boot, creates and
-- | runs a HTTPure server without SSL.
serve' ::
  forall route.
  ListenOptions ->
  { route :: RD.RouteDuplex' route
  , router :: Request route -> ResponseM
  , notFoundHandler :: Maybe (Request Unit -> ResponseM)
  } ->
  Effect Unit ->
  ServerM
serve' options { route, router, notFoundHandler } onStarted = do
  server <- HTTP.createServer (handleRequest { route, router, notFoundHandler: maybe defaultNotFoundHandler identity notFoundHandler })
  listen server options onStarted
  pure $ close server

-- | Given a `Options HTTPS.SSLOptions` object and a `HTTP.ListenOptions`
-- | object, a function mapping `Request` to `ResponseM`, and a `ServerM`
-- | containing effects to run on boot, creates and runs a HTTPure server with
-- | SSL.
-- serveSecure' ::
--   forall route.
--   Options SSLOptions ->
--   ListenOptions ->
--   RD.RouteDuplex' route ->
--   (Request route -> ResponseM) ->
--   Effect Unit ->
--   ServerM
-- serveSecure' sslOptions options route router onStarted = do
--   server <- HTTPS.createServer sslOptions (handleRequest route router)
--   listen server options onStarted
--   pure $ close server

-- | Given a port number, return a `HTTP.ListenOptions` `Record`.
listenOptions :: Int -> ListenOptions
listenOptions port =
  { hostname: "0.0.0.0"
  , port
  , backlog: Nothing
  }

-- | Create and start a server. This is the main entry point for HTTPure. Takes
-- | a port number on which to listen, a function mapping `Request` to
-- | `ResponseM`, and a `ServerM` containing effects to run after the server has
-- | booted (usually logging). Returns an `ServerM` containing the server's
-- | effects.
serve ::
  forall route.
  Int ->
  { route :: RD.RouteDuplex' route
  , router :: Request route -> ResponseM
  , notFoundHandler :: Maybe (Request Unit -> ResponseM)
  } ->
  Effect Unit ->
  ServerM
serve = serve' <<< listenOptions

-- | Create and start an SSL server. This method is the same as `serve`, but
-- | takes additional SSL arguments.  The arguments in order are:
-- | 1. A port number
-- | 2. A path to a cert file
-- | 3. A path to a private key file
-- | 4. A handler method which maps `Request` to `ResponseM`
-- | 5. A callback to call when the server is up
-- serveSecure ::
--   forall route.
--   Int ->
--   String ->
--   String ->
--   RD.RouteDuplex' route ->
--   (Request route -> ResponseM) ->
--   Effect Unit ->
--   ServerM
-- serveSecure port certFile keyFile route router onStarted = do
--   cert' <- readTextFile UTF8 certFile
--   key' <- readTextFile UTF8 keyFile
--   let sslOpts = key := keyString key' <> cert := certString cert'
--   serveSecure' sslOpts (listenOptions port) route router onStarted
