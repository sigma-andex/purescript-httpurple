module Examples.Middleware.Main where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (type (<+>), Request, Response, ServerM, fullPath, header, ok, ok', serve, (<+>))
import Record as Record
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG
import Routing.Duplex.Generic.Syntax ((/))
import Type.Prelude (Proxy(..))

data Middleware = Middleware

derive instance Generic Middleware _

middlewareRoute :: RD.RouteDuplex' Middleware
middlewareRoute = RD.root $ RG.sum
  { "Middleware": "middleware" / RG.noArgs
  }

data SayHello = SayHello

derive instance Generic SayHello _

sayHelloRoute :: RD.RouteDuplex' SayHello
sayHelloRoute = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

-- | A middleware that logs at the beginning and end of each request
loggingMiddleware ::
  forall route.
  (Request route -> Aff Response) ->
  Request route ->
  Aff Response
loggingMiddleware router request = do
  liftEffect $ log $ "Request starting for " <> path
  response <- router request
  liftEffect $ log $ "Request ending for " <> path
  pure response
  where
  path = fullPath request

-- | A middleware that adds the X-Middleware header to the response, if it
-- | wasn't already in the response
headerMiddleware ::
  forall route.
  (Request route -> Aff Response) ->
  Request route ->
  Aff Response
headerMiddleware router request = do
  response@{ headers } <- router request
  pure $ response { headers = header' <> headers }
  where
  header' = header "X-Middleware" "middleware"

-- | A middleware that sends the body "Middleware!" instead of running the
-- | router when requesting /middleware
pathMiddleware ::
  forall route.
  (Request route -> Aff Response) ->
  Request (Middleware <+> route) ->
  Aff Response
pathMiddleware _ { route: Left Middleware } = ok "Middleware!"
pathMiddleware router request@{ route: Right r } = router $ Record.set (Proxy :: _ "route") r request

-- | Say 'hello' when run, and add a default value to the X-Middleware header
sayHello :: Request SayHello -> Aff Response
sayHello _ = ok' (header "X-Middleware" "router") "hello"

-- | The stack of middlewares to use for the server
middlewareStack :: forall route. (Request route -> Aff Response) -> Request (Either Middleware route) -> Aff Response
middlewareStack = loggingMiddleware <<< headerMiddleware <<< pathMiddleware

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route: middlewareRoute <+> sayHelloRoute, router: middlewareStack sayHello }
  where
  onStarted = do
    log " ┌───────────────────────────────────────┐"
    log " │ Server now up on port 8080            │"
    log " │                                       │"
    log " │ To test, run:                         │"
    log " │  > curl -v localhost:8080             │"
    log " │    # => ...                           │"
    log " │    # => ...< X-Middleware: router     │"
    log " │    # => ...                           │"
    log " │    # => hello                         │"
    log " │  > curl -v localhost:8080/middleware  │"
    log " │    # => ...                           │"
    log " │    # => ...< X-Middleware: middleware │"
    log " │    # => ...                           │"
    log " │    # => Middleware!                   │"
    log " └───────────────────────────────────────┘"
