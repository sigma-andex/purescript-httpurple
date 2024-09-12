module Examples.ExtensibleMiddleware.Main where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (ExtRequest, Middleware, Request, RequestR, Response, ServerM, ok, serve)
import HTTPurple as Headers
import Prim.Row (class Nub, class Union)
import Record (merge)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

authenticator ::
  forall route extIn extOut.
  Nub (RequestR route extOut) (RequestR route extOut) =>
  Union extIn (user :: Maybe String) extOut =>
  Middleware route extIn extOut
authenticator router request@{ headers } = case Headers.lookup headers "X-Token" of
  Just token | token == "123" -> router $ merge request { user: Just "John Doe" }
  _ -> router $ merge request { user: Nothing :: Maybe String }

requestTime ::
  forall route extIn extOut.
  Nub (RequestR route extOut) (RequestR route extOut) =>
  Union extIn (time :: JSDate) extOut =>
  Middleware route extIn extOut
requestTime router request = do
  time <- liftEffect JSDate.now
  router $ merge request { time }

data SayHello = SayHello

derive instance Generic SayHello _

sayHelloRoute :: RD.RouteDuplex' SayHello
sayHelloRoute = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

-- | Say 'hello <USER>' when run with X-Token, otherwise 'hello anonymous'
sayHello :: ExtRequest SayHello (user :: Maybe String, time :: JSDate) -> Aff Response
sayHello { user: Just user, time } = ok $ "hello " <> user <> ", it is " <> JSDate.toDateString time <> " " <> JSDate.toTimeString time
sayHello { user: Nothing, time } = ok $ "hello " <> "anonymous, it is " <> JSDate.toDateString time <> " " <> JSDate.toTimeString time

-- | The stack of middlewares to use for the server
middlewareStack :: forall route. (ExtRequest route (user :: Maybe String, time :: JSDate) -> Aff Response) -> Request route -> Aff Response
middlewareStack = authenticator <<< requestTime

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route: sayHelloRoute, router: middlewareStack sayHello }
  where
  onStarted = do
    log " ┌───────────────────────────────────────────────┐"
    log " │ Server now up on port 8080                    │"
    log " │                                               │"
    log " │ To test, run:                                 │"
    log " │  > http -v GET localhost:8080 X-Token:123     │"
    log " │    # => hello John Doe, it is ...             │"
    log " └───────────────────────────────────────────────┘"
