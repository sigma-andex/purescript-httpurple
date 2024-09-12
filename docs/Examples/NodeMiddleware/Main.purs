module Examples.NodeMiddleware.Main where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Aff (Aff)
import Effect.Console (log)
import HTTPurple (ExtRequest, NodeMiddleware, NodeMiddlewareStack(..), Response, ServerM, ok, serveNodeMiddleware, usingMiddleware)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

foreign import logger :: NodeMiddleware ()

type AuthenticatorR = (user :: Nullable String)

foreign import authenticator :: NodeMiddleware (user :: Nullable String)

nodeMiddleware :: NodeMiddlewareStack () AuthenticatorR
nodeMiddleware = NodeMiddlewareStack $ usingMiddleware logger >=> usingMiddleware authenticator

data SayHello = SayHello

derive instance Generic SayHello _

sayHelloRoute :: RD.RouteDuplex' SayHello
sayHelloRoute = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

-- | Say 'hello <USER>' when run with X-Token, otherwise 'hello anonymous'
sayHello :: ExtRequest SayHello AuthenticatorR -> Aff Response
sayHello { user } = case Nullable.toMaybe user of
  Just u -> ok $ "hello " <> u
  Nothing -> ok $ "hello " <> "anonymous"

-- | Boot up the server
main :: ServerM
main =
  serveNodeMiddleware { hostname: "localhost", port: 8080, onStarted } { route: sayHelloRoute, router: sayHello, nodeMiddleware }
  where
  onStarted = do
    log " ┌───────────────────────────────────────────────┐"
    log " │ Server now up on port 8080                    │"
    log " │                                               │"
    log " │ To test, run:                                 │"
    log " │  > http -v GET localhost:8080 X-Token:123     │"
    log " │    # => hello John Doe                        │"
    log " └───────────────────────────────────────────────┘"
