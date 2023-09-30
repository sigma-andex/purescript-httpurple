module Examples.CustomStack.Main where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks, runReaderT)
import Data.Generic.Rep (class Generic)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import HTTPurple (Request, Response, ServerM, ok, serve')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

data Route = SayHello

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

-- | A type to hold the environment for our ReaderT
type Env = { name :: String }

-- | Say 'hello, joe' when run
sayHello :: forall m. MonadAff m => MonadAsk Env m => Request Route -> m Response
sayHello _ = do
  name <- asks _.name
  ok $ "hello, " <> name

-- | Boot up the server
main :: ServerM
main =
  serve' (\a -> runReaderT a {name: "joe"}) { hostname: "localhost", port: 8080, onStarted } { route, router: sayHello }
  where
  onStarted = do
    log " ┌───────────────────────────────────────┐"
    log " │ Server now up on port 8080            │"
    log " │                                       │"
    log " │ To test, run:                         │"
    log " │  > curl -v localhost:8080             │"
    log " │    # => hello, joe                    │"
    log " └───────────────────────────────────────┘"
