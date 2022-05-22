module Examples.CustomStack.Main where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import HTTPure (Request, Response, ResponseM, ServerM, ok, serve)
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

-- | A middleware that introduces a ReaderT
readerMiddleware ::
  forall route.
  (Request route -> ReaderT Env Aff Response) ->
  Request route ->
  ResponseM
readerMiddleware router request = do
  runReaderT (router request) { name: "joe" }

-- | Say 'hello, joe' when run
sayHello :: forall m. MonadAff m => MonadAsk Env m => Request Route -> m Response
sayHello _ = do
  name <- asks _.name
  ok $ "hello, " <> name

-- | Boot up the server
main :: ServerM
main =
  serve 8080 { route, router: readerMiddleware sayHello, notFoundHandler: Nothing } do
    log " ┌───────────────────────────────────────┐"
    log " │ Server now up on port 8080            │"
    log " │                                       │"
    log " │ To test, run:                         │"
    log " │  > curl -v localhost:8080             │"
    log " │    # => hello, joe                    │"
    log " └───────────────────────────────────────┘"
