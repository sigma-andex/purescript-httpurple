module Examples.Chunked.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (Request, ResponseM, ServerM, ok, serve)
import Node.ChildProcess (defaultSpawnOptions, spawn, stdout)
import Node.Stream (Readable)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

data Route = SayHello

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

-- | Run a script and return it's stdout stream
runScript :: String -> Aff (Readable ())
runScript script =
  liftEffect $ stdout <$> spawn "sh" [ "-c", script ] defaultSpawnOptions

-- | Say 'hello world!' in chunks when run
router :: Request Route -> ResponseM
router = const $ runScript "echo 'hello '; sleep 1; echo 'world!'" >>= ok

-- | Boot up the server
main :: ServerM
main =
  serve { port: 8080, onStarted } { route, router }
  where
  onStarted = do
    log " ┌──────────────────────────────────────┐"
    log " │ Server now up on port 8080           │"
    log " │                                      │"
    log " │ To test, run:                        │"
    log " │  > curl -Nv localhost:8080           │"
    log " │    # => ...                          │"
    log " │    # => < Transfer-Encoding: chunked │"
    log " │    # => ...                          │"
    log " │    # => hello                        │"
    log " │    (1 second pause)                  │"
    log " │    # => world!                       │"
    log " └──────────────────────────────────────┘"
