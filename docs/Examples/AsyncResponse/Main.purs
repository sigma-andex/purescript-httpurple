module Examples.AsyncResponse.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Console (log)
import HTTPure (Request, ResponseM, ServerM, ok, serve)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

data Route = SayHello

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

-- | The path to the file containing the response to send
filePath :: String
filePath = "./docs/Examples/AsyncResponse/Hello"

router :: Request Route -> ResponseM
router { route: SayHello } = readTextFile UTF8 filePath >>= ok

-- | Boot up the server
main :: ServerM
main =
  serve 8080 { route, router, notFoundHandler: Nothing } do
    log " ┌────────────────────────────────────────────┐"
    log " │ Server now up on port 8080                 │"
    log " │                                            │"
    log " │ To test, run:                              │"
    log " │  > curl localhost:8080   # => hello world! │"
    log " └────────────────────────────────────────────┘"
