module Examples.BinaryResponse.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Console (log)
import HTTPurple (Headers, Request, ResponseM, ServerM, header, ok', serve)
import Node.FS.Aff (readFile)
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
filePath = "./docs/Examples/BinaryResponse/circle.png"

responseHeaders :: Headers
responseHeaders = header "Content-Type" "image/png"

-- | Respond with image data when run
router :: Request Route -> ResponseM
router = const $ readFile filePath >>= ok' responseHeaders

-- | Boot up the server
main :: ServerM
main =
  serve 8080 { route, router, notFoundHandler: Nothing } do
    log " ┌──────────────────────────────────────┐"
    log " │ Server now up on port 8080           │"
    log " │                                      │"
    log " │ To test, run:                        │"
    log " │  > curl -o circle.png localhost:8080 │"
    log " └──────────────────────────────────────┘"
