module Examples.BinaryResponse.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import HTTPurple (Request, ResponseHeaders, Response, ServerM, header, ok', serve)
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

responseHeaders :: ResponseHeaders
responseHeaders = header "Content-Type" "image/png"

-- | Respond with image data when run
router :: Request Route -> Aff Response
router = const $ readFile filePath >>= ok' responseHeaders

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  onStarted = do
    log " ┌──────────────────────────────────────┐"
    log " │ Server now up on port 8080           │"
    log " │                                      │"
    log " │ To test, run:                        │"
    log " │  > curl -o circle.png localhost:8080 │"
    log " └──────────────────────────────────────┘"
