# HTTPurple 🪁

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/sigma-andex/purescript-httpurple/main/License)

A 🎨 colourful fork of the amazing [HTTPure](https://github.com/citizennet/purescript-httpure) http server framework.


## Installation

```bash
spago install httpurple
```

## Quick Start

```purescript
module Main where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import HTTPurple (ServerM, ok, serve)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route = Hello String

derive instance Generic Route _

route :: RouteDuplex' Route
route = root $ sum
  { "Hello": "hello" / segment
  }

main :: ServerM
main =
  serve { port: 8080 } { route, router }
  where
  router { route: Hello name } = ok $ "hello " <> name
```

then start the server

```bash
➜ spago run
           Src   Lib   All
Warnings   0     0     0  
Errors     0     0     0  
[info] Build succeeded.
HTTPurple 🪁 up and running on http://0.0.0.0:8080
```

query your server, e.g. using [httpie](https://httpie.io/)

```bash
➜ http http://localhost:8080/hello/🗺  
HTTP/1.1 200 OK
Connection: keep-alive
Content-Length: 10
Date: Sun, 22 May 2022 16:50:52 GMT
Keep-Alive: timeout=5

hello 🗺
```

## Documentation

See the [docs folder](./docs).

## Examples

HTTPurple ships with a number of [examples](./docs/Examples). To run an example,
in the project root, run:

```bash
spago -x test.dhall run --main Examples.<Example Name>.Main
```

Each example's startup banner will include information on routes available on
the example server.

## Testing

To run the test suite, in the project root run:

```bash
spago -x test.dhall test
```

## License

This is a fork of [HTTPure](https://github.com/citizennet/purescript-httpure), which is licensed under MIT. See the [original license](./LICENSES/httpure.LICENSE). This work is similarly licensed under [MIT](./License).
