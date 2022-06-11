# HTTPurple 🪁

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/sigma-andex/purescript-httpurple/main/License)

A functional http framework with a focus on type-safety and simplicity. 

This project was originally forked from the amazing [HTTPure](https://github.com/citizennet/purescript-httpure) http server framework. If you are coming from HTTPure you might want to have a look at the [differences to HTTPure](./docs/Differences.md).

## ToC
1. [Installation](#installation)
1. [Quick start](#quick-start)
1. [Documenation](#documentation)
1. [Examples](#examples)
1. [Testing](#testing)
1. [License](#license)

## Installation

```bash
spago install httpurple
```

## Quick start

```purescript
module Main where

import Prelude hiding ((/))

import HTTPurple

data Route = Hello String
derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
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

See the [docs folder](./docs) for the in-depth guides. 

* [Basics](./docs/Basics.md) - Basic introduction to HTTPurple 🪁
* [Routing](./docs/Routing.md) - Explanation of the routing dsl
* [Requests](./docs/Requests.md) - Guide to request handling
* [Response](./docs/Responses.md) - Guide to response handling
* [Differences](./docs/Differences.md) - A detailed description of the differences to HTTPure

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
