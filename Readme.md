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

import Prelude

import Effect.Console (log)
import HTTPurple (ServerM, serve, ok)

main :: ServerM
main = serve 8080 router $ log "Server now up on port 8080"
  where
    router _ = ok "hello world!"
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
