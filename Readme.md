# HTTPurple 🪁

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/sigma-andex/purescript-httpurple/main/License)
[![Latest release](http://img.shields.io/github/release/sigma-andex/purescript-httpurple.svg)](https://github.com/sigma-andex/purescript-httpurple/releases)
[![purescript-httpure on Pursuit](https://pursuit.purescript.org/packages/purescript-httpure/badge)](https://pursuit.purescript.org/packages/purescript-httpure)

A 🎨 colourful fork of the amazing [HTTPure](https://github.com/citizennet/purescript-httpure) http server framework.


## Installation

```bash
spago install httpure
```

## Quick Start

```purescript
module Main where

import Prelude

import Effect.Console (log)
import HTTPure (ServerM, serve, ok)

main :: ServerM
main = serve 8080 router $ log "Server now up on port 8080"
  where
    router _ = ok "hello world!"
```

## Documentation

Module documentation is published
on [Pursuit](http://pursuit.purescript.org/packages/purescript-httpure).

You can also take a look at [our guides](./docs).

## Examples

HTTPure ships with a number of [examples](./docs/Examples). To run an example,
in the project root, run:

Or, without `nix`:

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

[MIT](./License)
