# HTTPurple 🪁

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/sigma-andex/purescript-httpurple/main/License)

A 🎨 colourful fork of the amazing [HTTPure](https://github.com/citizennet/purescript-httpure) http server framework.

Coming from HTTPure? You might want to have a look at the [differences to HTTPure](#differences-to-httpure).

## ToC
1. [Installation](#installation)
1. [Quick start](#quick-start)
1. [Documenation](#documentation)
1. [Examples](#examples)
1. [Testing](#testing)
1. [Differences to HTTPure](#differences-to-httpure)
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

See the [docs folder](./docs) for more in-depth guides.

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

## Differences to HTTPure 

HTTPurple 🪁 is a fork of [HTTPure](https://github.com/citizennet/purescript-httpure) that I started to freely experiment with some ideas I have on improving the usage experience. Currently I have no intentions on back-porting any of it to HTTPure, as I don't have the time for it and also don't want to restrict myself. 

If you have used HTTPure before, you'll probably want to go through the following changes to get started using HTTPurple 🪁:
* [routing-duplex](#routing-duplex)
* [startup options](#startup-options)
* [other improvements](#other-improvmenets)
* [hot module reloading](#hot-module-reloading)

### Routing-duplex

The most notable difference to HTTPure is that HTTPurple 🪁 uses the amazing [`routing-duplex`](https://github.com/natefaubion/purescript-routing-duplex) library for routing. I found the previous lookup-based routing tedious to work with, especially when having more complex routes, and quite error-prone, especially if you need reverse-routing for redirects.

[`routing-duplex`](https://github.com/natefaubion/purescript-routing-duplex) offers an elegant bidirectional routing which was initially designed for SPAs. Have a look at the really extensive [`documentation`](https://github.com/natefaubion/purescript-routing-duplex). The benefits of using routing-duplex are
* Much simpler and less tedious definition of routes
* Roundtrip printing/parsing of routes, so no more invalid redirects
* Exhaustive pattern matching so you are sure to match all defined routes
* Option to separate routes into logical groups

Here is a bit more elaborated examples:

```purescript
module Main where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import HTTPurple

data Route
  = Home
  | Profile String
  | Account String
  | Search { q :: String, sorting :: Maybe Sort }
derive instance Generic Route _

data Sort = Asc | Desc
derive instance Generic Sort _

sortToString :: Sort -> String
sortToString = case _ of
  Asc -> "asc"
  Desc -> "desc"

sortFromString :: String -> Either String Sort
sortFromString = case _ of
  "asc" -> Right Asc
  "desc" -> Right Desc
  val -> Left $ "Not a sort: " <> val

sort :: RouteDuplex' String -> RouteDuplex' Sort
sort = as sortToString sortFromString

api :: RouteDuplex' Route
api = root $ sum
  { "Home": noArgs
  , "Profile": "profile" / string segment
  , "Account": "account" / string segment
  , "Search": "search" ? { q: string, sorting: optional <<< sort }
  }

main :: ServerM
main = serve { port: 8080 } { route: api, router: apiRouter }
  where

  apiRouter { route: Home } = ok "hello world!"
  apiRouter { route: Profile profile } = ok $ "hello " <> profile <> "!"
  apiRouter { route: Account account } = found' redirect ""
    where
    reverseRoute = print api $ Profile account
    redirect = headers [ Tuple "Location" reverseRoute ]
  apiRouter { route: Search { q, sorting } } = ok $ "searching for query " <> q <> " " <> case sorting of
    Just Asc -> "ascending"
    Just Desc -> "descending"
    Nothing -> "defaulting to ascending"
```

### Startup options

HTTPurple 🪁 greatly simplifies the startup options and functions. The `serve`, `serve'`, `serveSecure` and `serveSecure'` have been merged into a single function `serve` that accepts listen options as the first parameter and uses sane defaults if you don't provide any.

The easiest way to start a server is to provide just the route and a router: 

```purescript
main :: ServerM
main =
  serve {} { route, router }
```

This will spin up the http server with sane defaults.
```bash
HTTPurple 🪁 up and running on http://0.0.0.0:8080
```

But you can overwrite any of the optional properties like this

```purescript
main :: ServerM
main =
  serve {
    hostname: "localhost"
  , port: 9000
  , certFile: "./Certificate.cer"
  , keyFile: "./Key.key"
  , notFoundHandler
  , onStarted: log "Server started 🚀"
  , closingHandler: NoClosingHandler
  } { route, router }
  where
  notFoundHandler :: Request Unit -> ResponseM
  notFoundHandler = const $ ok "Nothing to see here"
```

### Other improvmenets

* Default closing handler - A default closing handler is provided so you can just stop your server using `ctrl+x` without having to worry about anything. You can deactivate it by setting `closingHandler: NoClosingHandler` in the listen options.

### Hot module reloading

With HTTPurple 🪁 you can easily set up a hot module reloading workflow:

Create an `index.js` with the content:
```javascript
import * as Main from './output/Main/index.js'
Main.main()
```

Add to `package.json`:
```json
  ...
  "scripts": {
      "hot": "spago build -w & nodemon \"node index.js\""
    },
  "type": "module",
  ...
```

Spin up:
```bash
npm run hot
```
Develop:
```bash
HTTPurple 🪁 up and running on http://0.0.0.0:8080
[nodemon] restarting due to changes...
[nodemon] restarting due to changes...
[nodemon] starting `node "node index.js" index.js`
HTTPurple 🪁 up and running on http://0.0.0.0:8080
[nodemon] restarting due to changes...
[nodemon] restarting due to changes...
[nodemon] starting `node "node index.js" index.js`
HTTPurple 🪁 up and running on http://0.0.0.0:8080
```

## License

This is a fork of [HTTPure](https://github.com/citizennet/purescript-httpure), which is licensed under MIT. See the [original license](./LICENSES/httpure.LICENSE). This work is similarly licensed under [MIT](./License).
