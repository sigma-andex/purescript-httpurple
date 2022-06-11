# HTTPurple Basics

This guide is a brief overview of the basics of creating a HTTPurple server.

## TOC

1. [Creating a Server](#creating-a-server)
1. [Hot module reloading](#hot-module-reloading)
1. [Further server settings](#further-server-settings)

## Creating a Server

To create a server, use `HTTPurple.serve`.
Both of these functions take a port number, a router function, and an `Effect`
that will run once the server has booted. The signature of the router function
is:

```purescript
HTTPurple.Request route -> HTTPurple.ResponseM
```

For more details on routing, see the [Routing guide](./Routing.md). For more
details on responses, see the [Responses guide](./Responses.md). The router can
be composed with middleware; for more details, see the [Middleware
guide](./Middleware.md).

You can create an HTTPurple server using `HTTPurple.serve`:

```purescript
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

`HTTPurple.serve` takes as arguments two records:
1. Server configuration - A record containing all additional settings that you want to pass. See [further server settings](#further-server-settings) for a list of all settings.
1. A record containing your route and a router for these routes. See the [routing guide](./Routing.md) for more information.


## Hot module reloading

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

## Further server settings  

HTTPurple 🪁 defines a series of settings that you can override.

Here is an example of the full list of server settings:

```
{
    hostname: "localhost"
  , port: 9000
  , certFile: "./Certificate.cer"
  , keyFile: "./Key.key"
  , notFoundHandler: custom404Handler
  , onStarted: log "Server started 🚀"
  , closingHandler: NoClosingHandler
  }
```

### SSL

**Note**: SSL is usually something that you want to handle at the infrastructure level and not within the application's http server. The SSL support is mainly here because HTTPure had it, but I might remove it in the near future if it hinders development.

You can create an SSL-enabled HTTPurple server using `HTTPurple.serve` by passing a certFile, a keyFile and an optionally a different port:
```purescript
main :: HTTPurple.ServerM
main =
  HTTPurple.serve { port: 443, certFile : "./Certificate.cer", keyFile:  "./Key.key" } { route, router }
  ...
```

You can look at [the SSL Example](./Examples/SSL/Main.purs), which uses this
method to create the server.


### Closing handler

HTTPurple 🪁 comes with a default closing handler, so `Ctrl+x` just stops the server. 
you can switch off this behaviour by passing 
```purescript
{ closingHandler: NoClosingHandler }
```
to `serve` and define your own closing handler:


```purescript
import Prelude

import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Effect (Effect)
import Effect.Console (log)
import HTTPurple (serve, ok)
import Node.Process (onSignal)

main :: Effect Unit
main = do 
  closingHandler <- serve 8080 { route, router }
  -- do something with closingHandler
```
