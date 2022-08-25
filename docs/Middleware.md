# Writing and Using Middleware in HTTPurple 🪁 

Since HTTPurple 🪁 routers are just pure functions, you can write a middleware by
simply creating a function that takes a router and an `HTTPure.Request`, and
returns an `HTTPure.ResponseM`. You can then simply use function composition to
combine middlewares, and pass your router to your composed middleware to
generate the decorated router!

See [the Middleware example](./Examples/Middleware/Main.purs) to see how you can
build, compose, and consume different types of middleware.

🎉 **New:** 

HTTPurple 🪁 now supports extensible middlewares that allows you to add further data to your request, see [Extensible Middlewares](#extensible-middlewares).

HTTPurple 🪁 now supports Node.js/Express middlewares, see [Node.js Middlewares](#node-middlewares).

## Writing Middleware

A middleware is a function with the signature:

```purescript
(HTTPure.Request -> HTTPure.ResponseM) -> HTTPure.Request -> HTTPure.ResponseM
```

Note that the first argument is just the signature for a router function. So
essentially, your middleware should take a router and return a new router.
That's it! You can do pretty much anything with middlewares. Here are a few
examples of common middleware patterns:

You can write a middleware that wraps all future work in some behavior, like
logging or timing:

```purescript
myMiddleware router request = do
  doSomethingBefore
  response <- router request
  doSomethingAfter
  pure response
```

Or perhaps a middleware that injects something into the response:

```purescript
myMiddleware router request = do
  response <- router request
  HTTPure.response' response.status response.headers $
    response.body <> "\n\nGenerated using my super duper middleware!"
```

You could even write a middleware that handles routing for some specific cases:

```purescript
myMiddleware _ { path: [ "somepath" ] } = HTTPure.ok "Handled by my middleware!"
myMiddleware router request = router request
```

Or even a middleware that conditionally includes another middleware:

```purescript
myMiddleware router = if something then someOtherMiddleware router else router
```

Just make sure your middlewares follow the correct signature, and users will be
able to compose them at will!

Note that because there is nothing fancy happening here, you could always write
higher order functions that don't follow this signature, if it makes sense. For
instance, you could write a function that takes two routers, and selects which
one to use based on some criteria. There is nothing wrong with this, but you
should try to use the middleware signature mentioned above as much as possible
as it will make your middleware easier to consume and compose.

## Consuming Middleware

Consuming middleware easy: simply compose all the middleware you want, and then
pass your router to the composed middleware.  For instance:

```purescript
main = HTTPure.serve port composedRouter $ Console.log "Server is up!"
  where
    composedRouter = middlewareA <<< middlewareB <<< middlewareC $ router
```

Be aware of the ordering of the middleware that you compose--since we used
`<<<`, the middlewares will compose right-to-left. But because middlewares
choose when to apply the router to the request, this is a bit like wrapping the
router in each successive middleware from right to left. So when the router
executes on a request, those middlewares will actually _execute_
left-to-right--or from the outermost wrapper inwards.

In other words, say you have the following HTTPurple 🪁 server:

```purescript
middleware letter router request = do
  EffectClass.liftEffect $ Console.log $ "Starting Middleware " <> letter
  response <- router request
  EffectClass.liftEffect $ Console.log $ "Ending Middleware " <> letter
  pure response

main = HTTPure.serve port composedRouter $ Console.log "Server is up!"
  where
    composedRouter = middleware "A" <<< middleware "B" $ router
```

When this HTTPurple 🪁 server receives a request, the logs will include:

```
Starting Middleware A
Starting Middleware B
...
Ending Middleware B
Ending Middleware A
```

## Extensible Middlewares

The base type for requests, `ExtRequest` is now extensible:
```purescript
type RequestR route ext =
  ( method :: Method
  , path :: Path
  , query :: Query
  , route :: route
  , headers :: RequestHeaders
  , body :: RequestBody
  , httpVersion :: Version
  , url :: String
  | ext
  )
type ExtRequest route ext = { | RequestR route ext }
```
and the old `Request` is just a type alias for an extensible request without any further data:
```purescript
type Request route = { | RequestR route () }
```

This allows us to write middlewares that extend our request with additional information.

E.g. we can write an authenticator middleware that adds user information to the request:
```purescript
authenticator ::
  forall route extIn extOut.
  Nub (RequestR route extOut) (RequestR route extOut) =>
  Union extIn (user :: Maybe String) extOut =>
  Middleware route extIn extOut
authenticator router request@{ headers } = case Headers.lookup headers "X-Token" of
  Just token | token == "123" -> router $ merge request { user: Just "John Doe" }
  _ -> router $ merge request { user: Nothing :: Maybe String }
```
The type `Middleware` is defined as 
```purescript
type MiddlewareM m route extIn extOut = (ExtRequest route extOut -> m Response) -> ExtRequest route extIn -> m Response
type Middleware route extIn extOut = MiddlewareM Aff route extIn extOut
```
and adds the `extOut` extension to our request handler. In our case, it adds `(user :: Maybe String)` to the request handler. `extIn` defines the input extension that the middleware receives. At the root level, this will be an empty row `()` (unless we use a node middleware).
Not fixing `extIn` to `()` however allows us to stack our middlewars.

Let's add another middleware, that adds the request time to the request:
```purescript
requestTime ::
  forall route extIn extOut.
  Nub (RequestR route extOut) (RequestR route extOut) =>
  Union extIn (time :: JSDate) extOut =>
  Middleware route extIn extOut
requestTime router request = do
  time <- liftEffect JSDate.now
  router $ merge request { time }
```
Similar to the `authenticator` middleware, we add a new field `time` to the request.

We can now compose our middleware stack:
```purescript
middlewareStack :: forall route. (ExtRequest route (user :: Maybe String, time :: JSDate) -> ResponseM) -> Request route -> ResponseM
middlewareStack = authenticator <<< requestTime
```

We can now use our middleware stack. Let's define a simple route:
```purescript
data SayHello = SayHello

derive instance Generic SayHello _

sayHelloRoute :: RD.RouteDuplex' SayHello
sayHelloRoute = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }
```

and a router that makes use of our newly added information:
```purescript
-- | Say 'hello <USER>' when run with X-Token, otherwise 'hello anonymous'
sayHello :: ExtRequest SayHello (user :: Maybe String, time :: JSDate) -> ResponseM
sayHello { user: Just user, time } = ok $ "hello " <> user <> ", it is " <> JSDate.toDateString time <> " " <> JSDate.toTimeString time
sayHello { user: Nothing, time } = ok $ "hello " <> "anonymous, it is " <> JSDate.toDateString time <> " " <> JSDate.toTimeString time
```
As you can see, we are now using an `ExtRequest` with the additional information `(user :: Maybe String, time :: JSDate)`, which we can use in our function body.

Finally, we wrap our `sayHello` router with our `middlewareStack`:

```purescript
main =
  serve { hostname: "localhost", port: 8080 } { route: sayHelloRoute, router: middlewareStack sayHello }
```

See the full example in the [`Examples/ExtensibleMiddleware`](./Examples/ExtensibleMiddleware/) folder.


## Node Middlewares

Node/Express middlewares are no supported, but currently only on the application level (i.e. they will be run on every request).

For our example, we'll use two existing node middlewares, and one custom node middleware.
The first one is `morgan`, which adds logging to our http server.
Install it using 
```bash
npm install morgan --save
```

To use it, we create a FFI and export it as `morgan`
```javascript
import { default as M } from "morgan";
export const morgan = M("tiny")
```

Now we can define the foreign import in our Purescript file:
```purescript
foreign import morgan :: NodeMiddleware ()
```
The empty row `()` indicates, that this middleware doesn't add any information to our request.

Next, we'll add `helmet` which adds some security headers to our response.

```bash
npm install helmet --save
```

Add it to our ffi
```javascript
import { default as H } from "helmet";
export const helmet = H()
```
and define the foreign import
```purescript
foreign import helmet :: NodeMiddleware ()
```

Finally, let's define a small node middleware that adds something to our request. Add a simple authenticating middleware to our ffi:

```javascript
export const authenticator = function (req, res, next) {
  if(req.headers["x-token"] == "123") {
    req.user = "John Doe" 
  } else {
    req.user = null
  }
  next();
};
```
If the middleware receives the http header `x-token` with value `123`, it will add the user, otherwise `null`.

Our foreign import now looks like this:
```purescript
type AuthenticatorR = (user :: Nullable String)

foreign import authenticator :: NodeMiddleware (user :: Nullable String)
```
Our node middleware extends requests with `(user :: Nullable String)`.

We can now compose our middlewares into a single `NodeMiddlewareStack`:
```purescript
nodeMiddleware ∷ NodeMiddlewareStack () AuthenticatorR
nodeMiddleware = NodeMiddlewareStack $ usingMiddleware morgan >=> usingMiddleware helmet >=> usingMiddleware authenticator
```

Let's define a simple route with a handler, that makes use of our node middleware:
```purescript
data Route = Hello

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Hello": noArgs
  }

main :: ServerM
main =
  serveNodeMiddleware { port: 8080 } { route, router: router, nodeMiddleware }
  where
  router { route: Hello, user } = case Nullable.toMaybe user of
    Just u -> ok $ "hello user " <> u
    Nothing -> ok $ "hello anonymous"
```
Note, that we have to use `serveNodeMiddleware` instead of `serve` and pass the `nodeMiddleware` along the `route` and `router`. In the router, we gain access to the nullable user. 

You can see a full example of node middlewares in the [`Examples/NodeMiddleware`](./Examples/NodeMiddleware/) folder.
