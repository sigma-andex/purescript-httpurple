# Routing in HTTPurple 🪁

## Table of contents
* [Routing introduction](#introduction)
* [The request record](#the-request-record)
* [Matching HTTP methods](#matching-http-methods)
* [Matching paths and query parameters](#matching-paths-and-query-parameters)
  * [Adding further types](#adding-further-types)
  * [Composing routes](#composing-routes)
  * [Reverse routing](#reverse-routing)
* [Working with request headers](#working-with-request-headers)

## Routing introduction

HTTPurple 🪁 uses [`routing-duplex`](https://github.com/natefaubion/purescript-routing-duplex) for routing.

You'll need two things:
1. A data type representing your routes
2. A mapping between the data constructors of your data type and the paths

Here an example:
```purescript
import HTTPurple

-- We define a data type representing our route
-- In this case, a single route `SayHello` that accepts one parameter
data Route = SayHello String
derive instance Generic Route _ -- a generic instance is needed

-- The mapping between our data constructor `SayHello` and 
-- the route /hello/<argument>
route :: RouteDuplex' Route
route = mkRoute
  { "SayHello": "hello" / string segment
  }

-- We then start the http server passing the route and a handler function (router)
main :: ServerM
main = serve {} { route, router }
  where
  router { route: SayHello name } = ok $ "hello " <> name <> "!"
```

The router function is called for each inbound request to the HTTPure server.
Its signature is:

```purescript
forall route. HTTPurple.Request route -> HTTPurple.ResponseM
```

So in HTTPurple, routing is handled simply by the router being a pure function
which is passed a value that contains all information about the current request,
and which returns a response monad. There's no fancy path parsing and matching
algorithm to learn, and everything is pure--you don't get anything or set
anything, you simply define the return value given the input parameters, like
any other pure function.

This is quite powerful, as all routing can be defined using the same PureScript
pattern matching and guard syntax you use everywhere else. It allows you to
break up your router to sub-routers easily, using whatever router grouping makes
sense for your app. It also leads to some powerful patterns for defining and
using middleware. For more details about defining and using middleware, see the
[Middleware guide](./Middleware.md).

For more details about the response monad, see the [Responses
guide](./Responses.md).

## The Request Record

The `HTTPurple.Request route` type is the input parameter for the router function. It is
a `Record` type that contains the following fields:

- `method` - A member of `HTTPurple.Method`.
- `route` - A data type representing your route with paths and query parameters.
- `headers` - A `HTTPurple.Headers` object. The `HTTPurple.Headers` newtype wraps
  the `Object String` type and provides some typeclass instances that make more
  sense when working with HTTP headers.
- `body` - A `String` containing the contents of the request body, or an empty
  `String` if none was provided.

Following are some more details on working with specific fields, but remember,
you can combine guards and pattern matching for any or all of these fields
however it makes sense for your use case.

## Matching paths and query parameters

Let's have a look at a bit more complex routing scenario. 
Imagine we are developing the backend service for a simple web shop.We want two define three routes:
- `/` which returns the data of the start page
- `/categories/<category>/products/<product>` which takes two path parameters category name and product name and returns a certain product
- `/search?q=<query>&sorting=<asc|desc>` which takes two query parameters, a search string and an optional sorting argument

```purescript
-- We define three data types representing the three routes
data Route
  = Home
  | Products String String -- the product route with two path parameters
  | Search { q :: String, sorting :: Maybe String } -- the search route with two query parameters, whereby sorting is optional
derive instance Generic Route _


-- Next we define the route (mapping)
route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs -- the root route /
  , "Products": "categories" / string segment / "products" / string segment
  , "Search": "search" ? { q: string, sorting: optional <<< string }
  }

-- Finally, we pass the route (mapping) to the server and also define a route handler
main :: ServerM
main = serve { port: 8080 } { route: route, router }
  where
  router { route: Home } = ok "home"
  router { route: Products category product } = do
    ok $ "category=" <> category <> ", product=" <> product
  router { route: Search { q, sorting } } = ok $ "searching=" <> q <> "," <> case sorting of
    Just "asc" -> "ascending"
    _ -> "descending"
```

As you can see, in the route handler you can directly pattern match your data type. Pattern matching of the route is exhaustive so that you will get an error if you miss a route.

### Adding further types

We can add further type information to our route data type.
Instead of treating the path arguments of our product route as `String`, we can define newtypes for the arguments:
```purescript
newtype Category = Category String
instance Newtype Category String

newtype Product = Product String
instance Newtype Product String
```

We'll change our data constructor to accept the two newtypes:
```purescript
  | Products Category Product
```

We can then define new segment types to match the different arguments:

```purescript
category :: RouteDuplex' Category
category = _Newtype R.segment

product :: RouteDuplex' Product
product = _Newtype segment
```

Now we can update our route mapping:
```purescript
"Products": "categories" / category / "products" / product
```

We can furthe add a more expressive type for our sorting query parameter

```purescript
data Sort = Asc | Desc
derive instance Generic Sort _
```

We can then define the conversion to and from string and define a new `RouteDuplex`:
```purescript
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
```

We can then update our `Route` data type to use the new `Sort` data type
```purescript
  | Search { q :: String, sorting :: Maybe Sort }
```
and use the new `sort` function in our route mapping: 
```purescript
"Search": "search" ? { q: string, sorting: optional <<< sort }
```

As you can see, the `RoutingDuplex` approach is quite powerful and once you got a grip on it, it is also rather straight-forward to use.
I recommend you to also check out the [`routing-duplex`  documentation](https://github.com/natefaubion/purescript-routing-duplex) which contains many more examples.

### Composing routes

Sometimes you might want to define two route data types to structure your routing logically. Composing routes is straight-forward with HTTPurple. 
E.g. you could have an `ApiRoute` representing your business api and a `MetaRoute` for technical routes, such as a health check:

```purescript
data ApiRoute
  = Home
derive instance Generic Route _
api :: RouteDuplex' ApiRoute
api = mkRoute
  { "Home": G.noArgs }

data MetaRoute = Health

derive instance Generic MetaRoute _

meta :: RouteDuplex' MetaRoute
meta = mkRoute { "Health": "health" / G.noArgs }
```

You can compose these two routes using the `<+>` operator, and compose the routing handlers with `orElse`:

```purescript
main :: ServerM
main = serve { port: 8080, notFoundHandler } { route: api <+> meta, router: apiRouter `orElse` metaRouter }
  where

  apiRouter { route: Home } = ok "hello world!"
  metaRouter { route: Health } = ok """{"status":"ok"}"""
  ```

### Reverse routing

Reverse routing, e.g. for redirects or HATEOAS, is straight-forward using the `print` function from `routing-duplex`:

```purescript
data Route
  = Old
  | New

route :: RouteDuplex' Route
route = mkRoute
  { "Old": "old" / RG.noArgs
  , "New": "new" / RG.noArgs
  }

main :: ServerM
main = serve { port: 8080 } { route, router }
  where
  router { route: Old } = found' redirect ""
    where
    redirect = headers [ Tuple "Location" $ print route $ New account ]
```


## Matching HTTP Methods

You can use normal pattern matching to route based on the HTTP method:

```purescript
router { method: HTTPurple.Post } = HTTPurple.ok "received a post"
router { method: HTTPurple.Get } = HTTPurple.ok "received a get"
router { method } = HTTPurple.ok $ "received a " <> show method
```

To see the list of methods that HTTPure understands, see the
[Method](../src/HTTPure/Method.purs) module. To see an example server that
routes based on the HTTP method, see [the Post
example](./Examples/Post/Main.purs).

## Working With Request Headers

Headers are again very similar to working with path segments or query
parameters:

```purescript
router { headers }
  | headers !? "X-Foo" = HTTPurple.ok "There is an 'X-Foo' header"
  | headers !@ "X-Foo" == "bar" = HTTPurple.ok "The header 'X-Foo' is 'bar'"
  | otherwise = HTTPurple.ok $ "The value of 'X-Foo' is " <> headers !@ "x-foo"
```

Note that using the `HTTPurple.Lookup` typeclass on headers is case-insensitive.

To see an example server that works with headers, see [the Headers
example](./Examples/Headers/Main.purs).
