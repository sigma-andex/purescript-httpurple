# Creating HTTPure Responses

## The Response Monad

The `HTTPure.ResponseM` monad is the return type of the router function. It is
an `Aff` type that contains an `HTTPure.Response`. Because it is an `Aff`, it
transparent to add asynchronous behavior when you need.

To see an example server taking advantage of asynchronous responses, see [the
Async Response example](./Examples/AsyncResponse/Main.purs).

## Response Helpers

HTTPure defines a number of helpers for creating response monads for all
different HTTP response status codes. Some of these helpers take a body, for
instance, `HTTPure.ok` and `HTTPure.partialContent`, and some do not, for
instance `HTTPure.created` or `HTTPure.noContent`. There are prime functions
corresponding to each helper--for instance, `HTTPure.ok'` and
`HTTPure.created'`. The prime versions are the same as the base versions except
they also return response headers--see the [Setting Response
Headers](#setting-response-headers) section below for more details.

For a full list of helpers, see the [Response](../src/HTTPurple/Response.purs)
module.

In some cases, the defined helpers don't cover your needs--for instance, if
HTTPure doesn't have a helper defined for some HTTP response code you care about
(and you don't want to [contribute it](../Contributing.md)--hint hint, you
should contribute it!), or if you need to specify a body where normally one
wouldn't be sent. For these cases, you can use `HTTPure.response`, which takes a
status code and a body. If you need to specify headers, use `HTTPure.response'`.
If you don't need to specify a body, you can use `HTTPure.emptyResponse` or
`HTTPure.emptyResponse'`.

## Raw Responses

The `HTTPure.ResponseM` monad contains a `HTTPure.Response` value, which is a
`Record` type containing the following fields:

- `status` - An `Int` HTTP response status code.
- `headers` - A `HTTPure.Headers` containing the HTTP response headers.
- `body` - A `String` containing the HTTP response body.

You can manually construct a response if you want to:

```purescript
router _ = pure $ { status: 200, headers: HTTPure.headers [], body: "foo" }
```

This can be useful in some circumstances, but in the vast majority of cases it's
recommended that you use the response helpers described above -- they are more
explicit and allow you to avoid using magic numbers in your code for HTTP status
codes.

## Setting Response Headers

If you need to return response headers, you can do so using the prime versions
of the response helpers. These functions take an `HTTPure.ResponseHeaders` object. You
can construct an `HTTPure.ResponseHeaders` in a few ways:

- `HTTPure.empty` - Construct an empty `HTTPure.ResponseHeaders`
- `HTTPure.header` - Given a string with a header name and a string with a
  value, construct a singleton `HTTPure.ResponseHeaders`. For instance:

```purescript
headers = HTTPure.header "Content-Type" "application/json"
```

- `HTTPure.headers` - Construct a `HTTPure.Headers` from a record:

```purescript
headers = HTTPure.headers {
  "Content-Type": "application/json",
  "Set-Cookie": ["cookie-value-1", "cookie-value-2"], -- note: you can set multiple headers by using an Array
  "X-My-CustomHeader": "some-value"
}
```

Because `HTTPure.ResponseHeaders` has an instance of `Semigroup`, you can also append
`HTTPure.ResponseHeaders` objects:

```purescript
headers =
  HTTPure.header "X-Header-A" "valueA" <> HTTPure.header "X-Header-B" "valueB"
```

To see an example server that sets response headers, see [the Headers
example](./Examples/Headers/Main.purs).
