# Static files

HTTPurple 🪁 can serve files from a directory on disk — stylesheets, scripts, images, a hand-written `index.html` — through `serveStatic`. It resolves the request path against a root directory, refuses anything that would land outside it, and answers with the file and a `Content-Type` derived from its extension. `serveStatic`, its options and the `Dotfiles` and `RootError` types are exported from `HTTPurple` like everything else; they live in the `HTTPurple.Static` module.

## TOC
1. [Serving a directory](#serving-a-directory)
1. [A handler, not a middleware](#a-handler-not-a-middleware)
1. [Options](#options)
1. [What each response means](#what-each-response-means)
1. [Content types](#content-types)
1. [Methods and `HEAD`](#methods-and-head)
1. [What it doesn't do](#what-it-doesnt-do)
1. [Coming from `serve-static`](#coming-from-serve-static)

## Serving a directory

Give the assets a route that captures the rest of the path with `rest`, build the handler once at startup, and call it from the router:

```purescript
data Route = Home | Asset (Array String)

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  , "Asset": "static" / rest
  }

router :: ({ asset :: Array String } -> Aff Response) -> Request Route -> Aff Response
router serveAsset { route: Home } = serveAsset { asset: [ "index.html" ] }
router serveAsset { route: Asset asset } = serveAsset { asset }

main :: ServerM
main = do
  serveAsset <- serveStatic { root: "./public" } >>= either (show >>> throw) pure
  serve { hostname: "localhost", port: 8080 } { route, router: router serveAsset }
```

`serveStatic` runs in `Effect` and returns `Either RootError handler`: it resolves the root to an absolute path and `stat`s it once, so a mistyped root refuses to boot instead of 404ing every asset at runtime. The handler it returns is a plain function from `{ asset :: Array String }` to a response; keep it and reuse it for every request rather than calling `serveStatic` per request.

A request for `/static/style.css` is parsed by `routing-duplex` into `Asset [ "style.css" ]`, and the handler serves `./public/style.css`:

```
> curl -i localhost:8080/static/style.css
HTTP/1.1 200 OK
Content-Length: 52
Content-Type: text/css; charset=utf-8
X-Content-Type-Options: nosniff

body {
  font-family: sans-serif;
  margin: 2rem;
}
```

Nested paths work the same way: `/static/css/site.css` becomes `Asset [ "css", "site.css" ]` and serves `./public/css/site.css`. The full example is [test/Examples/Static/Main.purs](../test/Examples/Static/Main.purs), and the files it serves are in [docs/Examples/Static](./Examples/Static).

## A handler, not a middleware

If you have served files from Node before, you probably did it with Express's [`serve-static`](https://github.com/expressjs/serve-static), which is a middleware. `serveStatic` is not, and this section explains why.

In Express, `serve-static` is a middleware: it is handed the raw request, strips its mount prefix, decodes the URL, and decides for itself whether it applies, falling through to the next middleware when it doesn't. It has to be, because Express has no typed routing — `req.url` is all a handler knows.

HTTPurple already has the typed route by the time a handler runs, so `serveStatic` gives you a handler, and a narrow one: it takes the segments the route captured, not the request. The router decides *whether* a request is an asset and *which* one, and the handler only decides *how* to serve it. Nothing else in the request can change which file is opened. That is the same split every other HTTPurple handler lives with, and it makes the things `serve-static` needs options for into ordinary router clauses:

- **Mount prefix** — whatever precedes `rest` in the route (`"static" / rest`).
- **Fallthrough** — a request that doesn't match the asset route never reaches the handler.
- **Index pages** — `Home` routes to `[ "index.html" ]` explicitly; there is no directory listing or index lookup to configure.
- **Method dispatch** — see [Methods and `HEAD`](#methods-and-head).

A router that calls it is wrapped by [middleware](./Middleware.md) like any other router — with the `loggingMiddleware` from the [Middleware example](./Examples/Middleware), the example above becomes `{ route, router: loggingMiddleware (router serveAsset) }`.

The one thing a middleware would give you is serving a whole tree with no route definition at all. Here that costs one constructor with `rest` — the same trade HTTPurple makes everywhere else.

## Options

`serveStatic` takes a record of options. Only `root` is required — the directory to serve, resolved against the working directory at startup if relative. Every other field is optional and takes its default when you leave it out. Write the values plainly, `dotfiles: Deny` and not `Just Deny`:

```purescript
serveStatic { root: "./public", dotfiles: Deny }
```

The one optional field so far is `dotfiles`, which says what to do with a request naming a segment that starts with `.`:

- `Ignore` (the default) answers 404, as if nothing were there
- `Deny` answers 403
- `Allow` serves it

The default hides dotfiles because a root is often a checkout or a build output and so holds `.env` or `.git/` — files that must never leak by default. `Allow` serves them, for the rare root that must publish dotted paths such as `/.well-known/`. It applies to the whole root, `.env` and `.git/` included.

The check is applied to the request's own segments after normalisation, never to the root's: a root under `~/.config/site` works, and `.git/../img` names `img`, not `.git`.

A `RootError` from `serveStatic` is one of `EmptyRoot`, `MissingRoot path` or `NotADirectory path`, where `path` is the resolved absolute path so the message names the directory actually looked for.

## What each response means

The handler answers with one of five statuses. Every response, refusals included, carries a `Content-Type` and `X-Content-Type-Options: nosniff`; the refusals have no body.

- **200** — the file was read, and the body is its contents.
- **400** — a segment contains a null byte, a `/` or a `\`. `routing-duplex` splits the URL before decoding it, so a `%2F` arrives as a literal slash inside one segment; the handler refuses it rather than treating it as a separator.
- **403** — the path would escape the root: a `..` with no earlier segment left to pop. It is 403 rather than 404 so the answer never reveals whether the target exists. Also a dotfile under `Deny`.
- **404** — no such file; a directory, including the root itself; a path through a file (`hello.txt/x`); a dotfile under `Ignore`.
- **500** — the file exists but couldn't be read: `EACCES`, `EMFILE`, `EIO` and the like. These are server faults and are deliberately not disguised as 404s.

A request can never name a file outside the root. The check works on the path alone: a symlink inside the root that points outside it is followed, as `serve-static` also does.

## Content types

The type comes from the extension of the file on disk, never from the request. Text types are declared as UTF-8, so save your text files in that encoding. The known extensions are:

`.css` `.gif` `.html` `.ico` `.jpeg` `.jpg` `.js` `.json` `.mjs` `.png` `.svg` `.txt` `.wasm` `.webp` `.woff` `.woff2`

Anything else is `application/octet-stream`, which browsers treat as an opaque download. So a file type not listed above, such as `.pdf`, is downloaded, not displayed. Together with `nosniff` on every response, this stops a browser from treating a file as a web page because its contents look like one.

## Methods and `HEAD`

The handler doesn't look at the method. If a `POST` to an asset route should be a 405, that is a router clause:

```purescript
router serveAsset { route: Asset asset, method: Get } = serveAsset { asset }
router serveAsset { route: Asset asset, method: Head } = serveAsset { asset }
router _ { route: Asset _ } = methodNotAllowed' (header "Allow" "GET, HEAD")
```

Keep the `Head` clause. HTTPurple gives a `HEAD` request the method `Head`, so without that clause it would fall through to the last one and get a 405.

`HEAD` needs nothing: Node discards the body of a response to a `HEAD` request on its own, and because HTTPurple's `Buffer` body sets `Content-Length` explicitly, a `HEAD` gets exactly `GET`'s headers with no body. (The file is still read to produce them.)

## What it doesn't do

This is a first version, limited to the essentials. These are not supported yet:

- **Index files and directory redirects.** A request naming a directory is a 404. There is no `index.html` lookup and no `/dir` → `/dir/` redirect; with typed routes the home page is a router clause, as in the example above.
- **Conditional requests and caching.** No `ETag`, `Last-Modified` or `Cache-Control`, so no 304s; every request re-sends the file.
- **Streaming.** The whole file is read into memory and sent as one buffer, so every request in progress holds a full copy of its file. Fine for ordinary web assets, not for large downloads.
- **`Range` requests.** A request for part of a file is answered with the whole file. Resuming a download and seeking in audio or video depend on this.
- **Symlink resolution.** A symlink inside the root is followed wherever it points, as above.

## Coming from `serve-static`

If you know Express's [`serve-static`](https://github.com/expressjs/serve-static) (or [`send`](https://github.com/pillarjs/send), which does its work), the behaviour you'd expect is mostly here, with these differences:

- It's a handler, not a middleware: the router decides which requests are assets, so mount prefix, fallthrough and index pages are router clauses rather than options.
- The root is resolved and checked once at boot, and an unusable root refuses to boot; `serve-static` never checks that the root exists, so a mistyped one 404s every asset instead.
- Decoding is `routing-duplex`'s, done before the handler runs, and it splits before it decodes — so a `%2F` inside a segment is a 400 here, where `send` decodes first and treats it as a separator.
- A directory is a 404. `send` redirects `/dir` to `/dir/` and then looks for an index file.
- No `ETag`, `Last-Modified` or 304, and no `Range` or 206 — yet. See [What it doesn't do](#what-it-doesnt-do).
- The body is buffered, not streamed.
- `X-Content-Type-Options: nosniff` is on every response. `send` sets it only on its own error pages and leaves file responses to `helmet`.

Traversal (403) and the dotfile policies (`allow` / `deny` / `ignore`, default `ignore`) behave the same.
