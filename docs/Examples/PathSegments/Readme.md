# Path Segments Example

This is a basic example that demonstrates working with URL segments. It includes
code that fetches the whole set of URL segments as an array of strings, and code
that routes based on the value of specific segments.

To run the example server, run:

```bash
nix-shell --run 'example PathSegments'
```

Or, without nix:

```bash
spago -x test.dhall run --main Examples.PathSegments.Main
```
