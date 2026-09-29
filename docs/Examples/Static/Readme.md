# Static Example

This example serves the files in [`public/`](./public) from disk. `/` answers
with `index.html`, and everything under `/static/` maps onto the directory, so
`/static/style.css` is `public/style.css`. A request for `.secret` is a 404: the
handler hides dotfiles unless told otherwise.

To run the server, run this from the root of the repository:

```bash
spago run --main Examples.Static.Main
```
