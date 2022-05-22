{ name = "httpure"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "js-uri"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-http"
  , "node-streams"
  , "options"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "record"
  , "refs"
  , "routing-duplex"
  , "strings"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/cprussin/purescript-httpure.git"
}
