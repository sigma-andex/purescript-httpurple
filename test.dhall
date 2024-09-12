let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs", "docs/Examples/**/*.purs" ],
  dependencies = conf.dependencies # [
  , "exceptions"
  , "foreign"
  , "node-child-process"
  , "spec"
  , "transformers"
  , "unsafe-coerce" 
  , "typelevel-prelude"
  , "js-date"
  , "nullable"
  , "tailrec"
  ]
}
