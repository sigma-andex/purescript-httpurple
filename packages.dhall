let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230804/packages.dhall
        sha256:85d0df546868128fdbd6b4fc309252d69d2c0e99090c7e8ad80e97986f4d9ac4

in  upstream
  with node-event-emitter.version = "v3.0.0"
  with node-event-emitter.dependencies =
    [ "effect"
    , "either"
    , "functions"
    , "maybe"
    , "nullable"
    , "prelude"
    , "unsafe-coerce"
    ]
  with node-buffer.version = "v9.0.0"
  with node-buffer.dependencies =
    [ "arraybuffer-types"
    , "effect"
    , "maybe"
    , "st"
    , "unsafe-coerce"
    , "nullable"
    ]
  with node-fs.version = "v9.1.0"
  with node-fs.dependencies =
    [ "datetime"
    , "effect"
    , "either"
    , "enums"
    , "exceptions"
    , "functions"
    , "integers"
    , "js-date"
    , "maybe"
    , "node-buffer"
    , "node-path"
    , "node-streams"
    , "nullable"
    , "partial"
    , "prelude"
    , "strings"
    , "unsafe-coerce"
    ]
  with node-streams.version = "v9.0.0"
  with node-streams.dependencies =
    [ "aff"
    , "effect"
    , "exceptions"
    , "maybe"
    , "node-buffer"
    , "node-event-emitter"
    , "nullable"
    , "prelude"
    , "unsafe-coerce"
    ]
  with node-process.version = "v11.2.0"
  with node-process.dependencies =
    [ "effect"
    , "foreign-object"
    , "foreign"
    , "maybe"
    , "node-streams"
    , "node-event-emitter"
    , "posix-types"
    , "prelude"
    , "unsafe-coerce"
    ]
  with node-net.version = "v5.1.0"
  with node-net.dependencies =
    [ "console"
    , "datetime"
    , "effect"
    , "exceptions"
    , "maybe"
    , "node-buffer"
    , "node-event-emitter"
    , "node-fs"
    , "node-streams"
    , "nullable"
    , "partial"
    , "prelude"
    , "unsafe-coerce"
    ]
  with node-url.version = "v7.0.0"
  with node-url.dependencies =
    [ "prelude"
    , "effect"
    , "foreign"
    , "nullable"
    , "tuples"
    ]
  with node-zlib =
    { dependencies =
      [ "aff"
      , "console"
      , "effect"
      , "either"
      , "functions"
      , "node-buffer"
      , "node-streams"
      , "prelude"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/purescript-node/purescript-node-zlib.git"
    , version = "v0.4.0"
    }
  with node-readline.version = "v8.1.0"
  with node-readline.dependencies =
    [ "effect"
    , "foreign"
    , "node-event-emitter"
    , "node-process"
    , "node-streams"
    , "options"
    , "prelude"
    ]
  with node-tls =
    { dependencies =
      [ "console"
      , "effect"
      , "either"
      , "exceptions"
      , "foreign"
      , "maybe"
      , "node-buffer"
      , "node-event-emitter"
      , "node-net"
      , "node-streams"
      , "nullable"
      , "partial"
      , "prelude"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/purescript-node/purescript-node-tls.git"
    , version = "v0.3.1"
    }
  with node-http.version = "v9.1.0"
  with node-http.dependencies =
    [ "arraybuffer-types"
    , "contravariant"
    , "effect"
    , "foreign"
    , "foreign-object"
    , "maybe"
    , "node-buffer"
    , "node-net"
    , "node-streams"
    , "node-tls"
    , "node-url"
    , "nullable"
    , "options"
    , "prelude"
    , "unsafe-coerce"
    ]
