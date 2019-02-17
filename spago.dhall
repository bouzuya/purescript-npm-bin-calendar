{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purescript-npm-bin-calendar"
, dependencies =
    [ "bouzuya-datetime"
    , "console"
    , "datetime"
    , "effect"
    , "foreign-object"
    , "formatters"
    , "node-fs"
    , "node-process"
    , "psci-support"
    , "simple-json"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
