{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, dependencies = 
    [ "prelude"
    , "console"
    , "effect"
    , "variant"
    , "nonempty"
    , "aff"
    , "halogen"
    , "halogen-formless"
    , "remotedata"
    , "routing"
    , "formatters"
    , "routing-duplex"
    , "now"
    , "affjax"
    , "slug"
    , "precise-datetime"
    , "typelevel-prelude"
    , "argonaut-core"
    , "argonaut-codecs"
    , "aff-bus"
    , "struct"
    , "tolerant-argonaut"
    , "css"
    , "halogen-css"
    , "colors"
    , "simple-json"
    , "remotedata"
    , "halogen-select"
    , "generics-rep"
    , "argonaut-codecs"
    , "argonaut-generic"
, "psci-support", "free", "effect", "halogen-hooks-extra",
"type-equality" , "event", "refs", "avar", "debug", "undefined", "folds", "web-events",
"datetime", "web-html", "record", "halogen-hooks", "halogen-portal", "tuples", "web-uievents", "random"
{- , "halogen-portal"-}
    ]
}
