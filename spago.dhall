{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "graphql-gen"
, license = "Apache-2.0"
, dependencies = [ "console", "effect", "graphql-parser", "quickcheck" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
