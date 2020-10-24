{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "audio-behaviors"
  , "canvas"
  , "console"
  , "drawing"
  , "effect"
  , "psci-support"
  , "typelevel-klank-dev"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
