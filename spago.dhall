{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "datetime"
  , "effect"
  , "halogen"
  , "halogen-subscriptions"
  , "lists"
  , "node-readline"
  , "now"
  , "numbers"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
