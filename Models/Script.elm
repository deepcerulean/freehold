module Models.Script exposing (Script, Action(..), init)

type Action = Seek String
            | Wander

type alias Script = List Action

init : Script
init =
  [ Wander
  , Seek "campfire"
  ]
