module Models.World exposing (World, init)

import String

-- type
type alias World = {
  name : String
}

-- init
init : World
init =
  { name = "Gooblax" }
