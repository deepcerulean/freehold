module Models.Thing exposing (Thing, init, campfire, fire)

import Models.Point exposing (Point)

type alias Thing = { name : String
                   , id : Int
                   , location : Point Int
                   , assetUrl : String
                   , frames : Int
                   }

init : Int -> String -> String -> Int -> Point Int -> Thing
init id name url frames pt =
    { name = name
    , id = id
    , location = pt
    , assetUrl = url
    , frames = frames
    }

campfire : Int -> Point Int -> Thing
campfire id pt =
  init id "Campfire" "wood" 1 pt


fire : Int -> Point Int -> Thing
fire id pt =
  init id "Fire" "fire" 2 pt
