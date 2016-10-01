module Models.World exposing (World, empty, generate, evolve)

import Algorithms.Conway
import Models.Person exposing (Person, init)
import Models.Cartogram exposing (Cartogram)
import Models.Terrain exposing (water, dirt, rock, sand)

import String
import Dict exposing (Dict)
import Random exposing (Generator)

-- type

type alias World = { name : String
                   , people : List Person
                   , terrain : Cartogram
                   , dimensions : (Int, Int)
                   }

empty : (Int,Int) -> World
empty dims =
  { name = "Nowhere"
  , people = []
  , terrain = Models.Cartogram.empty dims
  , dimensions = dims
  }

generate : (Int,Int) -> Generator World
generate (width,height) =
  let mapmaker = (Models.Cartogram.generate (width,height)) in
  Random.map (\terrain' ->
    { name = "Nowhere"
    , people = []
    , terrain = terrain'
    , dimensions = (width,height)
    }
    ) mapmaker

evolve : Int -> World -> World
evolve n model =
  { model | terrain = model.terrain
                      --|> Algorithms.Conway.evolve n (dirt,water)
                      |> Algorithms.Conway.evolve (n//2) (rock,dirt)
                      |> Algorithms.Conway.evolve (n//2) (sand,water)
                      |> Algorithms.Conway.evolve (n) (water,dirt)
  }
