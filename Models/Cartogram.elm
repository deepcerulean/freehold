module Models.Cartogram exposing (Cartogram, empty, generate)

--import Algorithms.Conway exposing (evolve)
import Models.Map exposing (Map, at, set)
import Models.Terrain exposing (Terrain, water, dirt)
import Models.Point exposing (Point)

import Dict exposing (Dict)
import Random exposing (Generator)

type alias Cartogram = Map Terrain

empty : (Int,Int) -> Cartogram
empty dims =
  Models.Map.init dims (always dirt)

generate : (Int, Int) -> Generator Cartogram
generate (width,height) =
  Models.Map.random (width,height) Models.Terrain.random
