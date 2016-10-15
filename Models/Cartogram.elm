module Models.Cartogram exposing (Cartogram, empty, generate)

import Models.Map exposing (Map, at, set)
import Models.Terrain exposing (Terrain, water, dirt)

import Random exposing (Generator)

type alias Cartogram = Map Terrain

empty : (Int,Int) -> Cartogram
empty dims =
  Models.Map.init dims (always dirt)

generate : (Int, Int) -> Generator Cartogram
generate (width,height) =
  Models.Map.random (width,height) Models.Terrain.random

type alias WorldRegion = { dimensions : (Int, Int)
                         , origin : (Int, Int)
                         }

analyze : Cartogram -> (List WorldRegion)
analyze map =
  let
    subdivisions =
      map |> subdivide
  in
    subdivisions

subdivide : Cartogram -> List WorldRegion
subdivide map =
  let
    gridWidth =
      1 + (map |> Models.Map.width)

    gridHeight =
      1 + (map |> Models.Map.height)

    regionSize =
      5

    buildRegion = (\(ox,oy) ->
      { dimensions = (regionSize, regionSize)
      , origin = (ox,oy)
      })

    width =
      gridWidth // regionSize

    height =
      gridHeight // regionSize
  in
    let
      regionRow = \y ->
        List.map (\x -> buildRegion (x,y)) [0..((width)-1)]
    in
      [0..((height)-1)]
    |> List.concatMap regionRow
