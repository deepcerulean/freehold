module Models.Map exposing (Map, Location, init, set, at, random)

import Models.Point exposing (Point, grid)
import Extend.List exposing (zip)

import Dict exposing (Dict)
import Random exposing (Generator, andThen)

type alias Location = Point Int

type alias Map a = Dict Location a

init : (Int, Int) -> (Location -> a) -> Map a
init (width, height) inspect =
  grid width height
  |> List.foldr (set' inspect) Dict.empty

set : a -> Location -> Map a -> Map a
set a pt model =
  model |> Dict.insert pt a

set' : (Location -> a) -> Location -> Map a -> Map a
set' f pt model =
  model |> set (f pt) pt

at : Location -> Map a -> Maybe a
at pt model =
  model |> Dict.get pt

fromList : (Int, Int) -> List a -> Map a
fromList (width, height) ls =
  let grid' = grid width height in
  ls
  |> zip grid'
  |> Dict.fromList

random : (Int,Int) -> (Generator a) -> Generator (Map a)
random (width,height) generate =
  let
    generate' =
      Random.list (width*height) generate

    grid' =
      grid width height
  in
    Random.map (fromList (width,height)) generate'

-- sparse random? that like builds a sparse hash (with not all the keys filled in)
-- still constrained in the space...
