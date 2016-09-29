module Models.Map exposing (Map, init, set, at, random)

import Models.Point exposing (Point, grid)
import Models.Terrain exposing (Terrain, water, dirt)
import Extend.List exposing (zip)

import Dict exposing (Dict)
import Random exposing (Generator, andThen)

type alias Map a = Dict Point a

init : (Int, Int) -> (Point -> a) -> Map a
init (width, height) inspect =
  grid width height
  |> List.foldr (set' inspect) Dict.empty

set : a -> Point -> Map a -> Map a
set a pt model =
  model |> Dict.insert pt a

set' : (Point -> a) -> Point -> Map a -> Map a
set' f pt model =
  model |> set (f pt) pt

at : Point -> Map a -> Maybe a
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
