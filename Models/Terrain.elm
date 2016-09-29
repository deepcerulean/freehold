module Models.Terrain exposing (Terrain, dirt, water, rock, color, random)

import Algorithms.Champ exposing (champernowne)
import Models.Point exposing (Point)

import Random exposing (Generator)

type Terrain = Dirt
             | Water
             | Rock

dirt : Terrain
dirt =
  Dirt

water : Terrain
water =
  Water

rock : Terrain
rock =
  Rock

color : Terrain -> String
color terrain =
  case terrain of
    Dirt ->
      "rgb(80,160,80)"

    Water ->
      "rgb(120,120,180)"

    Rock ->
      "rgb(160,160,160)"

pick : Int -> Terrain
pick n =
  if n == 0 then
    water
  else if n == 1 then
    rock
  else
    dirt

random : Generator Terrain
random =
  Random.map pick (Random.int 0 2)
