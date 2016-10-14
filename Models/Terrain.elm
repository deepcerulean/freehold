module Models.Terrain exposing (Terrain, random, dirt, water, rock, sand, bedrock, color, hoverColor)

import Models.Point exposing (Point)

import Random exposing (Generator)

type Terrain = Dirt
             | Water
             | Rock
             | Sand
             | Bedrock

bedrock : Terrain
bedrock =
  Bedrock

dirt : Terrain
dirt =
  Dirt

water : Terrain
water =
  Water

rock : Terrain
rock =
  Rock

sand : Terrain
sand =
  Sand

color : Terrain -> String
color terrain =
  case terrain of
    Dirt ->
      "rgb(80,160,80)"

    Water ->
      "rgb(120,120,180)"

    Rock ->
      "rgb(160,160,160)"

    Sand ->
      "rgb(160,160,80)"

    Bedrock ->
      "rgb(180,120,120)"

hoverColor : Terrain -> String
hoverColor terrain =
  case terrain of
    Dirt ->
      "rgb(160,240,160)"

    Water ->
      "rgb(160,160,240)"

    Rock ->
      "rgb(240,240,240)"

    Sand ->
      "rgb(240,240,160)"

    Bedrock ->
      "rgb(240,160,160)"

pick : Int -> Terrain
pick n =
  if n < 5 then
    water
  else if n < 10 then
    rock
  else if n < 12 then
    sand
  else if n < 36 then
    dirt
  else
    bedrock

random : Generator Terrain
random =
  Random.map pick (Random.int 0 100)
