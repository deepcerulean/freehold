module Models.Point exposing (Point
                             , Direction(..)
                             , grid
                             , adjacent
                             , nearby
                             , slide
                             , delta
                             , towards
                             , distance
                             , allDirections
                             , code
                             , invertDirection
                             , unitLength
                             , manhattanDistance
                             , asFloat
                             , asInt
                             )

import Extend.List exposing (andThen)

type Direction = North
               | South
               | East
               | West
               | Northeast
               | Northwest
               | Southeast
               | Southwest

allDirections : List Direction
allDirections =
  [ North
  , South
  , East
  , West
  , Northeast
  , Northwest
  , Southeast
  , Southwest
  ]

unitLength : Direction -> Float
unitLength dir =
  case dir of
    North -> 1.0
    South -> 1.0
    East -> 1.0
    West -> 1.0
    Northeast -> sqrt 2
    Northwest -> sqrt 2
    Southwest -> sqrt 2
    Southeast -> sqrt 2

type alias Point number = (number, number)

grid : Int -> Int -> List (Point Int)
grid width height =
   [0..(width-1)] `andThen` \x ->
   [0..(height-1)] `andThen` \y ->
   [(x,y)]

distance : Point Float -> Point Float -> Float
distance (ax,ay) (bx,by) =
  let
    dx =
      (ax - bx)

    dy =
      (ay - by)
  in
    sqrt(  (dx*dx) + (dy*dy)  )

manhattanDistance : Point Int -> Point Int -> Int
manhattanDistance (ax,ay) (bx,by) =
  let
    dx =
      (ax - bx)

    dy =
      (ay - by)
  in
    abs dx + abs dy
    --sqrt(  (dx*dx) + (dy*dy)  )

translate : Point number -> Point number -> Point number
translate (ax,ay) (bx,by) =
  (ax+bx, ay+by)

slide : Direction -> Point number -> Point number
slide dir (x,y) =
  translate (delta dir) (x,y)

adjacent : Point number -> List (Point number)
adjacent pt =
  allDirections
  |> List.map (\dir -> slide dir pt)

nearby : Int -> Point Int -> List (Point Int)
nearby dist pt =
  grid (dist*2) (dist*2)
    |> List.filter (\pt' -> ((manhattanDistance pt' (dist,dist)) < dist))
    |> List.map (translate pt)
    |> List.map (translate (-dist, -dist))

delta : Direction -> Point number
delta dir =
  case dir of
    North ->
      (0,-1)

    South ->
      (0,1)

    East ->
      (1,0)

    West ->
      (-1,0)

    Northwest ->
      translate (delta North) (delta West)

    Northeast ->
      translate (delta North) (delta East)

    Southwest ->
      translate (delta South) (delta West)

    Southeast ->
      translate (delta South) (delta East)

towards : Point number -> Point number -> Direction
towards (ax,ay) (bx,by) =
  if (ax > bx) && (ay > by) then
     Southeast
  else
    if (ax < bx) && (ay > by) then
      Southwest
    else
      if (ax > bx) && (ay < by) then
        Northeast
      else
        if (ax < bx) && (ay < by) then
          Northwest
        else
          towards' (ax,ay) (bx,by)

towards' : Point number -> Point number -> Direction
towards' (ax,ay) (bx,by) =
  let
    dx =
      abs (ax - bx)
    dy =
      abs (ay - by)

  in
    if dx > dy then
      if (ax > bx) then
        East
      else
        West
    else
      if (ay > by) then
        South
      else
        North

code : Point Int -> Int
code (x,y) =
  (x * 10000) + y

invertDirection : Direction -> Direction
invertDirection direction =
  case direction of
    North -> South
    South -> North
    East -> West
    West -> East
    Northeast -> Southwest
    Northwest -> Southeast
    Southwest -> Northeast
    Southeast -> Northwest

asFloat : Point Int -> Point Float
asFloat (x,y) =
  (toFloat x, toFloat y)

asInt : Point Float -> Point Int
asInt (x,y) =
  (round x, round y)
