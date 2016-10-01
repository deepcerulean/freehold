module Models.Point exposing (Point, Direction(..), grid, adjacent, slide, delta)

type Direction = North
               | South
               | East
               | West
               | Northeast
               | Northwest
               | Southeast
               | Southwest

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

type alias Point = (Int, Int)

grid : Int -> Int -> List Point
grid width height =
  let
    gridPoint = \y ->
      List.map (\x -> (x,y)) [0..(width-1)]
  in
    [0..(height-1)]
    |> List.concatMap gridPoint

translate : Point -> Point -> Point
translate (ax,ay) (bx,by) =
  (ax+bx, ay+by)

slide : Direction -> Point -> Point
slide dir (x,y) =
  translate (delta dir) (x,y)

adjacent : Point -> List Point
adjacent pt =
  allDirections
  |> List.map (\dir -> slide dir pt)

delta : Direction -> Point
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
