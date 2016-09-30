module Models.Point exposing (Point, grid, adjacent, slide, delta)

import Models.Direction exposing (Direction(..))

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

delta : Direction -> Point
delta dir =
  case dir of
    Models.Direction.North ->
      (0,-1)

    Models.Direction.South ->
      (0,1)

    Models.Direction.East ->
      (1,0)

    Models.Direction.West ->
      (-1,0)

adjacent : Point -> List Point
adjacent pt =
  Models.Direction.all
  |> List.map (\dir -> slide dir pt)
