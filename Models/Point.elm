module Models.Point exposing (Point, grid, adjacent, slide)

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

slide : Direction -> Point -> Point
slide dir (x,y) =
  case dir of
    Models.Direction.North ->
      (x,y-1)

    Models.Direction.South ->
      (x,y+1)

    Models.Direction.East ->
      (x+1,y)

    Models.Direction.West ->
      (x-1,y)

adjacent : Point -> List Point
adjacent pt =
  Models.Direction.all
  |> List.map (\dir -> slide dir pt)
