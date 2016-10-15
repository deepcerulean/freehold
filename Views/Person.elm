module Views.Person exposing (view)

import Models.Person exposing (Person)
import Extend.List

import Graphics
import Svg
import Dict

view : Person -> List (Svg.Svg a)
view model =
  let
    (x,y) =
      model.body.position

    goalLine =
      case model.goal of
        Just (gx,gy) ->
          [ Graphics.line (x,y) (0.5 + (toFloat gx), 0.5 + (toFloat gy)) "rgba(240,160,160,0.8)" ]

        Nothing ->
          []

    pathLines =
      if List.length model.path > 1 then
        model.path
        |> Extend.List.zip ((model.path) |> List.drop 1)
        |> List.foldl (\((ax,ay), (bx,by)) lines ->
          let
            a = ((toFloat ax)+0.5, (toFloat ay)+0.5)
            b = ((toFloat bx)+0.5,(toFloat by)+0.5)
          in
          lines ++ [ Graphics.line a b "rgba(240,240,240,0.5)" ]
          ) []
      else
        []

    searchGrid =
      case model.pathfinding of
        Nothing ->
          []
        Just ctx ->
          ctx.visited
            |> Dict.keys
            |> List.map (\(x',y') -> Graphics.circle 0.4 "rgba(160,240,160,0.3)" (0.5 + (toFloat x'), 0.5 + (toFloat y')))

  in
      [ Graphics.circle (0.5) "rgb(80,80,240)" (x,y)
      , Graphics.circle (0.3) "rgb(220,180,180)" (x,y-0.5)
      , Graphics.text (x,y-1.0) "rgb(240,240,240)" 0.3 model.name
      ]
        ++ goalLine
        ++ pathLines
        ++ searchGrid
