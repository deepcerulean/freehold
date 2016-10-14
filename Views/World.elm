module Views.World exposing (view)

import Models.Terrain
import Models.Point exposing (Point)
import Models.World exposing (World)
import Views.Terrain
import Views.Person
import Graphics exposing (rect, outline)

import Dict
import Svg

view : Maybe (Point Int) -> Maybe (Point Int) -> World -> List (Svg.Svg a)
view hoverAt selectAt model =
  let
    hover =
      case hoverAt of
        Nothing ->
          []

        Just pos ->
          [ rect pos "rgba(255,255,255,0.8)" ]

    select =
      case selectAt of
        Nothing ->
          []

        Just pos ->
          [ outline pos "rgba(255,255,255,0.8)" ]

    bg =
      [ Graphics.quad (0,0) model.dimensions (Models.Terrain.color Models.Terrain.dirt) ]

    people =
      model.people
        |> List.concatMap (Views.Person.view)

  in
    bg ++
    (model |> terrainView)
    ++ people
    ++ hover
    ++ select


terrainView : World -> List (Svg.Svg a)
terrainView model =
  model.terrain
  |> Dict.toList
  |> List.concatMap (\(pt,terrain) ->
    if terrain == Models.Terrain.dirt then [] else
    [ terrain |> Views.Terrain.view pt ]
  )

