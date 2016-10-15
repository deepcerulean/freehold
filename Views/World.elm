module Views.World exposing (view)

import Models.Terrain
import Models.Point exposing (Point)
import Models.World exposing (World)
--import Views.Terrain
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


    people =
      model.people
        |> List.concatMap (Views.Person.view)

  in
    (model |> terrainView)
    ++ people
    ++ hover
    ++ select


-- really a cartogram view maybe?? which should prob *include* bg
terrainView : World -> List (Svg.Svg a)
terrainView model =
  let
    bg =
      [ Graphics.quad (0,0) model.dimensions (Models.Terrain.color Models.Terrain.dirt) ]
  in bg ++
  (model.nonDirtTerrain
  |> Dict.toList
  --|> List.filter (\(pt,terrain) -> not (terrain == Models.Terrain.dirt))
  |> List.map (\(pt,terrain) -> Graphics.rect pt (Models.Terrain.color terrain)))

