module Views.World exposing (view)

import Models.World exposing (World)
import Views.Terrain

import String
import Dict
import Html

view hoverAt model =
  case hoverAt of
    Nothing ->
      terrainView model
    Just pos ->
      terrainView' pos model

terrainView model =
  model.terrain
  |> Dict.toList
  |> List.map (\(pt,terrain) ->
    terrain |> Views.Terrain.view pt
  )

terrainView' pos model =
  model.terrain
  |> Dict.toList
  |> List.map (\(pt,terrain) ->
    if pos == pt then
      terrain |> Views.Terrain.hover pt
    else
      terrain |> Views.Terrain.view pt
  )
