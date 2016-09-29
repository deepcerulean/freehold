module Views.World exposing (view)

import Models.World exposing (World)
import Views.Terrain

import String
import Dict
import Html

view model =
  let
    people =
      model.people

    message =
      model.name
      ++ ": "
      ++ ((List.map .name people) |> String.join ", ")
  in
    terrainView model
    --++
    --[
    --  Svg.text' [ x "50"
    --      , y "50"
    --      , fill "blue"
    --      , fontSize "1pt"
    --      , textAnchor "middle"
    --      ] [ Html.text message ]
    --]

terrainView model =
  model.terrain
  |> Dict.toList
  |> List.map (\(pt,terrain) ->
    terrain |> Views.Terrain.view pt)
