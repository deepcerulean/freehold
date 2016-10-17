module Views.World exposing (view)

import Models.Terrain
import Models.Point exposing (Point)
import Models.World exposing (World)
import Models.Thing exposing (Thing)
import Models.Person exposing (Person)

--import Views.Terrain
import Views.Person
import Views.Thing
import Graphics exposing (rect, outline)

import Dict
import Svg

type ViewModel = ThingView Float Thing | PersonView Float Person

thingView : Thing -> ViewModel
thingView thing =
  let
    (_,y) =
      thing |> Models.Thing.location

    zIndex =
      toFloat y
  in
    ThingView zIndex thing

personView : Person -> ViewModel
personView person =
  let (_,y) = person.body.position in
  PersonView y person

renderModel : Int -> ViewModel -> List (Svg.Svg a)
renderModel steps viewModel =
  case viewModel of
    ThingView _ thing ->
      Views.Thing.view steps thing

    PersonView _ person ->
      Views.Person.view person

zIndex : ViewModel -> Float
zIndex viewModel =
  case viewModel of
    ThingView z _ ->
      z

    PersonView z _ ->
      z

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

    viewModels =
      (model.people |> List.map (personView)) ++
      (model.things |> List.map (thingView))
      |> List.sortBy (\viewModel -> viewModel |> zIndex)
      |> List.concatMap (renderModel model.steps)

  in
    (model |> terrainView)
    ++ viewModels
    ++ hover
    ++ select


-- todo really a cartogram view maybe?? which should prob *include* bg
terrainView : World -> List (Svg.Svg a)
terrainView model =
  let
    bg =
      [ Graphics.quad (0,0) model.dimensions (Models.Terrain.color Models.Terrain.dirt) ]
  in bg ++
  (model.nonDirtTerrain
  |> Dict.toList
  |> List.map (\(pt,terrain) -> Graphics.rect pt (Models.Terrain.color terrain)))
