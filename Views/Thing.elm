module Views.Thing exposing (view)

import Models.Thing exposing (Thing(..), Entity, Kind(..), isAtomic, location, name)
import Graphics
import Svg exposing (image)

import Svg.Attributes exposing (xlinkHref, width, height, x, y)

view : Int -> Thing -> List (Svg.Svg a)
view step model =
  let thingView = viewFor step model in
  --    [ thingImage ]
  [ Graphics.text (model |> location) "white" 0.3 (model |> name)
  , Graphics.rect' (model |> location) "rgba(240,160,160,0.3)" [] -- thingImage ]
  ]
  ++ thingView

viewFor : Int -> Thing -> List (Svg.Svg a)
viewFor step model =
  case model of
    Atom entity ->
      [ (entity |> imageFor step) ]
    Molecule _ _ parts ->
      List.concatMap (viewFor step) (parts)

imageFor : Int -> Entity -> Svg.Svg a
imageFor step model =
  let
    (x',y') =
      model.location

    frame =
      if (model |> frames) == 1 then "" else
        toString (1+((step//10) % (model |> frames)))
    url =
      "/media/" ++ (model |> assetUrl) ++ frame ++ ".svg"
  in
  image [ xlinkHref url, width "1", height "1", x (toString x'), y (toString y') ] []

assetUrl : Entity -> String
assetUrl entity =
  case entity.kind of
    Wood ->
      "wood"

    Fire ->
      "fire"

frames : Entity -> Int
frames entity =
  case entity.kind of
    Wood ->
      1

    Fire ->
      2
