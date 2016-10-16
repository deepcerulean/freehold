module Views.Thing exposing (view)

import Models.Thing exposing (Thing)
import Graphics
import Svg exposing (image)

--import Html exposing (Html, img)
import Svg.Attributes exposing (xlinkHref, width, height, x, y)
--import VirtualDom
--text (px,py) color size string

view : Int -> Thing -> List (Svg.Svg a)
view step model =
  let thingImage = imageFor step model in
  --    [ thingImage ]
  [ Graphics.text model.location "white" 0.3 model.name
  , Graphics.rect' model.location "rgba(240,160,160,0.3)" [] -- thingImage ]
  , thingImage
  ]

imageFor : Int -> Thing -> Svg.Svg a
imageFor step model =
  let
    (x',y') =
      model.location

    frame =
      if model.frames == 1 then "" else
        toString (1+((step//10) % model.frames))
    url =
      "/media/" ++ model.assetUrl ++ frame ++ ".svg"
  in
  image [ xlinkHref url, width "1", height "1", x (toString x'), y (toString y') ] []
