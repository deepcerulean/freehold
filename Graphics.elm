module Graphics exposing (viewbox, rect)

import Models.Point exposing (Point)

import Html exposing (Html)
import Html.Attributes exposing (style)

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (viewBox, x, y, fontSize, textAnchor, fill, stroke, strokeWidth, width, height)
import Svg.Events

viewbox (width,height) (vWidth, vHeight) view =
  let
    vDim =
      "0 0 " ++ (toString vWidth) ++ " " ++ (toString vHeight)

    props =
      [ viewBox vDim
      , style [("height", (toString height))
              ,("width", (toString width))
              ]
      ]
  in
    svg props view

rect : Point -> String -> Svg msg
rect (x',y') color =
  Svg.rect [ x (toString x')
       , y (toString y')
       , width "1"
       , height "1"
       , stroke "black"
       , strokeWidth "0.01"
       , fill color
       ] []
