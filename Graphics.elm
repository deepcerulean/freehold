module Graphics exposing (viewbox, rect)

import Models.Point exposing (Point)

import Html exposing (Html)
import Html.Attributes exposing (style)

import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes exposing (viewBox, x, y, fontSize, textAnchor, fill, stroke, strokeWidth, width, height, preserveAspectRatio, transform)
import Svg.Events

viewbox : (Int,Int) -> (Int,Int) -> Float -> (Float,Float) -> List (Svg.Svg a) -> Html a
viewbox (width,height) (vWidth, vHeight) scale (offsetX, offsetY) view =
  let
    vDim =
      "0 0 " ++ (toString vWidth) ++ " " ++ (toString vHeight)

    props =
      [ viewBox vDim
      , style [("height", (toString (height)))
              ,("width", (toString (width)))
              ]
      , preserveAspectRatio "xMinYMin"
      ]
    wrapView =
      g [ transform ("scale(" ++ toString scale ++ ") translate(" ++toString offsetX++ ", " ++ toString offsetY ++ ")") ] view
  in
    svg props [ wrapView ]

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
