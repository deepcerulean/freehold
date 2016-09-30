module Graphics exposing (rect, view)

import Models.Point exposing (Point)
import Viewport exposing (Viewport)

import Html exposing (Html)
import Html.Attributes exposing (style)

import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes exposing (viewBox, x, y, fontSize, textAnchor, fill, stroke, strokeWidth, width, height, preserveAspectRatio, transform)
import Svg.Events

view : Viewport -> List (Svg.Svg a) -> Html a
view {worldDims,dimensions,scale,offset} view =
  let
    (width,height)  =
      worldDims

    (vWidth, vHeight) =
       dimensions

    vDim =
      "0 0 " ++ (toString width) ++ " " ++ (toString height)

    props =
      [ viewBox vDim
      , style [("height", (toString (vHeight)))
              ,("width", (toString (vWidth)))
              ]
      , preserveAspectRatio "xMinYMin"
      ]

    (offsetX, offsetY) =
      offset

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
