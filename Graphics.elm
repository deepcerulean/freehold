module Graphics exposing (view, rect, outline, quad, circle, text, line)

import Models.Point exposing (Point)
import Viewport exposing (Viewport)

import Html exposing (Html)
import Html.Attributes exposing (style)

import Svg exposing (Svg, svg, rect, g, text', font)
import Svg.Attributes exposing (viewBox, x, y, fontSize, textAnchor, fill, stroke, strokeWidth, width, height, preserveAspectRatio, transform, cx, cy, r, fontFamily, fontWeight, x1, y1, x2, y2)

view : Viewport -> List (Svg a) -> List (Svg a) -> Html a
view viewport view interfaceView =
  let
    (width,height) =
      viewport.worldDims

    (vWidth, vHeight) =
       viewport.dimensions

    vDim =
      "0 0 " ++ (toString width) ++ " " ++ (toString height)

    props =
      [ viewBox vDim
      , style [("height", (toString (vHeight+1)))
              ,("width", (toString (vWidth)))
              ]
      , preserveAspectRatio "xMinYMin"
      ]

    (offsetX, offsetY) =
      viewport |> Viewport.offset

    wrapView =
      g [ transform ("scale(" ++ toString viewport.scale ++ ") translate(" ++toString offsetX++ ", " ++ toString offsetY ++ ")") ] view
  in
    svg props ([ wrapView ] ++ interfaceView) -- [wrapView] ++ interfaceView ]

quad : Point number -> (Int,Int) -> String -> Svg msg
quad (x',y') (width',height') color =
  Svg.rect [ x (toString x')
           , y (toString y')
           , width (toString width')
           , height (toString height')
           , fill color
           ] []

rect : Point number -> String -> Svg msg
rect (x',y') color =
  Svg.rect [ x (toString x')
       , y (toString y')
       , width "1.001"
       , height "1.001"
       , fill color
       ] []

circle : (Float,Float) -> Float -> String -> Svg msg
circle (x',y') radius color =
  Svg.circle [ cx (toString x')
             , cy (toString y')
             , r (toString radius) --"1.001"
             , stroke "black"
             , strokeWidth "0.01"
             --, height (toString radius) --"1.001"
             , fill color
             ] []

outline : Point number -> String -> Svg msg
outline (x', y') color =
  Svg.rect [ x (toString x')
       , y (toString y')
       , width "1"
       , height "1"
       , stroke color
       , strokeWidth "0.02"
       , fill "rgba(255,255,255,0.0)"
       ] []

text : Point number -> String -> Float -> String -> Svg msg
text (px,py) color size string =
  text' [ x (toString px)
        , y (toString py)
        , fontSize (toString size)
        , fontFamily "Helvetica"
        , fontWeight "lighter"
        , fill color
        , textAnchor "middle"
        ] [ Html.text string ]

line : Point number -> Point number -> String -> Svg msg
line (ax,ay) (bx,by) color =
  Svg.line [ x1 (toString ax)
           , y1 (toString ay)
           , x2 (toString bx)
           , y2 (toString by)
           , stroke color
           , strokeWidth "0.1"
           ] []
