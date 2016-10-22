module Viewport exposing (Viewport, init, pan, zoom, resize, find, animate, offset)

import Models.Point exposing (Point, Direction)
import Body exposing (Body)

import Mouse

-- more "global" config :(
-- could push these into 'settings' param on viewport?
friction : Float
friction = 0.34

panFactor : Float
panFactor = 2.25

maxPanSpeed : Float
maxPanSpeed = 4.0

minZoom : Float
minZoom = 1

maxZoom : Float
maxZoom = 8

zoomFriction : Float
zoomFriction = 0.04

maxZoomVelocity : Float
maxZoomVelocity = 0.8

zoomFactor : Float
zoomFactor = 0.125

sign : Float -> Float
sign x = if x < 0 then -1 else if x > 0 then 1 else 0

type alias Viewport = { dimensions : ( Int, Int )
                      , scale : Float
                      , worldDims : ( Int, Int )
                      , zoomVelocity : Float
                      , offsetBody : Body
                      }

init : (Int,Int) -> Viewport
init (width,height) = { dimensions = (800,600)
                      , scale = minZoom --(minZoom+maxZoom)/2
                      , worldDims = (width,height)
                      , zoomVelocity = 0
                      , offsetBody = Body.init (0,0) friction
                      }
                      |> centerAt (width//2,height//2)

offset : Viewport -> (Float, Float)
offset model =
  model.offsetBody.position

panVelocity : Viewport -> (Float, Float)
panVelocity model =
  model.offsetBody.velocity

find : Mouse.Position -> Viewport -> (Int,Int)
find pos model =
  let
    (x,y) =
      (toFloat (pos.x), toFloat (pos.y))

    scale =
      model |> scaleFactor

    (ox, oy) =
      model |> offset
  in
      (round (((x) * scale) - (0.5 + ox)), round (((y) * scale) - (0.5 + oy)))

pan : Float -> Direction -> Viewport -> Viewport
pan factor dir model =
  let
    (dx,dy) =
      Models.Point.delta dir

    vx' =
      factor * (toFloat dx)

    vy' =
      factor * (toFloat dy)

    offsetBody =
      model.offsetBody
        |> Body.applyImpulse (vx', vy')
        |> Body.constrainVelocity maxPanSpeed maxPanSpeed
  in
    { model | offsetBody = offsetBody
    }

zoom : Int -> Viewport -> Viewport
zoom delta model =
  let
    dy =
      zoomFactor * (toFloat delta)
  in
    model |> accelerateZoomVelocity dy

accelerateZoomVelocity : Float -> Viewport -> Viewport
accelerateZoomVelocity factor model =
  let
    zoomVelocity =
      if abs (model.zoomVelocity + factor) > maxZoomVelocity then
        maxZoomVelocity * sign model.zoomVelocity
      else
        model.zoomVelocity + factor
  in
  { model | zoomVelocity = zoomVelocity }

resize : (Int,Int) -> Viewport -> Viewport
resize dims model =
  { model | dimensions = dims }

findCenter : Viewport -> (Int,Int)
findCenter model =
  let
    scale =
      model
      |> scaleFactor

    cellSize =
      (1/scale)

    (vWidth,vHeight) =
      model.dimensions

    (ox,oy) =
      model |> offset

    (scx,scy) =
      ( toFloat vWidth / 2, toFloat vHeight / 2)

    (rcx,rcy) =
      ( scx - ((0.5 + ox) * cellSize), scy - ((0.5 + oy) * cellSize))

    (pcx,pcy) =
      (rcx / cellSize, rcy / cellSize)
  in
    (round pcx, round pcy)

centerAt : (Int,Int) -> Viewport -> Viewport
centerAt (x,y) model =
  let
    (width,height) =
      model.dimensions

    scale =
      model |> scaleFactor

    cellSize =
      (1/scale)

    w =
      ((toFloat width) / cellSize) /2

    h =
      ((toFloat height) / cellSize) /2

    ox =
      -(toFloat x) + w

    oy =
      -(toFloat y) + h
  in
    { model | offsetBody = model.offsetBody |> Body.place (ox - 0.5,oy - 0.5) }

scaleFactor : Viewport -> Float
scaleFactor model =
  let
    aspectRatio =
      (toFloat height) / (toFloat width)

    (vWidth,vHeight) =
      model.dimensions

    (width,height) =
      model.worldDims --.dimensions

    scale' =
      (if (toFloat vHeight / toFloat vWidth) < aspectRatio then
        (toFloat height / toFloat vHeight)
      else
        (toFloat width / toFloat vWidth)
      )
  in
    scale' / ( model.scale )


animate : Viewport -> Viewport
animate model =
  model
    |> animateZoom
    |> animatePan

animateZoom : Viewport -> Viewport
animateZoom model =
  let
    scale =
      min maxZoom (max minZoom (model.scale - model.zoomVelocity))

    zoomVelocity =
      model.zoomVelocity - (zoomFriction * sign model.zoomVelocity)

    zoomVelocity' =
      if sign zoomVelocity == sign model.zoomVelocity && (scale > model.scale || scale < model.scale) then
        zoomVelocity
      else
        0

    center =
      model |> findCenter

  in
    if model.zoomVelocity > 0 || model.zoomVelocity < 0 then
    { model | scale = scale
            , zoomVelocity = zoomVelocity'
    }
    |> centerAt (center)
    |> frame
    else model |> frame

animatePan : Viewport -> Viewport
animatePan model =
  { model | offsetBody = model.offsetBody |> Body.step }
  |> frame

translate : (Float,Float) -> Viewport -> Viewport
translate (dx,dy) model =
  let (ox,oy) = model |> offset in
  { model | offsetBody = model.offsetBody |> Body.place (ox+dx,oy+dy) }
  |> frame

frame : Viewport -> Viewport
frame model =
  let
    (ox,oy) =
      model |> offset

    scale = model |> scaleFactor

    (width,height) =
      model.worldDims

    (vWidth,vHeight) =
      model.dimensions

    xMax =
      toFloat width - (toFloat vWidth * scale)

    yMax =
      toFloat height - (toFloat vHeight * scale)

    ox' =
      min 0 (max -xMax ox)

    oy' =
      min 0 (max -yMax oy)
  in
    { model | offsetBody = model.offsetBody |> Body.place (ox', oy') }
