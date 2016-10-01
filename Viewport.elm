module Viewport exposing (Viewport, init, pan, zoom, resize, find, animate)

import Models.Point exposing (Point, Direction)
import Mouse



sign x = if x < 0 then -1 else if x > 0 then 1 else 0

type alias Viewport = { dimensions : ( Int, Int )
                      , scale : Float
                      , offset : (Float, Float)
                      , worldDims : ( Int, Int )
                      , panVelocity : (Float,Float)
                      , zoomVelocity : Float
                      }

init : (Int,Int) -> Viewport
init worldDims' = { dimensions = (800,600)
                  , scale = 1.0
                  , offset = (0.0,0.0)
                  , worldDims = worldDims'
                  , panVelocity = (0,0)
                  , zoomVelocity = 0
                  }

find : Mouse.Position -> Viewport -> (Int,Int)
find pos model =
  let
    (x,y) =
      (toFloat (pos.x), toFloat (pos.y))

    scale =
      model |> scaleFactor

    (ox, oy) =
      model.offset
  in
      (round (((x) * scale) - (0.5 + ox)), round (((y) * scale) - (0.5 + oy)))

maxPanSpeed = 1.0

pan : Float -> Direction -> Viewport -> Viewport
pan factor' dir model =
  let
    factor =
      factor' -- / model.scale

    (vx,vy) =
      model.panVelocity

    (dx,dy) =
      Models.Point.delta dir

    vx' =
      vx + factor * (toFloat dx)

    vy' =
      vy + factor * (toFloat dy)

    vx'' =
      if abs vx' > maxPanSpeed then maxPanSpeed * sign vx' else vx'

    vy'' =
      if abs vy' > maxPanSpeed then maxPanSpeed * sign vy' else vy'
  in
    { model | panVelocity = (vx'', vy'')
    }

zoom : Int -> Viewport -> Viewport
zoom delta model =
  let
    dy =
      0.15 * (toFloat delta)

    --scale' =
    --  max 1.0 (model.scale - dy)

    --center =
    --  model |> findCenter
  in
    model
      --|> freezeScroll
      |> accelerateZoomVelocity dy --scale'
      --|> scaleTo scale'
      --|> centerAt center

maxZoomVelocity = 0.8
accelerateZoomVelocity factor model =
  let
    zoomVelocity =
      if abs (model.zoomVelocity + factor) > maxZoomVelocity then
        maxZoomVelocity * sign model.zoomVelocity
      else
        model.zoomVelocity + factor
  in
  { model | zoomVelocity = zoomVelocity }

freezeScroll model =
  { model | panVelocity = (0,0) }

scaleTo scale' model =
  { model | scale = scale' }

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
      model.offset

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
    { model | offset = (ox - 0.5,oy - 0.5) }

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

zoomFriction = 0.05
minZoom = 1
maxZoom = 4
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
    else model

friction = 0.0175 -- pan fric
animatePan : Viewport -> Viewport
animatePan model =
  let
    {offset,panVelocity} =
      model

    (vx,vy) =
      panVelocity
  in
    if abs vx > 0 || abs vy > 0 then
      let
        (ox,oy) =
          offset


        vx' =
          vx - (friction * sign vx)

        vy' =
          vy - (friction * sign vy)

        vx'' =
          if sign vx' == sign vx then vx' else 0

        vy'' =
          if sign vy' == sign vy then vy' else 0
      in
        { model | panVelocity = (vx'',vy'') }
        |> translate (vx'',vy'')
      else model

translate : (Float,Float) -> Viewport -> Viewport
translate (dx,dy) model =
  let (ox,oy) = model.offset in
  { model | offset = (ox+dx,oy+dy) }
