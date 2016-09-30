module Viewport exposing (Viewport, init, pan, zoom, resize, find, animate)

import Models.Point exposing (Point)
import Models.Direction exposing (Direction)
import Mouse

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
      0.05 * (toFloat delta)

    scale' =
      max 1.0 (model.scale - dy)

    center =
      model |> findCenter
  in
    model
      |> freezeScroll
      |> scaleTo scale'
      |> centerAt center

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



-- init : (Int,Int) -> Viewport
-- init worldDims' = { dimensions = (800,600)
--                   , scale = 1.0
--                   , offset = (0.0,0.0)
--                   , worldDims = worldDims'
--                   , panVelocity = (0,0)
--                   , zoomVelocity = 0
--                   }

friction = 0.06
sign x = if x < 0 then -1 else if x > 0 then 1 else 0

animate : Viewport -> Viewport
animate model =
  let
    {offset,panVelocity} =
      model

    (ox,oy) =
      offset

    (vx,vy) =
      panVelocity

    vx' =
      vx - (friction * sign vx)

    vy' =
      vy - (friction * sign vy)

    vx'' =
      if sign vx' == sign vx then vx' else 0

    vy'' =
      if sign vy' == sign vy then vy' else 0
  in
    { model | offset = (ox+vx'',oy+vy'')
            , panVelocity = (vx'',vy'')
    }
