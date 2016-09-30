module Viewport exposing (Viewport, init, pan, zoom, resize, find)

import Models.Point exposing (Point)
import Models.Direction exposing (Direction)
import Mouse

type alias Viewport = { dimensions : ( Int, Int )
                      , scale : Float
                      , offset : (Float, Float)
                      , worldDims : ( Int, Int )
                      --, panVelocity : (Float,Float)
                      --, zoomVelocity : Float
                      }

init : (Int,Int) -> Viewport
init worldDims' = { dimensions = (800,600) -- worldDims'
                  , scale = 1.0
                  , offset = (0.0,0.0)
                  , worldDims = worldDims'
                  --, panVelocity = (0.0,0.0)
                  --, zoomVelocity = 0.0
                  }

--friction = 0.1
--
--sign = \n -> if n > 0 then 1 else if n < 0 then -1 else 0
--
--animate : Viewport -> Viewport
--animate model =
--  let
--    (vx,vy) =
--      model.panVelocity
--
--    (ox,oy) =
--      model.offset
--
--    vx' =
--      max 0 vx - (friction * sign vy)
--
--    vy' =
--      max 0 vy - (friction * sign vy)
--  in
--  { model | offset = (ox+vx,oy+vy)
--          , panVelocity = (vx', vy')
--          }

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

pan : Float -> Direction -> Viewport -> Viewport
pan factor' dir model =
  let
    factor =
      factor' / model.scale

    (ox,oy) =
      model.offset

    (dx,dy) =
      Models.Point.delta dir
  in
    { model | offset = (ox + factor * (toFloat dx) , oy + factor * (toFloat dy))
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
      |> scaleTo scale'
      |> centerAt center

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
    |> Debug.log "CENTER"

centerAt : (Int,Int) -> Viewport -> Viewport
centerAt (x,y) model =
  let
    (width,height) =
      model.dimensions
      |> Debug.log "dims"

    scale =
      model |> scaleFactor
      |> Debug.log "scale"

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
