import Algorithms.Conway
import Graphics exposing (viewbox)
import Models.Direction exposing (Direction(..))
import Models.Point exposing (Point)
import Models.Terrain exposing (water, dirt)
import Models.World exposing (World)
import Support.Wheel exposing (Delta)
import Views.World

import Task
import String
import Char
import Random
import Time exposing (Time, millisecond)
import Window
import Mouse
import Keyboard exposing (KeyCode)
import Html exposing (Html)
import Html.App as App

-- type
type Msg = ResizeWindow (Int, Int)
         | Tick Time
         | NewWorld World
         | MoveMouse Mouse.Position
         | Zoom Delta
         | Keypress KeyCode
         | NoOp

type alias Game = { world : World
                  , dims : (Int, Int)
                  , setup : Bool
                  , hover : Maybe (Int,Int)
                  , scale : Float
                  , offset : Point
                  }

-- MAIN
main =
  App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- init
init : (Game, Cmd Msg)
init =
  (
    { world = Models.World.empty
    , dims = (0,0) -- will be overridden with Window.size
    , setup = False
    , hover = Nothing
    , scale = 1.0
    , offset = (0,0)
    },
    Task.perform (\_ -> NoOp) sizeToMsg Window.size
  )

-- update
update : Msg -> Game -> (Game, Cmd Msg)
update message model =
  case message of
    ResizeWindow dims ->
      (model |> resize dims, Cmd.none)

    NewWorld world' ->
      ({model | world = world' |> Models.World.evolve 4}, Cmd.none)

    Tick _ ->
      model |> tick

    MoveMouse pos ->
      (model |> mouseAt pos, Cmd.none)

    Zoom delta ->
      (model |> zoom delta, Cmd.none)

    Keypress key ->
      (model |> press key, Cmd.none)

    NoOp ->
      (model, Cmd.none)

press : KeyCode -> Game -> Game
press key model =
  let char = Char.fromCode key |> Debug.log "key" in
  model |> parse char

parse : Char -> Game -> Game
parse char model =
  case char of
    'h' -> model |> pan East
    'l' -> model |> pan West
    'j' -> model |> pan North
    'k' -> model |> pan South
    _  -> model

pan : Direction -> Game -> Game
pan dir model =
  { model | offset = model.offset |> Models.Point.slide dir }

zoom : Delta -> Game -> Game
zoom delta model =
  let dy = 0.001 * (toFloat delta.y) in
  { model | scale = max 1.0 (model.scale + dy) }

resize : (Int,Int) -> Game -> Game
resize dims' model =
  { model | dims = dims' }

tick : Game -> (Game, Cmd Msg)
tick model =
  model
  |> generate (30,20)

mouseAt : Mouse.Position -> Game -> Game
mouseAt pos model =
  let pos' = model |> screenToCoord pos in
      { model | hover = Just pos' }

screenToCoord : Mouse.Position -> Game -> (Int,Int)
screenToCoord pos model =
  let
    aspectRatio =
      (toFloat height) / (toFloat width)

    (vWidth,vHeight) =
      model.dims

    (width,height) =
      model.world.dimensions

    (x,y) =
      (toFloat (pos.x), toFloat (pos.y)) -- - 10), toFloat (pos.y - 10))

    scale' =
      (if (toFloat vHeight / toFloat vWidth) < aspectRatio then
        (toFloat height / toFloat vHeight)
      else
        (toFloat width / toFloat vWidth)
      )

    scale =
      scale' / ( model.scale )

    (offsetX, offsetY) =
      model.offset
  in
      (round ((x * scale) - 0.5) - offsetX, round ((y * scale) - 0.5) - offsetY)
      |> Debug.log "pos"

generate : (Int,Int) -> Game -> (Game, Cmd Msg)
generate (w,h) model =
  if model.setup then (model, Cmd.none) else
    ({ model | setup = True }, Random.generate NewWorld (Models.World.generate (w,h)))
    |> Debug.log "SETUP"

-- subs
subscriptions : Game -> Sub Msg
subscriptions model =
  Sub.batch [ Window.resizes sizeToMsg
            , Time.every 100 Tick
            , Mouse.moves MoveMouse
            , Support.Wheel.deltas Zoom
            , Keyboard.presses Keypress
            ]

sizeToMsg : Window.Size -> Msg
sizeToMsg size =
  ResizeWindow (size.width, size.height)

-- view
view model =
  let
    worldView =
      Views.World.view model.hover model.world
  in
    Graphics.viewbox model.dims model.world.dimensions model.scale model.offset worldView

--frame model =

