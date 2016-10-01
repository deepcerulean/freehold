import Algorithms.Conway
import Graphics
import Models.Point exposing (Point, Direction(..))
import Models.World exposing (World)
import Support.Wheel exposing (Delta)
import Views.World
import Viewport exposing (Viewport)

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

-- global config
worldSize = (160,55)

-- type
type Msg = ResizeWindow (Int, Int)
         | Tick Time
         | NewWorld World
         | MoveMouse Mouse.Position
         | Zoom Delta
         | Keypress KeyCode
         | NoOp

type alias Game = { world : World
                  , setup : Bool
                  , hover : Maybe (Int,Int)
                  , viewport : Viewport
                  }

-- MAIN
main =
  App.program
  { init = init worldSize
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- init
init : (Int,Int) -> (Game, Cmd Msg)
init dims =
  (
    { world = Models.World.empty dims
    , setup = False
    , hover = Nothing
    , viewport = Viewport.init dims
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
      ({model | world = world' |> Models.World.evolve 6}, Cmd.none)

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
  let char = Char.fromCode key in
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
  { model | viewport = model.viewport |> Viewport.pan 0.2 dir }

zoom : Delta -> Game -> Game
zoom delta model =
  { model | viewport = model.viewport |> Viewport.zoom delta.y }

resize : (Int,Int) -> Game -> Game
resize dims model =
  { model | viewport = model.viewport |> Viewport.resize dims }

tick : Game -> (Game, Cmd Msg)
tick model =
  { model | viewport = model.viewport |> Viewport.animate }
  |> generate model.world.dimensions

mouseAt : Mouse.Position -> Game -> Game
mouseAt pos model =
  let pos' = model.viewport |> Viewport.find pos in
      { model | hover = Just pos' }

generate : (Int,Int) -> Game -> (Game, Cmd Msg)
generate (w,h) model =
  if model.setup then (model, Cmd.none) else
    ({ model | setup = True }, Random.generate NewWorld (Models.World.generate (w,h)))

-- subs
subscriptions : Game -> Sub Msg
subscriptions model =
  Sub.batch [ Window.resizes sizeToMsg
            , Time.every 20 Tick
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

    offset = model.viewport.offset
  in
    Graphics.view model.viewport worldView
