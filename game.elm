import Graphics
import Models.Point exposing (Point, Direction(..))
import Models.World exposing (World)
import Support.Wheel exposing (Delta)
import Views.World
import Viewport exposing (Viewport)

import Task
import Char
import Random
import Time exposing (Time, millisecond)
import Window
import Mouse
import Keyboard exposing (KeyCode)
import Html exposing (Html)
import Html.App as App

-- global config
worldSize : (Int,Int)
worldSize = (200,120)

framerate : Float
framerate = 60

defaultSpeed : Int
defaultSpeed = 1

-- type
type Msg = ResizeWindow (Int, Int)
         | Tick Time
         | NewWorld World
         | MoveMouse Mouse.Position
         | ClickMouse Mouse.Position
         | Zoom Int
         | Keypress KeyCode
         | NoOp

type alias Game = { world : World
                  , setup : Bool
                  , hover : Maybe (Int, Int)
                  , select : Maybe (Int, Int)
                  , viewport : Viewport
                  , speed : Int
                  , ticks : Int
                  }

-- MAIN
main : Platform.Program Basics.Never
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
    , select = Nothing
    , viewport = Viewport.init dims
    , speed = defaultSpeed
    , ticks = 0
    },
    Task.perform (\_ -> NoOp) sizeToMsg Window.size
  )

mkCmd : a -> Cmd a
mkCmd = Task.perform (Debug.crash << toString) identity << Task.succeed

-- update
update : Msg -> Game -> (Game, Cmd Msg)
update message model =
  case message of
    ResizeWindow dims ->
      (model |> resize dims, Cmd.none)

    NewWorld world' ->
      ({model | world = world'}, Cmd.none) -- |> Models.World.terraform 4 }, Cmd.none)

    Tick _ ->
      model |> tick

    MoveMouse pos ->
      (model |> hoverAt pos, Cmd.none)

    ClickMouse pos ->
      (model |> clickAt pos, Cmd.none)

    Zoom delta ->
      (model |> zoom -delta, Cmd.none)

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
    'a' -> model |> pan East
    'l' -> model |> pan West
    'd' -> model |> pan West
    'j' -> model |> pan North
    's' -> model |> pan North
    'k' -> model |> pan South
    'w' -> model |> pan South
    '0' -> { model | speed = 0 }
    '1' -> { model | speed = 1 }
    '2' -> { model | speed = 2 }
    '3' -> { model | speed = 3 }
    '-' -> model |> zoom 1
    '=' -> model |> zoom -1
    _   -> model

pan : Direction -> Game -> Game
pan dir model =
  { model | viewport = model.viewport |> Viewport.pan 1.0 dir }

zoom : Int -> Game -> Game
zoom delta model =
  { model | viewport = model.viewport |> Viewport.zoom delta }

resize : (Int,Int) -> Game -> Game
resize dims model =
  { model | viewport = model.viewport |> Viewport.resize dims }

tick : Game -> (Game, Cmd Msg)
tick model =
  { model | viewport = model.viewport |> Viewport.animate
          , ticks = model.ticks + 1
  }
  |> evolve (if model.ticks % 2 == 0 then model.speed else 0) --/2)
  |> terraform
  |> generate model.world.dimensions

hoverAt : Mouse.Position -> Game -> Game
hoverAt pos model =
  let pos' = model.viewport |> Viewport.find pos in
      { model | hover = Just pos' }

clickAt : Mouse.Position -> Game -> Game
clickAt pos model =
  let pos' = model.viewport |> Viewport.find pos in
      { model | select = Just pos' }

generate : (Int,Int) -> Game -> (Game, Cmd Msg)
generate (w,h) model =
  if model.setup then (model, Cmd.none) else
    ({ model | setup = True }, Random.generate NewWorld (Models.World.generate (w,h)))
    |> Debug.log "GENERATING NEW WORLD"

terraform : Game -> Game
terraform model =
  if ((not (model.setup)) || (model.world |> Models.World.isDoneTerraforming)) then
    model
  else
    { model | world = model.world |> Models.World.terraform 1 }

evolve : Int -> Game -> Game
evolve n model =
  if n > 0 && (model.setup && (model.world |> Models.World.isDoneTerraforming)) then
    { model | world = model.world |> Models.World.step model.hover model.select }
      |> evolve (n-1)
  else
    model

-- subs
subscriptions : Game -> Sub Msg
subscriptions model =
  Sub.batch [ Window.resizes sizeToMsg
            , Time.every (Time.millisecond * (1000/framerate)) Tick
            , Mouse.moves MoveMouse
            , Mouse.clicks ClickMouse
            , Support.Wheel.xDeltas Zoom
            , Keyboard.presses Keypress
            ]

sizeToMsg : Window.Size -> Msg
sizeToMsg size =
  ResizeWindow (size.width, size.height)

-- view
view : Game -> Html a
view model =
  let
    worldView =
      model.world |> (Views.World.view model.hover model.select)

    offset =
      model.viewport |> Viewport.offset

    infoView =
        [ Graphics.text (10,4) "white" 2.0 ("hover at " ++ (toString model.hover))
        ]

  in
    Graphics.view model.viewport worldView infoView
