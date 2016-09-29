import Algorithms.Conway --exposing (evolve)
import Models.World exposing (World)
import Models.Terrain exposing (water, dirt)
import Views.World
import Graphics exposing (viewbox)
import Support.Wheel exposing (Delta)

import Html exposing (Html)
import Html.App as App

import String
import Time exposing (Time, millisecond)
import Task
import Window
import Mouse
import Random

-- type
type Msg = ResizeWindow (Int, Int)
         | Tick Time
         | NewWorld World
         | MoveMouse Mouse.Position
         | Zoom Delta
         | NoOp

type alias Game = { world : World
                  , dims : (Int, Int)
                  , setup : Bool
                  , hover : Maybe (Int,Int)
                  , scale : Float
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
    , dims = (-1,-1)
    , setup = False
    , hover = Nothing
    , scale = 1.0
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

    NoOp ->
      (model, Cmd.none)


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

    scale =
      (if (toFloat vHeight / toFloat vWidth) < aspectRatio then
        (toFloat height / toFloat vHeight)
        --|> Debug.log "scale"
      else
        (toFloat width / toFloat vWidth)
      ) / ( model.scale )
        --|> Debug.log "scale"
  in
      (round ((x * scale) - 0.5), round ((y * scale) - 0.5))
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
    Graphics.viewbox model.dims model.world.dimensions model.scale worldView
