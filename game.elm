import Algorithms.Conway --exposing (evolve)
import Models.World exposing (World)
import Models.Terrain exposing (water, dirt)
import Views.World
import Graphics exposing (viewbox)

import Html exposing (Html)
import Html.App as App

import String
import Time exposing (Time, millisecond)
import Task
import Window
import Random

-- type
type Msg = ResizeWindow (Int, Int)
         | Tick Time
         | NewWorld World
         | NoOp

type alias Game = { world : World
                  , dims : (Int, Int)
                  , setup : Bool
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

    NoOp ->
      (model, Cmd.none)

    Tick _ ->
      model |> tick

resize : (Int,Int) -> Game -> Game
resize dims' model =
  { model | dims = dims' }

tick : Game -> (Game, Cmd Msg)
tick model =
  model
  |> generate

generate : Game -> (Game, Cmd Msg)
generate model =
  if model.setup then (model, Cmd.none) else
    ({ model | setup = True }, Random.generate NewWorld (Models.World.generate (80,60)))
    |> Debug.log "SETUP"

-- subs
subscriptions : Game -> Sub Msg
subscriptions model =
  Sub.batch [ Window.resizes sizeToMsg
            , Time.every 100 Tick
            ]

sizeToMsg : Window.Size -> Msg
sizeToMsg size =
  ResizeWindow (size.width, size.height)

-- view
view model =
  let
    worldView =
      Views.World.view model.world
  in
    Graphics.viewbox model.dims model.world.dimensions worldView
