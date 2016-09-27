import Models.World exposing (World)

import Html exposing (Html)
import Html.App as App
import Html.Attributes exposing (style)
import Svg exposing (svg, rect, text')
import Svg.Attributes exposing (viewBox, width, height, x, y, fontSize, textAnchor, fill)
import Svg.Events
import Window
import Task

-- type
type Msg = ResizeWindow (Int, Int)
         | NoOp

type alias Game = { world : World
                  , width : Int
                  , height : Int
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
    { world = Models.World.init
    , width = 100
    , height = 100
    },
    --Cmd.none
    Task.perform (\_ -> NoOp) sizeToMsg Window.size
  )


-- update
update : Msg -> Game -> (Game, Cmd Msg)
update message model =
  case message of
    ResizeWindow (width', height') ->
      ({ model | width = width', height = height' }, Cmd.none)
    NoOp ->
      (model, Cmd.none)

-- subs
subscriptions : Game -> Sub Msg
subscriptions model =
  Sub.batch [
    Window.resizes sizeToMsg
  ]

sizeToMsg : Window.Size -> Msg
sizeToMsg size =
  ResizeWindow (size.width, size.height)

-- view
view model =
  let
    message = "HELLO WORLD"
  in
    Html.div [ style [("background-color", "none")] ]
    [
      svg [ viewBox "0 0 100 100", style [("height", (toString model.height)),("width", (toString model.width))] ] [
          text' [ x "50"
                , y "50"
                , fill "blue"
                , fontSize "1pt"
                , textAnchor "middle"
                ] [ Html.text message ]
        ]
    ]
