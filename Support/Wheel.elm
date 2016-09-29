--
-- taken from here: https://github.com/epeterson320/wheel/blob/master/src/Wheel.elm
-- (does not seem to appear on elm-package??)

effect module Support.Wheel where { subscription = MySub } exposing
  ( Delta
  , delta
  , deltas
  , xDeltas
  )

{-| This library provides access to mouse wheel events, to use for
something other than scrolling, like:
* zooming in a 3D app
* panning in a maps app.
If you just want the mouse wheel to scroll a regular page, you don't
need this.
# Wheel Events
@docs Delta, delta
# Subscriptions
@docs deltas, xDeltas
-}
import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json exposing ((:=))
import Task exposing (Task)
import Process

-- API

{-| The amount scrolled by a wheel event, in pixels, vertically and
horizontally.
-}
type alias Delta =
  { x : Int
  , y : Int
  }

{-| The decoder used to extract a `Delta` from a JavaScript wheel
event.
    view : Model -> Html Wheel.Delta
    view model =
      div
        [ on "wheel" Wheel.delta ]
        [ text "Scroll to zoom" ]
-}
delta : Json.Decoder Delta
delta =
  Json.object2 Delta
    ("deltaX" := Json.int)
    ("deltaY" := Json.int)

{-| Subscribe to wheel events anywhere on the screen.
    subscriptions : Model -> Sub Msg
    subscriptions model =
      Sub.batch
        [ Mouse.clicks Click
        , Wheel.deltas Zoom
        ]
-}
deltas : (Delta -> msg) -> Sub msg
deltas tagger =
  subscription (MySub tagger)

{-| Subscribe to only the vertical component of wheel events anywhere
on the screen.
-}
xDeltas : (Int -> msg) -> Sub msg
xDeltas tagger =
  deltas .x |> Sub.map tagger


-- SUBSCRIPTIONS

type MySub msg
  = MySub (Delta -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub tagger) =
  MySub (tagger >> func)



-- EFFECT MANAGER

type alias State msg =
  Maybe
    { subs : List (MySub msg)
    , pid : Process.Id
    }


init : Task Never (State msg)
init = Task.succeed Nothing

(&>) t1 t2 = t1 `Task.andThen` \_ -> t2

onEffects : Platform.Router msg Delta -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
  case (oldState, newSubs) of
    (Nothing, []) ->
      Task.succeed Nothing

    (Just {pid}, []) ->
      Process.kill pid
        &> Task.succeed Nothing

    (Nothing, _) ->
      Process.spawn (Dom.onWindow "wheel" delta (Platform.sendToSelf router))
        `Task.andThen` \pid ->

      Task.succeed (Just { subs = newSubs, pid = pid })

    (Just {pid}, _) ->
      Task.succeed (Just { subs = newSubs, pid = pid })


onSelfMsg : Platform.Router msg Delta -> Delta -> State msg -> Task Never (State msg)
onSelfMsg router delta state =
  case state of
    Nothing ->
      Task.succeed state

    Just {subs} ->
      let
        send (MySub tagger) =
          Platform.sendToApp router (tagger delta)
      in
        Task.sequence (List.map send subs)
          &> Task.succeed state
