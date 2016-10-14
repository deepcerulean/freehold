module Algorithms.Conway exposing (evolve)

import Models.Point exposing (Point)
import Models.Map exposing (Map, set, at)

import Dict exposing (Dict)

type alias Life = { starvation : Int
                  , loneliness : Int
                  , birth : List Int
                  }

evolve : Life -> Int -> (a,a) -> Map a -> Map a
evolve life n (alive,dead) model =
  if n < 1 then
    model
  else
    let model' = apply life (alive,dead) model in
    evolve life (n-1) (alive,dead) model'

apply : Life -> (a,a) -> Map a -> Map a
apply life (alive,dead) model =
  let
    pts =
      model |> Dict.keys

    (add, remove) =
      pts
      |> List.foldr (applyAt model life (alive,dead)) ([], [])

    model' =
      add |> List.foldr (set alive) model
  in
    remove |> List.foldr (set dead) model'

matches : Point -> a -> Map a -> Bool
matches pt match model =
  case (model |> at pt) of
    Nothing ->
      False

    Just a' ->
      a' == match

neighborCount : Point -> a -> Map a -> Int
neighborCount pt alive model =
  pt
  |> Models.Point.adjacent
  |> List.filter (\pt -> model |> matches pt alive)
  |> List.length

applyAt : Map a -> Life -> (a,a) -> Point -> (List Point, List Point) -> (List Point, List Point)
applyAt model {starvation,loneliness,birth} (alive,dead) pt (add, remove) =
  let
    neighbors =
      model |> neighborCount pt alive

    living =
      model |> matches pt alive

    born =
      List.member neighbors birth
  in
    if living then
      if neighbors <= loneliness || starvation <= neighbors then
        (add, pt :: remove)
      else
        (add, remove)
    else if born then
      (pt :: add, remove)
    else
      (add, remove)
