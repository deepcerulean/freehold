module Algorithms.Conway exposing (evolve)

import Models.Point exposing (Point)
import Models.Map exposing (Map, set, at)

import Dict exposing (Dict)

evolve : Int -> (a,a) -> Map a -> Map a
evolve n (alive,dead) model =
  if n < 1 then
    model
  else
    let model' = apply (alive,dead) model in
    evolve (n-1) (alive,dead) model'

apply : (a,a) -> Map a -> Map a
apply (alive,dead) model =
  let
    pts =
      model |> Dict.keys

    (add, remove) =
      pts
      |> List.foldr (applyAt model (alive,dead)) ([], [])

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

applyAt : Map a -> (a,a) -> Point -> (List Point, List Point) -> (List Point, List Point)
applyAt model (alive,dead) pt (add, remove) =
  let
    neighbors =
      model |> neighborCount pt alive

    living =
      model |> matches pt alive

    starvation =
      8

    loneliness =
      2

    birth =
      [3..9]

    born =
      List.member neighbors birth
  in
    if living then
      if neighbors < loneliness || starvation < neighbors then
        (add, pt :: remove)
      else
        (add, remove)
    else if born then
      (pt :: add, remove)
    else
      (add, remove)
