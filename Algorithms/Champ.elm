module Algorithms.Champ exposing (champernowne)

import Extend.List exposing (getAt)

-- binary champernowne
champernowne : Int -> Bool
champernowne n =
  getAt champ n
  |> Maybe.withDefault False

champLimit = 1000

champ : List Bool
champ =
  List.concatMap toBools [1..champLimit]

toBools : Int -> List Bool
toBools n =
  let
    lsb =
      if n % 2 == 0 then
        [False]
      else
        [True]
  in
    if n < 2 then
      lsb
    else
      (toBools (n//2)) ++ lsb
