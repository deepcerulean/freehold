module Extend.List exposing (getAt, zip, uniqueBy)

import Set exposing (Set)

getAt : List a -> Int -> Maybe a
getAt xs idx =
  List.head <| List.drop idx xs

zip : List a -> List b -> List (a,b)
zip =
  List.map2 (,)

-- almost certainly from list.extras
uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f list =
  uniqueHelp f Set.empty list

uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a
uniqueHelp f existing remaining =
  case remaining of
    [] ->
      []

    first :: rest ->
      let computedFirst = f first in
      if Set.member computedFirst existing then
        uniqueHelp f existing rest
      else
        first :: uniqueHelp f (Set.insert computedFirst existing) rest

