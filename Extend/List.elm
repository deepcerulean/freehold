module Extend.List exposing (getAt, zip)

getAt : List a -> Int -> Maybe a
getAt xs idx =
  List.head <| List.drop idx xs

zip : List a -> List b -> List (a,b)
zip =
  List.map2 (,)
