module Extend.List exposing
  ( getAt
  , zip
  , minBy
  , andThen
  )

getAt : List a -> Int -> Maybe a
getAt xs idx =
  List.head <| List.drop idx xs

zip : List a -> List b -> List (a,b)
zip =
  List.map2 (,)

-- helpers from list extras
minBy : (a -> comparable) -> List a -> Maybe a
minBy f ls =
  let minBy x (y, fy) = let fx = f x in if fx < fy then (x, fx) else (y, fy)
  in case ls of
        [l']    -> Just l'
        l'::ls' -> Just <| fst <| List.foldl minBy (l', f l') ls'
        _       -> Nothing

andThen : List a -> (a -> List b) -> List b
andThen = flip List.concatMap
