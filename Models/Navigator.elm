module Models.Navigator exposing (Navigator
                                 , init
                                 , askWay
                                 , pathFor
                                 , iterate
                                 )

import Algorithms.Path exposing (Context)
import Models.Point exposing (Point)
import Dict exposing (Dict)
import Set exposing (Set)

-- person id -> context
type alias Navigator = { obstacles : Set (Point Int)
                       , paths : Dict Int Context
                       }

init : Set (Point Int) -> Navigator
init obstacles' =
    { obstacles = obstacles'
    , paths = Dict.empty
    }

-- register intention to search for path with nav coordinator
askWay : Int -> Point Int -> Point Int -> Navigator -> Navigator
askWay callerId src dst model =
  let
    path =
      (Algorithms.Path.init dst src model.obstacles)

    paths' =
      model.paths
        |> Dict.insert callerId path
  in
    { model | paths = paths' }

-- callers will have to poll
pathFor : Int -> Navigator -> Maybe (List (Point Int))
pathFor callerId model =
  let
    paths =
      model.paths

    callerContext =
      paths
        |> Dict.get callerId

  in
    case callerContext of
      Nothing ->
        Nothing -- []

      Just ctx ->
        ctx.path
          --|> Debug.log ("Found path for caller " ++ (toString callerId))

-- open requests, sorted by distance
pathsByDistance : Navigator -> List (Int, Context)
pathsByDistance model =
  model.paths
    |> Dict.toList
    |> List.filter (\(_, ctx) -> (ctx.path) == Nothing)
    |> List.sortBy (\(_, ctx) -> ctx |> Algorithms.Path.manhattanDistance)

--workRemaining : Navigator -> Bool
--workRemaining model =
--  model.paths
--    |> Dict.toList
--    |> List.any (\(_, ctx) -> ctx.path == Nothing)

-- world will have to iterate -- we will choose to explore one path at a time
-- proximate goal is to iterate only longest AND shortest path
-- (two lanes)
iterate : Int -> Navigator -> Navigator
iterate n model =
  model
    |> iterateCaller (shortestPath model)
    |> iterateCaller (longestPath model)

shortestPath : Navigator -> Maybe (Int, Context)
shortestPath model =
  let paths' = model |> pathsByDistance in
  paths' |> List.head

longestPath : Navigator -> Maybe (Int, Context)
longestPath model =
  let paths' = model |> pathsByDistance |> List.reverse in
  paths' |> List.head

iterateCaller : Maybe (Int, Context) -> Navigator -> Navigator
iterateCaller caller model =
  case caller of
    Nothing ->
      model

    Just (callerId,path) ->
      let
        path' =
          path |> Algorithms.Path.findIncremental 1

        tail =
          Dict.remove callerId model.paths

        updatedPaths =
          Dict.insert callerId path' tail
      in
        { model | paths = updatedPaths }
