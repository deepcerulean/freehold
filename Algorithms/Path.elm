module Algorithms.Path exposing (Context, init, seek, find, findIncremental, movesFrom)

import Models.Point exposing (Point, Direction)
import Models.Map exposing (Map, Location)

import Dict exposing (Dict)


type alias Path = Maybe (List Location)
type alias SearchMap = Map Direction


type alias Context = { visited : SearchMap
                     , frontier : SearchMap
                     , source : Location
                     , predicate : (Location -> Bool)
                     , moves : (Location -> SearchMap)
                     , depth : Int
                     , path : Path
                     }

init : Location -> Location -> (Location -> Bool) -> Context
init dst src blocked =
    { visited = Dict.empty
    , frontier = Dict.empty
    , source = src
    , predicate = (\pt -> pt == dst)
    , moves = movesFrom blocked
    , depth = 100
    , path = Nothing
    }

seek : Location -> Location -> (Location -> Bool) -> (List Location)
seek dst src blocked =
  if src == dst then [] else
  find dst src blocked
  |> Maybe.withDefault []

movesFrom : (Location -> Bool) ->  Location -> SearchMap
movesFrom blocked point =
  Models.Point.allDirections
    |> List.map (\direction -> (Models.Point.slide direction point, direction))
    |> List.filter ((\p -> not (blocked p)) << fst)
    |> Dict.fromList

find : Location -> Location -> (Location -> Bool) -> Maybe (List Location)
find dst src pred =
  init dst src pred |> find'

find' : Context -> Maybe (List Location)
find' context =
  let {visited, frontier, source, predicate, moves, depth} = context in
  if depth < 0 then
    Nothing
  else
    let context' = context |> findIncremental in
    case context'.path of
      Nothing ->
        find' context'

      Just path' ->
        Just path'

findIncremental : Context -> Context
findIncremental context =
  let
    {visited, frontier, source, predicate, moves, depth} =
      context

    maybeGoal =
      frontier
        |> Dict.keys
        |> List.filter predicate
        |> List.head
  in
    case maybeGoal of
      Just goal ->
        let
          path =
            (constructPath (Dict.union visited frontier) source goal)
              --|> Debug.log "constructed path!!!"
        in
          { context | path = Just (List.reverse path) }

      Nothing ->
        if Dict.isEmpty frontier then
          { context | depth = context.depth - 1
                    , frontier = moves source
          }
            --|> Debug.log "frontier was empty, moving from source"
        else
          let
            (ox,oy) =
              source

            newFrontier =
               (frontier
                |> Dict.keys
                |> List.sortBy (\(px,py) -> -(Models.Point.distance (toFloat ox,toFloat oy) (toFloat px, toFloat py)))
                 -- so this ^^ is subtly 'wrong' -- we need to compute *path* distance so far, which i think does mean tracking depth per-cell?
                 -- maybe we can get there by clever unions/folding but i'm not seeing it
                 -- unfortunately not super-obvs how to do the path-distance comparison here even *if* it was available
                 -- (which doesn't seem like it would be too bad)
                -- todo fix this subtle pathfinding bug
                |> List.map moves
                |> List.foldl Dict.union Dict.empty
                )
            newVisited =
              Dict.union visited frontier

          in
            if Dict.isEmpty frontier then
              context
            else
              { context | visited = newVisited
                        , frontier = (Dict.diff newFrontier newVisited)
                        , depth = (context.depth-1)
              }

constructPath : SearchMap -> Location -> Location -> List Location
constructPath visited source destination =
  let
    isDestination = \pt ->
      pt == destination

    maybeDestination =
      visited
        |> Dict.keys
        |> List.filter isDestination
        |> List.head
  in
     if isDestination source then
       []
     else
       case maybeDestination of
         Nothing ->
           []

         Just point ->
           let
             maybeDirection =
               Dict.get point visited
           in
             case maybeDirection of
               Just direction ->
                 let
                   newDest =
                     point
                     |> Models.Point.slide (Models.Point.invertDirection direction)
                 in
                   [destination] ++ (constructPath visited source newDest)

               Nothing ->
                 []
                   |> Debug.log "construct path could not join up a path!!"
