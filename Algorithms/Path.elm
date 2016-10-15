module Algorithms.Path exposing (Context, SearchMap, init, seek, find, findIncremental, movesFrom)

import Models.Point exposing (Point, Direction(..))
import Models.Map exposing (Map, Location)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Path = Maybe (List Location)
type alias SearchData = (Float, Direction)
type alias SearchMap = Map SearchData --Direction

type alias Context = { visited : SearchMap
                     , frontier : SearchMap
                     , source : Location
                     , dest : Location
                     , blocked : Set Location
                     , moves : (Float -> Location -> SearchMap)
                     , depth : Int
                     , path : Path
                     }

-- todo:
--   [ ] don't search off the map
--   [ ] check zones before searching
--   [ ] hierarchical 'region' analysis for speeding up long paths!

-- it's really a graph...

--type alias Region = { position : Location
--                    , width : Int
--                    , height : Int
--                    , connections : List Connection
--                    }
-- type alias Connection = { adjacentRegion : Region, location : Location }
--
--type alias Analysis = { zones: List (Set Location)
--                      , regions: List (Region)
--                      }

maxDepth : Int
maxDepth = 10000 -- no more than 10k cells searched...
                 -- we may need hierarchical srch!!
                 -- and island/continent is still a prob too, at least a quick 'zones' analysis

init : Location -> Location -> Set Location -> Context --(Location -> Bool) -> Context
init dst src blocked' =
    { visited = Dict.insert src (-1,Northeast) Dict.empty
    , frontier = Dict.empty
    , source = src
    , dest = dst
    --, predicate = (\pt -> pt == dst)
    , moves = movesFrom blocked'
    , depth = maxDepth
    , blocked = blocked'
    , path = Nothing
    }

seek : Location -> Location -> Set Location -> (List Location)
seek dst src blocked =
  if src == dst then [] else
  find dst src blocked
  |> Maybe.withDefault []

movesFrom : Set Location -> Float -> Location -> SearchMap
movesFrom blocked steps point =
  Models.Point.allDirections
    |> List.map (\direction ->
      (Models.Point.slide direction point, (steps + (Models.Point.unitLength direction), direction)))
    |> List.filter (\(p,_) -> not (Set.member p blocked))
    |> Dict.fromList

find : Location -> Location -> Set Location -> Maybe (List Location)
find dst src pred =
  init dst src pred |> find'

find' : Context -> Maybe (List Location)
find' context =
  if context.depth < 0 then
    Nothing
  else
    let context' = context |> findIncremental 10 in
    case context'.path of
      Nothing ->
        find' context'

      Just path' ->
        Just path'

findIncremental : Int -> Context -> Context
findIncremental n context =
  if n < 1 then context else
  let
    {visited, frontier, source, dest, moves, depth} =
      context

    maybeGoal =
      frontier
        |> Dict.keys
        |> List.filter (\pt -> pt == dest)
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
                    , frontier = moves 0 source
          }
        else
          context
            |> extendFrontier --100
            |> findIncremental (n-1)

extendFrontier : Context -> Context
extendFrontier context =
  --if n < 1 then context else
  let
    {visited,frontier,moves,dest, source} =
      context

    pick =
      frontier
      |> Dict.toList
      |> List.sortBy (\(pt,(n,dir)) -> n/2 + ((Models.Point.distance (Models.Point.asFloat pt) (Models.Point.asFloat dest))/2)) -- + ((Models.Point.distance (Models.Point.asFloat pt) (Models.Point.asFloat source))/2))
      |> List.head
      |> Maybe.withDefault ((-1,-1),(-1,Northeast))

    (pickPoint,pickValue) =
      pick

    (pickSteps,pickDirection) =
      pickValue

    newVisited =
      visited
        |> Dict.insert pickPoint pickValue

    movesFromPick =
      moves pickSteps pickPoint
        --|> Dict.filter (\((px,py),_) -> px > 0 && py > 0)

    newFrontier =
      (Dict.diff movesFromPick visited)
      |> Dict.union (frontier |> Dict.remove pickPoint)

  in
    { context | visited = newVisited
              , frontier = newFrontier
              , depth = (context.depth-1)
    } -- |> extendFrontier (n-1)

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
               Just (_,direction) ->
                 let
                   newDest =
                     point
                     |> Models.Point.slide (Models.Point.invertDirection direction)
                 in
                   [destination] ++ (constructPath visited source newDest)

               Nothing ->
                 []
                   |> Debug.log "construct path could not join up a path!!"
