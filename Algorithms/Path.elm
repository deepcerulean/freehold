module Algorithms.Path exposing (Context, SearchMap, init, seek, find, findIncremental, movesFrom, manhattanDistance)

import Models.Point exposing (Point, Direction(..))
import Models.Map exposing (Map, Location)

import Dict exposing (Dict)
import Set exposing (Set)

type alias Path = Maybe (List Location)
type alias SearchData = (Float, Direction)
type alias SearchMap = Map SearchData

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
--   [ ] search at most 2 paths at once (nav manager)
--   [ ] don't search off the map
--   [ ] check zones before searching
--   [ ] hierarchical analysis for speeding up long paths
--   [ ] nav mesh?? smoothing??

maxDepth : Int
maxDepth =
  10000

init : Location -> Location -> Set Location -> Context
init dst src blocked' =
    { visited = Dict.insert src (-1,Northeast) Dict.empty
    , frontier = Dict.empty
    , source = src
    , dest = dst
    , moves = movesFrom blocked'
    , depth = maxDepth
    , blocked = blocked'
    , path = Nothing
    }

manhattanDistance : Context -> Int
manhattanDistance model =
  let
    (ax,ay) =
      model.source

    (bx,by) =
      model.dest
  in
    abs (ay - by) + abs (ax - bx)

seek : Location -> Location -> Set Location -> (List Location)
seek dst src blocked =
  if src == dst then [] else
  find dst src blocked
  |> Maybe.withDefault []

movesFrom : Set Location -> Float -> Location -> SearchMap
movesFrom blocked steps point =
  let dirToEntry = \direction -> (Models.Point.slide direction point, (steps + (Models.Point.unitLength direction), direction))
  in
  Models.Point.allDirections
    |> List.map dirToEntry
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
  if not (context.path == Nothing) then context else
  --if n < 1 then context else
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
            --|> findIncremental (n-1)

extendFrontier : Context -> Context
extendFrontier context =
  --if n < 1 then context else
  let
    {visited,frontier,moves,dest, source} =
      context

    (gx,gy) = dest

    pick =
      frontier
      |> Dict.toList
      |> List.sortBy (\((x,y),(n,dir)) ->
        (toFloat ((y - gy)^2 + (x - gx)^2)) + n
        --n +
        --(Models.Point.distance (Models.Point.asFloat pt) (Models.Point.asFloat dest)) -- +
        --(Models.Point.distance (Models.Point.asFloat source) (Models.Point.asFloat pt))
      )
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
