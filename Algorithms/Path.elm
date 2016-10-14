module Algorithms.Path exposing (Location, seek, find, findBy)

import Models.Point exposing (Point, Direction)
import Extend.List

type alias Location = Point Int
type alias PathSegment = (Location, Direction)

seek : Location -> Location -> (Location -> Bool) -> (List Location)
seek dst src blocked =
  find dst src (movesFrom blocked)
  |> Maybe.withDefault []
  |> Debug.log "Path#seek"

movesFrom : (Location -> Bool) ->  Location -> List PathSegment
movesFrom blocked point =
  Models.Point.allDirections
  |> List.map (\direction -> (Models.Point.slide direction point, direction))
  |> List.filter ((\p -> not (blocked p)) << fst)

find : Location -> Location -> (Location -> List PathSegment) -> Maybe (List Location)
find dst src moves =
  findBy (\pt -> pt == dst) moves src

findBy : (Location -> Bool) -> (Location -> List PathSegment) -> Location -> Maybe (List Location)
findBy predicate moves source =
  findBy' [] [] source predicate moves 100

findBy' : List PathSegment -> List PathSegment -> Location -> (Location -> Bool) -> (Location -> List PathSegment) -> Int -> Maybe (List Location)
findBy' visited frontier source predicate moves depth =
  if depth < 0 then
    Nothing
  else
    let
      maybeGoal =
        frontier
        |> List.filter (predicate << fst)
        |> List.head
    in
      case maybeGoal of
        Just (goal,_) ->
          let
            path =
              (constructPath (visited ++ frontier) source goal)
          in
            Just (List.reverse path)

        Nothing ->
          if List.length frontier == 0 then
            let frontier' = moves source in
              findBy' visited frontier' source predicate moves (depth-1)
          else
            let
              visitedPositions =
                List.map fst newVisited

              newFrontier =
                frontier
                |> List.concatMap (moves << fst)
                |> List.filter (\(pt,_) -> not (List.member pt visitedPositions))
                |> Extend.List.uniqueBy (Models.Point.code << fst)

              newVisited =
                (visited ++ frontier)
            in
              if List.length frontier > 0 then
                findBy' newVisited (newFrontier) source predicate moves (depth-1)
              else
                Nothing

constructPath : List PathSegment -> Location -> Location -> List Location
constructPath visited source destination =
  let
    isDestination = \pt ->
      pt == destination

    maybeDestination =
      visited
      |> List.filter (isDestination << fst)
      |> List.head
  in
     if isDestination source then
       []
     else
       case maybeDestination of
         Nothing ->
           []

         Just (point, direction) ->
           let
             newDest =
               point
               |> Models.Point.slide (Models.Point.invertDirection direction)
           in
             [destination] ++ (constructPath visited source newDest)
