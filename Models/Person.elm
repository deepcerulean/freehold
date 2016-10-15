module Models.Person exposing (Person, init, move, findPathTo, clearPath)

import Algorithms.Path
import Models.Point exposing (Point)
import Body exposing (Body)

type Activity = Idle
              | Walking
              | Pathfinding

-- type
type alias Person = { name : String
                    , age : Int
                    , body : Body
                    , goal : Maybe (Point Int)
                    , path : List (Point Int)
                    , pathfinding : Maybe (Algorithms.Path.Context)
                    , ticks : Int
                    }

-- init
init : Point Int -> String -> Int -> Person
init (x,y) name age =
  { name = name
  , age = age
  , body = Body.init (0.5 + (toFloat x), 0.5 + (toFloat y)) 0
  , goal = Nothing
  , path = []
  , pathfinding = Nothing
  , ticks = 0
  }

clearPath : Person -> Person
clearPath model =
  { model | path = [] }

findPathTo : Point Int -> (Point Int -> Bool) -> Person -> Person
findPathTo target blocked model =
  let
    (bx,by) =
      model.body.position
        --|> Debug.log "position"

    bodyPos =
      (round (bx - 0.5), round (by - 0.5))
  in
    if (blocked target) || (blocked bodyPos) || (target == bodyPos) then
      { model | path = []
              , pathfinding = Nothing
      }
    else
      case model.pathfinding of
        Nothing ->
          case model.path |> List.reverse |> List.head of
            Nothing ->
              --model
              --if List.length model.path == 0 then
              { model | pathfinding = Just (Algorithms.Path.init target bodyPos blocked)
                      , path = []
              }
              --  |> Debug.log "no path so overriding?"

            Just pt ->
              if pt == target then
                model -- we already have a path..?
              else
                { model | pathfinding = Just (Algorithms.Path.init target bodyPos blocked)
                        , path = []
                }
                |> Debug.log "overriding existing pathfinding context, new target identified"

        Just ctx ->
          model

findPaths : Person -> Person
findPaths model =
  case model.pathfinding of
    Nothing ->
      model

    Just context ->
      if context.depth < 0 then
        { model | path = []
                , pathfinding = Nothing
        }
      else
        let
          context' = context
            |> Algorithms.Path.findIncremental
        in
          case context'.path of
            Nothing ->
              { model | pathfinding = Just context' }
              |> Debug.log "increment pathfinding..."
              --{ model | pathfinding = Just (context') -- |> Algorithms.Path.findIncremental)
              --        , path = []
              --} |> Debug.log "start finding path..."

            Just path' ->
              { model | path = path'
                      , pathfinding = Nothing
              }

setGoal : Point Int -> Person -> Person
setGoal pt model =
  { model | goal = Just pt }

removeGoal : Person -> Person
removeGoal model =
  { model | goal = Nothing }

move : Person -> Person
move model =
  { model | ticks = model.ticks + 1 }
    |> updateBody
    |> findPaths
    |> followPath
    |> followGoal

followGoal : Person -> Person
followGoal model =
  case model.goal of
    Nothing ->
      model
    Just goal' ->
      model
        |> follow goal'
        |> clearGoalIfReached goal'

followPath : Person -> Person
followPath model =
  if List.length model.path == 0 then
    model
  else
    model
      --|> advancePath
      |> pickGoalFromPath
      |> advancePathIfNeeded

clearGoalIfReached : Point Int -> Person -> Person
clearGoalIfReached pt model =
  if (distanceTo pt model) < 0.1 then
    model
      |> removeGoal
  else
    model

distanceTo : Point Int -> Person -> Float
distanceTo (px,py) model =
  let
    target =
      (0.5 + (toFloat px), 0.5+(toFloat py))
  in
    Models.Point.distance target model.body.position

pickGoalFromPath : Person -> Person
pickGoalFromPath model =
  { model | goal = model.path |> List.head }

advancePathIfNeeded : Person -> Person
advancePathIfNeeded model =
  case model.path |> List.head of
    Just nextStep ->
      if (model |> distanceTo nextStep) < 1.35 then
        model
          |> advancePath
      else
        model
    Nothing ->
      model

advancePath : Person -> Person
advancePath model =
  { model | path = model.path |> List.drop 1 }

updateBody : Person -> Person
updateBody model =
  { model | body = model.body |> Body.step }

-- follow needs to adjust goal based on path...?
follow : Point Int -> Person -> Person
follow (x,y) model =
  let
    (px,py) =
      model.body.position

    roundedPos =
      (round px, round py)

    direction =
      Models.Point.towards model.body.position ((toFloat x)+0.5, (toFloat y) + 0.5)

    (dx,dy) =
      Models.Point.delta direction

    impulse =
      0.05

    distance =
      Models.Point.distance model.body.position ((toFloat x)+0.5, (toFloat y) + 0.5)

    distX =
      abs (px - ((toFloat x)+0.5))

    distY =
      abs (py - ((toFloat y)+0.5))

    maxImpulse =
      0.25

    maxImpulseX =
      if distX > 0.1 then
        maxImpulse
      else
        0

    maxImpulseY =
      if distY > 0.1 then
        maxImpulse
      else
        0

    body' =
      if distance < 0.15 then --(maxImpulse * 1.2) then
        model.body
          |> Body.place ((toFloat x)+0.5, (toFloat y) + 0.5)
          |> Body.halt

      else
      --if (round (px-0.5), round (py-0.5)) == (x,y) then
      --  model.body
      --    |> Body.halt
      --else
        model.body
        |> Body.applyImpulse (toFloat -(dx)*impulse, toFloat -(dy)*impulse)
        |> Body.constrainVelocity maxImpulseX maxImpulseY
  in
      { model | body = body' }
