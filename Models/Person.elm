module Models.Person exposing (Person, init, move, findPathTo, clearPath)

import Algorithms.Path
import Models.Point exposing (Point)
import Body exposing (Body)

-- type
type alias Person = { name : String
                    , age : Int
                    , body : Body
                    , goal : Maybe (Point Int)
                    , path : List (Point Int)
                    }

-- init
init : Point Int -> String -> Int -> Person
init (x,y) name age =
  { name = name
  , age = age
  , body = Body.init (0.5 + (toFloat x), 0.5 + (toFloat y)) 0
  , goal = Nothing
  , path = []
  }

clearPath : Person -> Person
clearPath model =
  { model | path = [] }

findPathTo : Point Int -> (Point Int -> Bool) -> Person -> Person
findPathTo target blocked model =
  case model.path |> List.reverse |> List.head of
    Nothing -> -- path is empty
      model
        |> Debug.log "path is empty..." --, do not seek path?"
        |> seekPath target blocked

    Just currentTarget ->
      if (target |> Debug.log "new target") == (currentTarget |> Debug.log "current target") then
        model
          |> Debug.log "path already exists, keep current path"
      else
        model
          |> Debug.log "path already exists, but not to endpoint -- recompute"
          |> seekPath target blocked


seekPath : Point Int -> (Point Int -> Bool) -> Person -> Person
seekPath target blocked model =
  let
    (bx,by) =
      model.body.position

    bodyPos =
      (round (bx - 0.5), round (by - 0.5))
  in
    if (blocked target) || (blocked bodyPos) then
      { model | path = [] }
    else
      let
        path' =
          Algorithms.Path.seek bodyPos target blocked
            |> List.reverse
      in
        if List.length path' == 0 then
          { model | path = [] }
          |> Debug.log "no path..."
        else
          { model | path = path' ++ [target] }
          |> Debug.log "found path!!!"

setGoal : Point Int -> Person -> Person
setGoal pt model =
  { model | goal = Just pt }
  |> Debug.log "set goal"

removeGoal : Person -> Person
removeGoal model =
  { model | goal = Nothing }
  |> Debug.log "remove goal!!!"

move : Person -> Person
move model =
  model
    |> updateBody
    |> followGoal
    |> followPath

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
    |> Debug.log "advance path"

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
        |> Debug.log "follow direction"

    (dx,dy) =
      Models.Point.delta direction
        |> Debug.log "follow delta"

    impulse =
      0.03

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
