module Models.Person exposing (Person, init, move)

--import Algorithms.Path
import Models.Point exposing (Point)
import Body exposing (Body)
import Models.Navigator exposing (Navigator)

-- type
type alias Person = { name : String
                    , id : Int
                    , age : Int
                    , body : Body
                    , goal : Maybe (Point Int)
                    , path : List (Point Int)
                    , ticks : Int
                    }

-- init
init : Int -> String -> Int -> Point Int -> Person
init id name age (x,y) =
  { id = id
  , name = name
  , age = age
  , body = Body.init (0.5 + (toFloat x), 0.5 + (toFloat y)) 0
  , goal = Nothing
  , path = []
  , ticks = id
  }

findPaths : Person -> Person
findPaths model =
  model

setGoal : Point Int -> Person -> Person
setGoal pt model =
  { model | goal = Just pt }

removeGoal : Person -> Person
removeGoal model =
  { model | goal = Nothing }

move : Navigator -> Person -> Person
move nav model =
  { model | ticks = model.ticks + 1 }
    |> checkPath nav
    |> updateBody
    |> followPath
    |> followGoal

checkPath : Navigator -> Person -> Person
checkPath nav model =
  if not (model.path == []) then model else
  --case model.path |> List.reverse |> List.head of
  --  Nothing ->
  --    model
  --  Just currentPathTarget ->
  case nav |> Models.Navigator.pathFor model.id of
    Nothing ->
      model -- |> Debug.log ("No path available for person " ++ (toString model.id))
    Just path' ->
      case path' |> List.reverse |> List.head of
        Nothing ->
          model

        Just pathTarget ->
          if (model |> distanceTo pathTarget) < 1.35 then
            model
          else
            { model | path = path' } --|> Debug.log ("Found path for " ++ (toString model.id))

          --if model.path == [] then
          --else model

  -- if List.length model.path > 0 then model else

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
      |> pickGoalFromPath
      |> advancePathIfNeeded

clearGoalIfReached : Point Int -> Person -> Person
clearGoalIfReached pt model =
  if (distanceTo pt model) < 0.1 then
    model
      |> removeGoal
      --|> Debug.log "removed goal"
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
    --|> Debug.log "picking goal from path"

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
