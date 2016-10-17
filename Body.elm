module Body exposing (Body, init, step, applyImpulse, place, constrainVelocity, constrainVelocityX, constrainVelocityY, halt)

sign : Float -> Float
sign x = if x < 0 then -1 else if x > 0 then 1 else 0

--friction = 0.1

type alias Body = { position : (Float, Float)
                  , velocity : (Float, Float)
                  , friction : Float
                  }

init : (Float,Float) -> Float -> Body
init (x,y) friction =
  { position = (x,y)
  , velocity = (0,0)
  , friction = friction
  }

place : (Float,Float) -> Body -> Body
place pos model =
  { model | position = pos }

step : Body -> Body
step body =
  body
    |> move
    |> applyFriction

move : Body -> Body
move model =
  let
    (x,y) =
      model.position

    (vx,vy) =
      model.velocity
  in
    { model | position = (x+vx,y+vy) }

applyImpulse : (Float,Float) -> Body -> Body
applyImpulse (dx,dy) model =
  let (vx,vy) = model.velocity in
  { model | velocity = (vx+dx, vy+dy) }

applyFriction : Body -> Body
applyFriction model =
  let
    (vx,vy) =
      model.velocity

    friction =
      model.friction

    vx' =
      vx - (friction * sign vx)

    vy' =
      vy - (friction * sign vy)

    vx'' =
      if sign vx' == sign vx then vx' else 0

    vy'' =
      if sign vy' == sign vy then vy' else 0

  in
    { model | velocity = (vx'', vy'') }

constrainPosition : (Float,Float) -> (Float,Float) -> Body -> Body
constrainPosition (minX,maxX) (minY,maxY) model =
  let
    (x,y) =
      model.position
  in
    { model | position = ((max minX (min maxX x)), (max minY (min maxY y))) }

constrainVelocity : Float -> Float -> Body -> Body
constrainVelocity maxVx maxVy model =
  model
    |> constrainVelocityX maxVx
    |> constrainVelocityY maxVy
  --let (vx,vy) = model.velocity in
  --    { model | velocity = (min maxVx (max -maxVx vx), min maxVy (max -maxVy vy)) }

constrainVelocityX : Float -> Body -> Body
constrainVelocityX maxVx model =
  let (vx,vy) = model.velocity in
      { model | velocity = (min maxVx (max -maxVx vx), vy) }

constrainVelocityY : Float -> Body -> Body
constrainVelocityY maxVy model =
  let (vx,vy) = model.velocity in
      { model | velocity = (vx, min maxVy (max -maxVy vy)) }

halt : Body -> Body
halt model =
    { model | velocity = (0,0) }
