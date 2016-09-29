module Views.Terrain exposing (view)

import Models.Terrain exposing (Terrain, color)
import Models.Point exposing (Point)

import Graphics exposing (rect)

view pt terrain =
  rect pt (color terrain)
