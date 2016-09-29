module Views.Terrain exposing (view, hover)

import Models.Terrain exposing (Terrain, color, hoverColor)
import Models.Point exposing (Point)

import Graphics exposing (rect)

view pt terrain =
  rect pt (color terrain)

hover pt terrain =
  rect pt (hoverColor terrain)
