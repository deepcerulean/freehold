module Models.Thing exposing (Thing(..), Entity, Kind(..), init, fire, wood, campfire, isAtomic, name, location)

import Models.Point exposing (Point)

-- base 'material' categories (atomic objects/substances that can't be "split" further...)
type Kind = Fire
          | Wood
          --| Head
          --| Body

type alias Entity = { name : String
                    , id : Int
                    , location : Point Int -- todo should really be a body...
                    , kind : Kind
                    }

type Thing = Atom Entity
           | Molecule String (Point Int) (List Thing)

-- ctors

init : Int -> Point Int -> Kind -> Thing
init id pt kind =
  Atom { kind = kind
       , id = id
       , location = pt
       , name = kind |> describe'
       }

fire : Int -> Point Int -> Thing
fire id pt =
  init id pt Fire


wood : Int -> Point Int -> Thing
wood id pt =
  init id pt Wood

campfire : Int -> Point Int -> Thing
campfire id pt =
  Molecule "campfire" pt (List.map (init id pt) [ Wood, Fire ])

-- helpers
isAtomic : Thing -> Bool
isAtomic model =
  case model of
    Atom _ ->
      True
    _ ->
      False

parts : Thing -> (List Thing)
parts model =
  case model of
    Atom _ ->
      []

    Molecule _ _ parts' ->
      parts'

name : Thing -> String
name model =
  case model of
    Atom entity ->
      entity.name

    Molecule name _ _ ->
      name

location : Thing -> Point Int
location model =
  case model of
    Atom entity ->
      entity.location

    Molecule _ pt _ ->
      pt

-- private helpers
describe' : Kind -> String
describe' kind =
  case kind of
    Fire ->
      "fire"

    Wood ->
      "wood"
