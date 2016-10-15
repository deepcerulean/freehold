module Models.World exposing (World, empty, generate, terraform, isDoneTerraforming, step)

import Algorithms.Conway

import Models.Cartogram exposing (Cartogram)
import Models.Map exposing (Map, at)
import Models.Person exposing (Person, init)
import Models.Point exposing (Point)
import Models.Terrain exposing (water, dirt, rock, sand, bedrock)

import Random exposing (Generator)
import Dict
import List

-- type

type alias World = { name : String
                   , people : List Person
                   , terrain : Cartogram
                   , dimensions : (Int, Int)
                   }

empty : (Int,Int) -> World
empty dims =
  { name = "Nowhere"
  , people = []
  , terrain = Models.Cartogram.empty dims
  , dimensions = dims
  }

isWithinDimensions : Point Int -> World -> Bool
isWithinDimensions (x,y) model =
  let (width,height) = model.dimensions in
     x >= 0 && y >= 0 && x < width && y < height

step : Maybe (Point Int) -> Maybe (Point Int) -> World -> World
step hover select model =
  model
    |> peopleFollow select
    |> peopleMove

peopleFollow : Maybe (Point Int) -> World -> World
peopleFollow pt model =
  let
    isBlocked = \pt ->
      if not (model |> isWithinDimensions pt) then
        True
      else
        let
          maybeTerrain =
            model.terrain
              |> Models.Map.at pt
        in
          case maybeTerrain of
            Just terrain ->
              not (terrain == Models.Terrain.dirt || terrain == Models.Terrain.sand)
            Nothing ->
              False

    modify =
      case pt of
        Just (x,y) ->
          Models.Person.findPathTo (x,y) isBlocked

        Nothing ->
          Models.Person.clearPath

  in
    { model | people = model.people
                       |> List.map modify
    }

peopleMove : World -> World
peopleMove model =
  { model | people = model.people |> List.map (Models.Person.move) }

generate : (Int,Int) -> Generator World
generate (width,height) =
  let mapmaker = (Models.Cartogram.generate (width,height)) in
  Random.map (\terrain' ->
    { name = "Nowhere"
    , people = [ Models.Person.init (width//2, height//2) "Gorn" 25 ]
    , terrain = terrain'
    , dimensions = (width,height)
    }
    ) mapmaker

terraform : Int -> World -> World
terraform n model =
  if n < 0 then model else
    let
      life = { starvation = 9
             , loneliness = 1
             , birth = [2..8]
             }
        |> Debug.log "terraform params"
    in
      { model | terrain = model.terrain
      |> Algorithms.Conway.evolve life 1 (water,bedrock)
      |> Algorithms.Conway.evolve life 1 (rock,bedrock)
      |> Algorithms.Conway.evolve life 1 (sand,bedrock)
      |> Algorithms.Conway.evolve life 1 (dirt,bedrock)
      }
      |> terraform (n-1)

isDoneTerraforming : World -> Bool
isDoneTerraforming model =
  let
      bedrockCount =
        model.terrain
          |> Dict.values
          |> List.filter (\t -> t == Models.Terrain.bedrock)
          |> List.length

  in
      not (bedrockCount > 20)
      --|> Debug.log "done terraform?"
