module Models.World exposing (World, empty, generate, terraform, isDoneTerraforming, step)

import Algorithms.Conway

import Models.Cartogram exposing (Cartogram)
--import Models.Map exposing (Map, at)
import Models.Person exposing (Person, init)
import Models.Point exposing (Point)
import Models.Terrain exposing (water, dirt, rock, sand, bedrock)
import Models.Thing exposing (Thing)

import Random exposing (Generator)
import Dict
import Set
import List
--import Svg

-- type

type alias World = { name : String
                   , people : List Person
                   , terrain : Cartogram
                   , dimensions : (Int, Int)
                   , terraformOver : Bool

                   , nonDirtTerrain : Cartogram
                   , things : List Thing
                   , steps : Int
                   --, viewModel : Maybe (Map Color)
                   }

empty : (Int,Int) -> World
empty dims =
  { name = "Nowhere"
  , people = []
  , terrain = Models.Cartogram.empty dims
  , dimensions = dims
  , terraformOver = False
  , nonDirtTerrain = Models.Cartogram.empty dims
  , things = []
  , steps = 0
  --, viewModel = Nothing
  }

generate : (Int,Int) -> Generator World
generate (width,height) =
  let mapmaker = (Models.Cartogram.generate (width,height)) in
  Random.map (\terrain' ->
    { name = "Nowhere"
    , people = [ Models.Person.init 1 (1 + width//2, height//2) "Gorn" 25
               , Models.Person.init 2 (2 + width//2, 2 + height // 2) "Sally" 24
               , Models.Person.init 3 (1 + width//2, 1 + height // 2) "Bourne" 24
               , Models.Person.init 4 (1 + width//2, 3 + height // 2) "Teela" 24
               , Models.Person.init 5 ( width//2, 1 + height // 2) "Oryn" 24
               ]
    , things = [ Models.Thing.campfire 1 ( width//2 , height // 2 )
               , Models.Thing.fire 2 ( width//2 , height // 2 )
               ]
    , terrain = terrain'
    , dimensions = (width,height)
    , terraformOver = False
    , nonDirtTerrain = Models.Cartogram.empty (width,height)
    , steps = 0
    --, viewModel = Nothing
    }
    ) mapmaker

isWithinDimensions : Point Int -> World -> Bool
isWithinDimensions (x,y) model =
  let (width,height) = model.dimensions in
     x >= 0 && y >= 0 && x < width && y < height

step : Maybe (Point Int) -> Maybe (Point Int) -> World -> World
step hover select model =
  { model | steps = model.steps + 1 }
    |> peopleFollow select
    |> peopleMove

peopleFollow : Maybe (Point Int) -> World -> World
peopleFollow pt model =
  case pt of
    Nothing ->
      model
    Just (x,y) ->
      let
        neighbors =
          model
            |> findOpenSquaresNear (x,y) (model.people |> List.length)

        blocked =
          model.nonDirtTerrain
            |> Dict.toList
            |> List.map fst
            |> Set.fromList

        modify = \pt ->
          Models.Person.findPathTo pt blocked

      in
        { model | people = model.people
                           |> List.map2 modify neighbors
        }

findOpenSquaresNear : Point Int -> Int -> World -> List (Point Int)
findOpenSquaresNear (x,y) n model =
  let
    neighbors =
      (x,y)
        |> Models.Point.adjacent
  in
    neighbors
      |> List.take n


peopleMove : World -> World
peopleMove model =
  { model | people = model.people
                       |> List.map (Models.Person.move)
  }


terraform : Int -> World -> World
terraform n model =
  if (n < 0 || model.terraformOver) then model else
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
      |> checkDoneTerraforming

isDoneTerraforming : World -> Bool
isDoneTerraforming model =
  model.terraformOver

checkDoneTerraforming : World -> World
checkDoneTerraforming model =
  let
    bedrockCount =
      model.terrain
        |> Dict.values
        |> List.filter (\t -> t == Models.Terrain.bedrock)
        |> List.length

    model' =
      model |> updateNonDirtTerrain

  in
    if not (bedrockCount > 20) then
      { model' | terraformOver = True }
    else model'
      --|> Debug.log "done terraform?"

updateNonDirtTerrain : World -> World
updateNonDirtTerrain model =
  { model | nonDirtTerrain = model.terrain
                             |> Dict.toList
                             |> List.filter (\(pt,terrain) -> not (terrain == Models.Terrain.dirt))
                             |> Dict.fromList
                             |> Debug.log "setting non dirt terrain"
                             }
