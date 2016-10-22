module Models.World exposing (World, empty, generate, terraform, isDoneTerraforming, step)

import Algorithms.Conway

import Models.Cartogram exposing (Cartogram)
--import Models.Map exposing (Map, at)
import Models.Person exposing (Person, init)
import Models.Point exposing (Point)
import Models.Terrain exposing (water, dirt, rock, sand, bedrock)
import Models.Thing exposing (Thing)
import Models.Navigator exposing (Navigator)

import Extend.List

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

                   , gatheringSpot : Maybe (Point Int)

                   , navigator : Navigator
                   }

empty : (Int,Int) -> World
empty dims =
  let emptyMap = Models.Cartogram.empty dims in
  { name = "Nowhere"
  , people = []
  , terrain = emptyMap
  , dimensions = dims
  , terraformOver = False
  , nonDirtTerrain = emptyMap
  , things = []
  , steps = 0
  , gatheringSpot = Nothing
  , navigator = Models.Navigator.init (Set.empty)
  }

generate : (Int,Int) -> Generator World
generate (width,height) =
  let mapmaker = (Models.Cartogram.generate (width,height)) in
  Random.map (\terrain' ->
    { name = "Nowhere"
    , people = []
    , things = [ Models.Thing.campfire 1 ( width//2 , height // 2 )
               , Models.Thing.fire 2 ( 3 + (width//2) , 4 + (height // 2) )
               , Models.Thing.wood 3 ( (width//2) - 3 , 1 + (height // 2) )
               ]
    , terrain = terrain'
    , dimensions = (width,height)
    , terraformOver = False
    , nonDirtTerrain = Models.Cartogram.empty (width,height)
    , steps = 0
    , gatheringSpot = Nothing
    , navigator = Models.Navigator.init (Set.empty)
    }) mapmaker

populate : World -> World
populate model =
  let
    initialPop =
      150

    (width,height) =
      model.dimensions

    positions =
      model
        |> findOpenSquaresNear initialPop ( width // 2, height // 2 )

    names =
      Models.Person.names

    people =
      List.map3 (\id name pt -> Models.Person.init id name 25 pt) [0..initialPop] names positions
  in
  { model | people = people }

isWithinDimensions : Point Int -> World -> Bool
isWithinDimensions (x,y) model =
  let (width,height) = model.dimensions in
     x >= 0 && y >= 0 && x < width && y < height

step : Maybe (Point Int) -> Maybe (Point Int) -> World -> World
step hover select model =
  { model | steps = model.steps + 1 }
    |> gatherAt select
    |> navigate
    |> peopleMove
    |> evaluateScripts

evaluateScripts : World -> World
evaluateScripts model =
  model

navigate : World -> World
navigate model =
  { model | navigator = model.navigator |> Models.Navigator.iterate }

gatherAt : Maybe (Point Int) -> World -> World
gatherAt pt model =
  case pt of
    Nothing ->
      model

    Just (x,y) ->
      if (model.gatheringSpot == (Just (x,y))) then model else
      let
          -- todo don't call this all day, it's $$$
        neighbors =
          model
            |> findOpenSquaresNear (model.people |> List.length) (x,y)

        peopleAndSpots =
          Extend.List.zip model.people neighbors

        askDirections = \(person,pt) nav ->
          let src = (Models.Point.asInt person.body.position) in
          nav
            |> Models.Navigator.askWay person.id src pt

        nav' =
          peopleAndSpots
            |> List.foldr askDirections model.navigator

      in
        { model | gatheringSpot = (Just (x,y))
                , navigator = nav'
                , people = model.people |> List.map (Models.Person.clearPath)
        }

findOpenSquaresNear : Int -> Point Int -> World -> List (Point Int)
findOpenSquaresNear n (x,y) model =
  let
    blocked =
      model.nonDirtTerrain
        |> Dict.toList
        |> List.map fst
        |> Set.fromList

    neighbors =
      (x,y)
        |> Models.Point.nearby (25) --+(round (sqrt (toFloat n))))
        |> List.filter (\pt -> not (Set.member pt blocked))
        |> List.sortBy (\pt -> Models.Point.distance (Models.Point.asFloat (x,y)) (Models.Point.asFloat pt))
        |> List.take n
  in
    neighbors

peopleMove : World -> World
peopleMove model =
  { model | people = model.people
                       |> List.map (Models.Person.move model.navigator)
  }


terraform : Int -> World -> World
terraform n model =
  if (n < 0 || model.terraformOver) then model else
    let
      life = { starvation = 9
             , loneliness = 1
             , birth = [2..8]
             }
    in
      { model | terrain = model.terrain
      |> Algorithms.Conway.evolve life 1 (water,bedrock)
      |> Algorithms.Conway.evolve life 1 (rock,bedrock)
      |> Algorithms.Conway.evolve life 1 (sand,bedrock)
      |> Algorithms.Conway.evolve life 1 (dirt,bedrock)
      }
      |> checkDoneTerraforming
      |> terraform (n-1)

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
      { model' | terraformOver = True } |> populate
    else model'

updateNonDirtTerrain : World -> World
updateNonDirtTerrain model =
  let
    newNonDirt =
      model.terrain
        |> Dict.toList
        |> List.filter (\(pt,terrain) -> not (terrain == Models.Terrain.dirt))

    navigator =
      model.navigator

    navigator' =
      { navigator | obstacles = newNonDirt |> List.map fst |> Set.fromList }
  in
  { model | nonDirtTerrain = newNonDirt |> Dict.fromList
          , navigator = navigator'
  }


