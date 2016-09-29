module Models.Person exposing (Person, init)

import String

-- type
type alias Person = { name : String
                    , age : Int
                    }

-- init
init : String -> Int -> Person
init name age =
  { name = name
  , age = age
  }
