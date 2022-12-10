module Samples exposing (..)

import Grid exposing (black, white)
import Msg exposing (..)
import Random


type alias NEList a =
    ( a, List a )


samples : NEList Grid.SparseGridInput
samples =
    ( game1
    , [ game2
      , game3
      , game4
      , game5
      , game6
      , game7
      ]
    )


numSamples : Int
numSamples =
    List.length (Tuple.second samples) + 1


first : Grid.SparseGridInput
first =
    Tuple.first samples


get : Int -> Grid.SparseGridInput
get i =
    case modBy numSamples i of
        0 ->
            first

        j ->
            List.drop (j - 1) (Tuple.second samples) |> List.head |> Maybe.withDefault first


selectRandomSample : Cmd Msg
selectRandomSample =
    Random.generate GotRandomSample (Random.int 0 (numSamples - 1))


game1 : Grid.SparseGridInput
game1 =
    { width = 10
    , height = 10
    , cells =
        [ ( ( 1, 2 ), white )
        , ( ( 1, 5 ), white )
        , ( ( 2, 3 ), white )
        , ( ( 2, 6 ), white )
        , ( ( 2, 8 ), black )
        , ( ( 4, 2 ), white )
        , ( ( 4, 5 ), white )
        , ( ( 4, 7 ), white )
        , ( ( 5, 1 ), black )
        , ( ( 6, 4 ), white )
        , ( ( 6, 6 ), white )
        , ( ( 7, 1 ), black )
        , ( ( 7, 8 ), white )
        , ( ( 8, 0 ), black )
        , ( ( 8, 4 ), white )
        , ( ( 8, 6 ), black )
        , ( ( 9, 0 ), white )
        , ( ( 9, 1 ), white )
        ]
    }


game2 : Grid.SparseGridInput
game2 =
    { width = 10
    , height = 10
    , cells =
        [ ( ( 1, 8 ), white )
        , ( ( 2, 6 ), white )
        , ( ( 2, 7 ), white )
        , ( ( 3, 2 ), white )
        , ( ( 3, 4 ), white )
        , ( ( 4, 1 ), white )
        , ( ( 4, 2 ), white )
        , ( ( 4, 6 ), white )
        , ( ( 4, 8 ), white )
        , ( ( 5, 3 ), white )
        , ( ( 6, 5 ), white )
        , ( ( 7, 3 ), white )
        , ( ( 8, 2 ), white )
        , ( ( 8, 4 ), white )
        , ( ( 8, 9 ), white )
        , ( ( 9, 8 ), white )
        ]
    }


game3 : Grid.SparseGridInput
game3 =
    { width = 10
    , height = 10
    , cells =
        [ ( ( 1, 4 ), white )
        , ( ( 1, 6 ), white )
        , ( ( 1, 7 ), white )
        , ( ( 1, 8 ), black )
        , ( ( 2, 2 ), white )
        , ( ( 2, 4 ), black )
        , ( ( 2, 5 ), white )
        , ( ( 2, 8 ), black )
        , ( ( 3, 2 ), black )
        , ( ( 3, 5 ), white )
        , ( ( 3, 8 ), white )
        , ( ( 4, 2 ), black )
        , ( ( 4, 3 ), white )
        , ( ( 4, 4 ), white )
        , ( ( 4, 6 ), black )
        , ( ( 4, 7 ), white )
        , ( ( 5, 2 ), black )
        , ( ( 5, 7 ), black )
        , ( ( 6, 1 ), white )
        , ( ( 6, 3 ), black )
        , ( ( 6, 5 ), black )
        , ( ( 7, 1 ), black )
        , ( ( 7, 5 ), white )
        , ( ( 7, 8 ), white )
        , ( ( 8, 1 ), white )
        , ( ( 8, 2 ), white )
        , ( ( 8, 3 ), white )
        , ( ( 8, 5 ), white )
        ]
    }


game4 : Grid.SparseGridInput
game4 =
    { width = 10
    , height = 10
    , cells =
        [ ( ( 0, 5 ), white )
        , ( ( 0, 8 ), white )
        , ( ( 1, 1 ), white )
        , ( ( 1, 2 ), white )
        , ( ( 1, 4 ), white )
        , ( ( 1, 7 ), white )
        , ( ( 2, 1 ), white )
        , ( ( 2, 9 ), white )
        , ( ( 3, 2 ), white )
        , ( ( 3, 3 ), white )
        , ( ( 3, 8 ), white )
        , ( ( 4, 1 ), black )
        , ( ( 4, 2 ), white )
        , ( ( 4, 6 ), white )
        , ( ( 5, 1 ), white )
        , ( ( 5, 4 ), black )
        , ( ( 5, 9 ), white )
        , ( ( 6, 2 ), white )
        , ( ( 6, 4 ), white )
        , ( ( 6, 7 ), white )
        , ( ( 6, 8 ), white )
        , ( ( 7, 1 ), black )
        , ( ( 7, 6 ), white )
        , ( ( 8, 0 ), white )
        , ( ( 8, 3 ), white )
        , ( ( 8, 4 ), black )
        , ( ( 8, 8 ), black )
        , ( ( 9, 3 ), white )
        ]
    }


game5 : Grid.SparseGridInput
game5 =
    { width = 8
    , height = 8
    , cells =
        [ ( ( 0, 6 ), black )
        , ( ( 0, 7 ), white )
        , ( ( 1, 1 ), white )
        , ( ( 1, 2 ), white )
        , ( ( 1, 4 ), black )
        , ( ( 1, 6 ), black )
        , ( ( 2, 6 ), black )
        , ( ( 2, 7 ), white )
        , ( ( 3, 1 ), white )
        , ( ( 3, 3 ), black )
        , ( ( 3, 4 ), white )
        , ( ( 3, 5 ), black )
        , ( ( 4, 0 ), black )
        , ( ( 4, 1 ), white )
        , ( ( 4, 2 ), white )
        , ( ( 4, 6 ), black )
        , ( ( 4, 6 ), black )
        , ( ( 5, 0 ), black )
        , ( ( 5, 4 ), black )
        , ( ( 5, 5 ), white )
        , ( ( 6, 1 ), white )
        , ( ( 6, 2 ), black )
        , ( ( 6, 4 ), black )
        , ( ( 6, 6 ), black )
        , ( ( 7, 4 ), black )
        , ( ( 7, 7 ), black )
        ]
    }


game6 : Grid.SparseGridInput
game6 =
    { width = 9
    , height = 11
    , cells =
        [ ( ( 1, 4 ), white )
        , ( ( 2, 2 ), white )
        , ( ( 2, 4 ), white )
        , ( ( 2, 6 ), white )
        , ( ( 3, 3 ), white )
        , ( ( 3, 5 ), white )
        , ( ( 4, 3 ), black )
        , ( ( 5, 1 ), white )
        , ( ( 5, 3 ), white )
        , ( ( 5, 4 ), black )
        , ( ( 5, 6 ), white )
        , ( ( 5, 7 ), white )
        , ( ( 6, 3 ), black )
        , ( ( 7, 3 ), white )
        , ( ( 7, 5 ), white )
        , ( ( 8, 2 ), white )
        , ( ( 8, 4 ), white )
        , ( ( 9, 2 ), black )
        , ( ( 9, 4 ), white )
        , ( ( 9, 6 ), black )
        , ( ( 10, 0 ), white )
        , ( ( 10, 1 ), black )
        ]
    }


game7 : Grid.SparseGridInput
game7 =
    { width = 9
    , height = 11
    , cells =
        [ ( ( 1, 4 ), black )
        , ( ( 2, 2 ), white )
        , ( ( 2, 3 ), black )
        , ( ( 2, 5 ), black )
        , ( ( 2, 6 ), white )
        , ( ( 3, 2 ), white )
        , ( ( 3, 3 ), black )
        , ( ( 3, 5 ), black )
        , ( ( 3, 6 ), white )
        , ( ( 4, 1 ), white )
        , ( ( 4, 3 ), black )
        , ( ( 4, 5 ), black )
        , ( ( 4, 7 ), white )
        , ( ( 5, 1 ), black )
        , ( ( 5, 6 ), black )
        , ( ( 6, 1 ), white )
        , ( ( 6, 3 ), black )
        , ( ( 6, 5 ), black )
        , ( ( 7, 2 ), black )
        , ( ( 7, 6 ), black )
        , ( ( 8, 3 ), black )
        , ( ( 8, 5 ), black )
        , ( ( 9, 2 ), black )
        , ( ( 9, 6 ), black )
        , ( ( 10, 3 ), black )
        ]
    }
