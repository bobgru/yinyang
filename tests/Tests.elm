module Tests exposing (..)

import Expect
import Grid as Grid
import Location exposing (Location)
import Set as Set
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "getConnectedCells horizontally" <|
            \_ ->
                let
                    game : Grid.SparseGridInput
                    game =
                        { width = 2
                        , height = 2
                        , cells =
                            [ ( ( 0, 0 ), Grid.white )
                            , ( ( 0, 1 ), Grid.white )
                            ]
                        }

                    sparse =
                        Grid.initialModel game

                    expected : Set.Set Location
                    expected =
                        Set.fromList [ ( 0, 0 ), ( 0, 1 ) ]
                in
                Expect.equal expected (Grid.getConnectedCells (Grid.matchColor Grid.white) sparse.grid.cells ( 0, 0 ))
        , test "getConnectedCells vertically" <|
            \_ ->
                let
                    game : Grid.SparseGridInput
                    game =
                        { width = 2
                        , height = 2
                        , cells =
                            [ ( ( 0, 0 ), Grid.white )
                            , ( ( 1, 0 ), Grid.white )
                            ]
                        }

                    sparse =
                        Grid.initialModel game

                    expected : Set.Set Location
                    expected =
                        Set.fromList [ ( 0, 0 ), ( 1, 0 ) ]
                in
                Expect.equal expected (Grid.getConnectedCells (Grid.matchColor Grid.white) sparse.grid.cells ( 0, 0 ))
        , test "getConnectedCells not diagonally, and not if singleton" <|
            \_ ->
                let
                    game : Grid.SparseGridInput
                    game =
                        { width = 2
                        , height = 2
                        , cells =
                            [ ( ( 0, 0 ), Grid.white )
                            , ( ( 1, 1 ), Grid.white )
                            ]
                        }

                    sparse =
                        Grid.initialModel game

                    expected : Set.Set Location
                    expected =
                        Set.empty
                in
                Expect.equal expected (Grid.getConnectedCells (Grid.matchColor Grid.white) sparse.grid.cells ( 0, 0 ))
        , test "getConnectedCells vertically and horizontally" <|
            \_ ->
                let
                    game : Grid.SparseGridInput
                    game =
                        { width = 4
                        , height = 4
                        , cells =
                            [ ( ( 0, 0 ), Grid.white )
                            , ( ( 0, 1 ), Grid.white )
                            , ( ( 0, 2 ), Grid.white )
                            , ( ( 0, 3 ), Grid.white )
                            , ( ( 1, 0 ), Grid.white )
                            , ( ( 1, 3 ), Grid.white )
                            , ( ( 2, 0 ), Grid.white )
                            , ( ( 3, 0 ), Grid.white )
                            , ( ( 3, 1 ), Grid.white )
                            ]
                        }

                    sparse =
                        Grid.initialModel game

                    expected : Set.Set Location
                    expected =
                        Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 1, 0 ), ( 1, 3 ), ( 2, 0 ), ( 3, 0 ), ( 3, 1 ) ]
                in
                Expect.equal expected (Grid.getConnectedCells (Grid.matchColor Grid.white) sparse.grid.cells ( 0, 0 ))
        , test "getConnectedComplements when no errors" <|
            \_ ->
                let
                    game : Grid.SparseGridInput
                    game =
                        { width = 4
                        , height = 4
                        , cells =
                            [ ( ( 0, 0 ), Grid.white )
                            , ( ( 0, 1 ), Grid.white )
                            , ( ( 0, 2 ), Grid.white )
                            , ( ( 0, 3 ), Grid.white )
                            , ( ( 1, 0 ), Grid.white )
                            , ( ( 1, 3 ), Grid.white )
                            , ( ( 2, 0 ), Grid.white )
                            , ( ( 3, 0 ), Grid.white )
                            , ( ( 3, 1 ), Grid.white )
                            ]
                        }

                    sparse =
                        Grid.initialModel game

                    connectedCells : Set.Set Location
                    connectedCells =
                        Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 1, 0 ), ( 1, 3 ), ( 2, 0 ), ( 3, 0 ), ( 3, 1 ) ]

                    expected : List (Set.Set Location)
                    expected =
                        [ Set.fromList [ ( 1, 1 ), ( 1, 2 ), ( 2, 1 ), ( 2, 2 ), ( 2, 3 ), ( 3, 2 ), ( 3, 3 ) ] ]
                in
                Expect.equal expected (Grid.getConnectedComplements connectedCells sparse.grid)
        , test "getConnectedComplements when checkerboard formed" <|
            \_ ->
                let
                    game : Grid.SparseGridInput
                    game =
                        { width = 4
                        , height = 4
                        , cells =
                            [ ( ( 0, 0 ), Grid.white )
                            , ( ( 0, 1 ), Grid.white )
                            , ( ( 0, 2 ), Grid.white )
                            , ( ( 0, 3 ), Grid.white )
                            , ( ( 1, 0 ), Grid.white )
                            , ( ( 1, 3 ), Grid.white )
                            , ( ( 2, 0 ), Grid.white )
                            , ( ( 2, 3 ), Grid.white )
                            , ( ( 3, 0 ), Grid.white )
                            , ( ( 3, 1 ), Grid.white )
                            , ( ( 3, 2 ), Grid.white )
                            ]
                        }

                    sparse =
                        Grid.initialModel game

                    connectedCells : Set.Set Location
                    connectedCells =
                        Set.fromList
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 0, 3 )
                            , ( 1, 0 )
                            , ( 1, 3 )
                            , ( 2, 0 )
                            , ( 2, 3 )
                            , ( 3, 0 )
                            , ( 3, 1 )
                            , ( 3, 2 )
                            ]

                    expected : List (Set.Set Location)
                    expected =
                        [ Set.fromList [ ( 3, 3 ) ]
                        , Set.fromList [ ( 1, 1 ), ( 1, 2 ), ( 2, 1 ), ( 2, 2 ) ]
                        ]
                in
                Expect.equal expected (Debug.log "result" Grid.getConnectedComplements connectedCells sparse.grid)
        ]
