module Tests exposing (..)

import Expect
import Grid as Grid
import Location exposing (Location)
import Set as Set
import Test exposing (..)
import Tree as Tree



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ describe "getConnectedCells"
            [ test "horizontally" <|
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
            , test "vertically" <|
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
            , test "not diagonally, and not if singleton" <|
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
            , test "vertically and horizontally" <|
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
            ]
        , describe "getConnectedComplements"
            [ test "getConnectedComplements when no errors" <|
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
        , describe "Trees"
            [ test "singleton" <|
                \_ ->
                    let
                        expected : Tree.Tree Location
                        expected =
                            Tree.Leaf ( 0, 0 )
                    in
                    Expect.equal expected (Tree.singleton ( 0, 0 ))
            , test "with children" <|
                \_ ->
                    let
                        expected : Tree.Tree Location
                        expected =
                            Tree.Node ( 0, 0 ) [ Tree.Leaf ( 0, 1 ) ]
                    in
                    Expect.equal expected (Tree.tree ( 0, 0 ) [ Tree.singleton ( 0, 1 ) ])
            , test "fromLocationSet" <|
                \_ ->
                    let
                        locs : Set.Set Location
                        locs =
                            Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 1 ) ]

                        expected : Tree.Tree Location
                        expected =
                            Tree.Node ( 0, 0 ) [ Tree.Node ( 0, 1 ) [ Tree.Leaf ( 1, 1 ), Tree.Leaf ( 0, 2 ) ] ]
                    in
                    Expect.equal expected (Debug.log "Tree" <| Tree.fromLocationSet ( 0, 0 ) locs)
            , test "toPath" <|
                \_ ->
                    let
                        locs : Set.Set Location
                        locs =
                            Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 1 ) ]

                        tree =
                            Tree.fromLocationSet ( 0, 0 ) locs

                        expected : List Location
                        expected =
                            [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ), ( 0, 1 ), ( 0, 2 ), ( 0, 1 ), ( 0, 0 ) ]
                    in
                    Expect.equal expected (Tree.toPath tree)
            , test "toPolylinePoints" <|
                \_ ->
                    let
                        locs : Set.Set Location
                        locs =
                            Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 1 ) ]

                        tree =
                            Tree.fromLocationSet ( 0, 0 ) locs

                        expected : String
                        expected =
                            "0,0 0,1 1,1 0,1 0,2 0,1 0,0"
                    in
                    Expect.equal expected (Tree.toPolylinePoints tree)
            ]
        ]
