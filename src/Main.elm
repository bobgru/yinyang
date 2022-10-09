module Main exposing (..)

import Browser exposing (element)
import Browser.Dom as Dom
import Browser.Events as E
import Element exposing (..)
import Grid as Grid exposing (black, unassigned, white)
import Html exposing (Html)
import Json.Decode as Decode
import Location exposing (..)
import Msg exposing (..)
import Stack exposing (..)
import Task exposing (..)



---- MODEL ----


type alias Model =
    { grid : Grid.Model
    , snapshots : Stack Grid.Model
    , initialCells : Grid.SparseGridInput
    , cellHoveredOver : Maybe Location
    , viewportWidth : Float
    , viewportHeight : Float
    , error : Maybe String
    , undoCmd : Maybe ( Location, Grid.CellColor )
    }


initialCells : Grid.SparseGridInput
initialCells =
    game5


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


initialModel : Model
initialModel =
    { grid = Grid.initialModel initialCells
    , snapshots = Stack.initialise
    , initialCells = initialCells
    , cellHoveredOver = Nothing
    , viewportWidth = 1.0 -- placeholder
    , viewportHeight = 1.0 -- placeholder
    , error = Nothing
    , undoCmd = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, refreshViewport )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowSizeChanged _ _ ->
            ( model, refreshViewport )

        ViewPortChanged viewportResult ->
            updateViewPort model viewportResult

        CharacterKeyPressed c ->
            case ( c, model.undoCmd ) of
                ( 'u', Just ( loc, clr ) ) ->
                    updateCellColor model loc clr
                        |> (\( mdl, _ ) -> highlightCells mdl (Just loc))

                _ ->
                    ( model, Cmd.none )

        ControlKeyPressed keyValue ->
            ( model, Cmd.none )

        CellLeftClicked loc ->
            updateCellColor model loc Grid.black
                |> (\( mdl, _ ) -> highlightCells mdl (Just loc))

        CellRightClicked loc ->
            updateCellColor model loc Grid.white
                |> (\( mdl, _ ) -> highlightCells mdl (Just loc))

        CellHighlighted mloc ->
            highlightCells model mloc


refreshViewport : Cmd Msg
refreshViewport =
    Task.attempt ViewPortChanged Dom.getViewport


updateViewPort model viewportResult =
    case viewportResult of
        Ok viewport ->
            let
                newModel =
                    { model
                        | viewportWidth = viewport.viewport.width
                        , viewportHeight = viewport.viewport.height
                        , error = Nothing
                    }
            in
            ( newModel, Cmd.none )

        Err _ ->
            ( { model | error = Just "no viewport" }, Cmd.none )


updateCellColor : Model -> Location -> Grid.CellColor -> ( Model, Cmd Msg )
updateCellColor model loc clr =
    let
        newModel =
            { model
                | grid = Grid.updateCellColor loc clr model.grid
                , undoCmd = Just ( loc, clr )
            }

        newModel2 =
            { newModel | grid = Grid.highlightedCells newModel.grid (Just loc) }
    in
    ( newModel2, Cmd.none )


highlightCells : Model -> Maybe Location -> ( Model, Cmd Msg )
highlightCells model mloc =
    let
        newModel =
            { model | grid = Grid.highlightedCells model.grid mloc }
    in
    ( newModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Grid.view model.viewportWidth model.viewportHeight model.grid



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ E.onResize WindowSizeChanged
        , E.onKeyPress keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKeyPressed char

        _ ->
            ControlKeyPressed keyValue



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
