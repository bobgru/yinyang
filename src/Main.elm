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
import Samples
import Stack exposing (..)
import Task exposing (..)



---- MODEL ----


type alias Model =
    { grid : Grid.Model
    , snapshots : List ( Grid.Model, List ( Location, Grid.CellColor ) )
    , initialCells : Grid.SparseGridInput
    , cellHoveredOver : Maybe Location
    , viewportWidth : Float
    , viewportHeight : Float
    , error : Maybe String
    , undoStack : List ( Location, Grid.CellColor )
    , redoStack : List ( Location, Grid.CellColor )
    , showErrors : Bool
    }


initialCells : Grid.SparseGridInput
initialCells =
    Samples.first


initialModel : Model
initialModel =
    { grid = Grid.initialModel initialCells
    , snapshots = []
    , initialCells = initialCells
    , cellHoveredOver = Nothing
    , viewportWidth = 1.0 -- placeholder
    , viewportHeight = 1.0 -- placeholder
    , error = Nothing
    , undoStack = []
    , redoStack = []
    , showErrors = True
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Samples.selectRandomSample )



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
            handleKeyPress c model

        ControlKeyPressed keyValue ->
            ( model, Cmd.none )

        CellLeftClicked loc ->
            updateCellColor model loc Grid.black False
                |> (\( mdl, _ ) -> highlightCells mdl (Just loc))

        CellRightClicked loc ->
            updateCellColor model loc Grid.white False
                |> (\( mdl, _ ) -> highlightCells mdl (Just loc))

        CellHighlighted mloc ->
            highlightCells model mloc

        GotRandomSample i ->
            ( { model
                | grid = Grid.initialModel (Samples.get i)
              }
            , refreshViewport
            )


handleKeyPress : Char -> Model -> ( Model, Cmd Msg )
handleKeyPress c model =
    case c of
        'e' ->
            toggleShowError model

        'u' ->
            handleUndo model

        'r' ->
            handleRedo model

        's' ->
            handlePushSnapshot model

        'p' ->
            handlePopSnapshot model

        _ ->
            ( model, Cmd.none )


handleUndo model =
    case ( model.undoStack, model.redoStack ) of
        ( ( loc, clr ) :: newUndoStack, _ ) ->
            let
                newModel =
                    { model | undoStack = newUndoStack, redoStack = ( loc, clr ) :: model.redoStack }
            in
            updateCellColor newModel loc clr True
                |> (\( mdl, _ ) -> highlightCells mdl (Just loc))

        _ ->
            ( model, Cmd.none )


handleRedo model =
    case ( model.undoStack, model.redoStack ) of
        ( _, ( loc, clr ) :: newRedoStack ) ->
            let
                newModel =
                    { model | redoStack = newRedoStack, undoStack = ( loc, clr ) :: model.undoStack }
            in
            updateCellColor newModel loc clr True
                |> (\( mdl, _ ) -> highlightCells mdl (Just loc))

        _ ->
            ( model, Cmd.none )


handlePushSnapshot model =
    let
        newModel =
            { model | snapshots = ( model.grid, model.undoStack ) :: model.snapshots }
    in
    ( newModel, Cmd.none )


handlePopSnapshot model =
    case model.snapshots of
        ( newGrid, newUndoStack ) :: newSnapshots ->
            let
                newModel =
                    { model
                        | grid = newGrid
                        , undoStack = newUndoStack
                        , snapshots = newSnapshots
                        , redoStack = []
                    }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


toggleShowError model =
    ( { model | showErrors = not model.showErrors }, Cmd.none )


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


updateCellColor : Model -> Location -> Grid.CellColor -> Bool -> ( Model, Cmd Msg )
updateCellColor model loc clr undoOrRedo =
    let
        newUndoStack =
            if undoOrRedo then
                model.undoStack

            else
                ( loc, clr ) :: model.undoStack

        newRedoStack =
            if undoOrRedo then
                model.redoStack

            else
                []

        newModel =
            { model
                | grid = Grid.updateCellColor loc clr model.grid
                , undoStack = newUndoStack
                , redoStack = newRedoStack
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
        Grid.view model.viewportWidth model.viewportHeight model.grid model.showErrors



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
