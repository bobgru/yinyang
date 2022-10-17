module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Element exposing (..)
import Grid as Grid
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode
import Location exposing (..)
import Msg exposing (..)
import Samples
import Stack exposing (..)
import Task exposing (..)



---- MODEL ----


type Model
    = Loading Config
    | Running GameState



-- Accumulate the starting configuration until we have all of it,
-- then we can switch to Running.


type alias Config =
    { sampleGameIndex : Maybe Int
    , viewportWidth : Maybe Float
    , viewportHeight : Maybe Float
    , error : Maybe String
    }


type alias FullConfig =
    { sampleGameIndex : Int
    , viewportWidth : Float
    , viewportHeight : Float
    }


type alias GameState =
    { grid : Grid.Model
    , selectedSample : Int
    , viewportWidth : Float
    , viewportHeight : Float
    , error : Maybe String
    , snapshots : List ( Grid.Model, List ( Location, Grid.CellColor ) )
    , undoStack : List ( Location, Grid.CellColor )
    , redoStack : List ( Location, Grid.CellColor )
    , cellHoveredOver : Maybe Location
    , showErrors : Bool
    , showWins : Bool
    }


initialConfig : Config
initialConfig =
    { sampleGameIndex = Nothing
    , viewportHeight = Nothing
    , viewportWidth = Nothing
    , error = Nothing
    }


initialGameState : FullConfig -> GameState
initialGameState config =
    { grid = Grid.initialModel (Samples.get config.sampleGameIndex)
    , selectedSample = config.sampleGameIndex
    , viewportWidth = config.viewportWidth
    , viewportHeight = config.viewportHeight
    , error = Nothing
    , snapshots = []
    , undoStack = []
    , redoStack = []
    , cellHoveredOver = Nothing
    , showErrors = True
    , showWins = True
    }


init : ( Model, Cmd Msg )
init =
    ( Loading initialConfig, Samples.selectRandomSample )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowSizeChanged _ _ ->
            ( model, refreshViewport )

        ViewPortChanged viewportResult ->
            ( updateViewPort viewportResult model, Cmd.none )

        CharacterKeyPressed c ->
            ( handleKeyPress c model, Cmd.none )

        ControlKeyPressed _ ->
            ( model, Cmd.none )

        CellLeftClicked loc ->
            ( cellClicked loc Grid.black model, Cmd.none )

        CellRightClicked loc ->
            ( cellClicked loc Grid.white model, Cmd.none )

        CellHighlighted mloc ->
            case model of
                Loading _ ->
                    ( model, Cmd.none )

                Running state ->
                    ( Running <| highlightCells mloc state, Cmd.none )

        GotRandomSample i ->
            ( setSample i model, refreshViewport )


cellClicked : Location -> Grid.CellColor -> Model -> Model
cellClicked loc clr model =
    case model of
        Loading _ ->
            model

        Running state ->
            let
                newState =
                    updateCellColor loc clr False state
                        |> highlightCells (Just loc)
            in
            Running newState


setSample : Int -> Model -> Model
setSample i model =
    case model of
        Loading config ->
            Loading { config | sampleGameIndex = Just i }

        Running state ->
            Running
                { state
                    | selectedSample = i
                    , grid = Grid.initialModel (Samples.get i)
                }


handleKeyPress : Char -> Model -> Model
handleKeyPress c model =
    case model of
        Loading _ ->
            model

        Running state ->
            let
                newState =
                    case c of
                        'w' ->
                            toggleShowWins state

                        'e' ->
                            toggleShowErrors state

                        'u' ->
                            handleUndo state

                        'r' ->
                            handleRedo state

                        's' ->
                            handlePushSnapshot state

                        'p' ->
                            handlePopSnapshot state

                        _ ->
                            state
            in
            Running newState


handleUndo : GameState -> GameState
handleUndo state =
    case ( state.undoStack, state.redoStack ) of
        ( ( loc, clr ) :: newUndoStack, _ ) ->
            let
                newState =
                    { state | undoStack = newUndoStack, redoStack = ( loc, clr ) :: state.redoStack }
            in
            updateCellColor loc clr True newState
                |> highlightCells (Just loc)

        _ ->
            state


handleRedo : GameState -> GameState
handleRedo state =
    case ( state.undoStack, state.redoStack ) of
        ( _, ( loc, clr ) :: newRedoStack ) ->
            let
                newState =
                    { state | redoStack = newRedoStack, undoStack = ( loc, clr ) :: state.undoStack }
            in
            updateCellColor loc clr True newState
                |> highlightCells (Just loc)

        _ ->
            state


handlePushSnapshot : GameState -> GameState
handlePushSnapshot state =
    let
        newState =
            { state | snapshots = ( state.grid, state.undoStack ) :: state.snapshots }
    in
    newState


handlePopSnapshot : GameState -> GameState
handlePopSnapshot state =
    case state.snapshots of
        ( newGrid, newUndoStack ) :: newSnapshots ->
            let
                newState =
                    { state
                        | grid = newGrid
                        , undoStack = newUndoStack
                        , snapshots = newSnapshots
                        , redoStack = []
                    }
            in
            newState

        _ ->
            state


toggleShowErrors : GameState -> GameState
toggleShowErrors state =
    { state | showErrors = not state.showErrors }


toggleShowWins : GameState -> GameState
toggleShowWins state =
    { state | showWins = not state.showWins }


refreshViewport : Cmd Msg
refreshViewport =
    Task.attempt ViewPortChanged (Dom.getViewportOf "game_grid")


updateViewPort : Result Dom.Error Dom.Viewport -> Model -> Model
updateViewPort viewportResult model =
    case model of
        Loading config ->
            setInitialViewport config viewportResult

        Running state ->
            setRunningGameViewport state viewportResult


setInitialViewport : Config -> Result Dom.Error Dom.Viewport -> Model
setInitialViewport config viewportResult =
    case viewportResult of
        Ok viewport ->
            let
                newConfig =
                    { config
                        | viewportWidth = Just viewport.viewport.width
                        , viewportHeight = Just viewport.viewport.height
                        , error = Nothing

                        -- For testing error display during loading:
                        -- , error = Just "SABOTAGE! Sound the alarm!"
                    }
            in
            maybeStartGame newConfig

        Err _ ->
            let
                newConfig =
                    { config | error = Just "no viewport" }
            in
            Loading newConfig


setRunningGameViewport : GameState -> Result Dom.Error Dom.Viewport -> Model
setRunningGameViewport state viewportResult =
    case viewportResult of
        Ok viewport ->
            let
                newState =
                    { state
                        | viewportWidth = viewport.viewport.width
                        , viewportHeight = viewport.viewport.height
                        , error = Nothing
                    }
            in
            Running newState

        Err _ ->
            Running { state | error = Just "no viewport" }



-- This is the only place we flip from Loading to Running


maybeStartGame : Config -> Model
maybeStartGame newConfig =
    case fullConfig newConfig of
        Nothing ->
            Loading newConfig

        Just fc ->
            Running (initialGameState fc)


flipMaybeAndThen : Maybe a -> (a -> Maybe b) -> Maybe b
flipMaybeAndThen m f =
    Maybe.andThen f m


fullConfig : Config -> Maybe FullConfig
fullConfig config =
    flipMaybeAndThen config.sampleGameIndex
        (\sgi ->
            flipMaybeAndThen config.viewportHeight
                (\vph ->
                    flipMaybeAndThen config.viewportWidth
                        (\vpw ->
                            case config.error of
                                Nothing ->
                                    Just
                                        { sampleGameIndex = sgi
                                        , viewportHeight = vph
                                        , viewportWidth = vpw
                                        }

                                Just _ ->
                                    Nothing
                        )
                )
        )


updateCellColor : Location -> Grid.CellColor -> Bool -> GameState -> GameState
updateCellColor loc clr undoOrRedo state =
    let
        newUndoStack =
            if undoOrRedo then
                state.undoStack

            else
                ( loc, clr ) :: state.undoStack

        newRedoStack =
            if undoOrRedo then
                state.redoStack

            else
                []

        newState =
            { state
                | grid = Grid.updateCellColor loc clr state.grid
                , undoStack = newUndoStack
                , redoStack = newRedoStack
            }

        newState2 =
            { newState | grid = Grid.highlightedCells newState.grid (Just loc) }
    in
    newState2


highlightCells : Maybe Location -> GameState -> GameState
highlightCells mloc state =
    let
        newState =
            { state | grid = Grid.highlightedCells state.grid mloc }
    in
    newState



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] <|
        row [ width fill, height fill ] <|
            case model of
                Loading config ->
                    [ leftSidebarPlaceholder
                    , el [ Element.htmlAttribute (HA.id "game_grid"), width fill, height fill ]
                        (text
                            ("Loading"
                                ++ (case config.error of
                                        Nothing ->
                                            ""

                                        Just err ->
                                            ", but there was a problem: " ++ err
                                   )
                            )
                        )
                    , rightSidebar
                    ]

                Running state ->
                    [ leftSidebar state
                    , Grid.view state.viewportWidth state.viewportHeight state.grid state.showErrors state.showWins
                    , rightSidebar
                    ]


leftSidebarPlaceholder : Element msg
leftSidebarPlaceholder =
    column [ width (px 200), paddingXY 10 10 ]
        []


leftSidebar : GameState -> Element msg
leftSidebar state =
    column [ width (px 200), height fill, paddingXY 10 10 ]
        [ sampleGameSelectorView state
        , errorModeView state
        , winModeView state
        , snapshotCountView state
        ]


rightSidebar : Element msg
rightSidebar =
    column [ height fill ] [ text "Right Sidebar" ]


sampleGameSelectorView : GameState -> Element msg
sampleGameSelectorView model =
    el [ paddingXY 10 10 ] (text ("Sample Game: " ++ String.fromInt model.selectedSample))


errorModeView : GameState -> Element msg
errorModeView model =
    let
        txt =
            if model.showErrors then
                "Yes"

            else
                "No"
    in
    el [ paddingXY 10 10 ] (text <| "Show Errors: " ++ txt)


winModeView : GameState -> Element msg
winModeView model =
    let
        txt =
            if model.showWins then
                "Yes"

            else
                "No"
    in
    el [ paddingXY 10 10 ] (text <| "Show Wins: " ++ txt)


snapshotCountView : GameState -> Element msg
snapshotCountView model =
    el [ paddingXY 10 10 ] (text ("Snapshots: " ++ String.fromInt (List.length model.snapshots)))



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
