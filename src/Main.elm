module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Svg as S
import Svg.Attributes as SA
import Task exposing (..)



---- MODEL ----


type alias Model =
    { grid : Grid
    , gridWidth : Int
    , gridHeight : Int
    , initialCells : InitialCells
    , viewportWidth : Float
    , viewportHeight : Float
    , error : Maybe String
    }


type alias Grid =
    List (List (Maybe String))


type alias InitialCells =
    List ( Int, Int, Maybe String )


type alias CellColor =
    Maybe String


unassigned : CellColor
unassigned =
    Nothing


black : CellColor
black =
    Just "black"


white : CellColor
white =
    Just "white"


gridWidth : Int
gridWidth =
    10


gridHeight : Int
gridHeight =
    10


initialCells : InitialCells
initialCells =
    [ ( 1, 2, white )
    , ( 1, 5, white )
    , ( 2, 3, white )
    , ( 2, 6, white )
    , ( 2, 8, black )
    , ( 4, 2, white )
    , ( 4, 5, white )
    , ( 4, 7, white )
    , ( 5, 1, black )
    , ( 6, 4, white )
    , ( 6, 6, white )
    , ( 7, 1, black )
    , ( 7, 8, white )
    , ( 8, 0, black )
    , ( 8, 4, white )
    , ( 8, 6, black )
    , ( 9, 0, white )
    , ( 9, 1, white )
    ]


initializeGrid : Int -> Int -> InitialCells -> Grid
initializeGrid width height cells =
    -- assumes cells are legal positions and are sorted by row and column
    let
        sparse : Dict.Dict ( Int, Int ) CellColor
        sparse =
            Dict.fromList <| List.map (\( r, c, mClr ) -> ( ( r, c ), mClr )) cells

        dense : Grid
        dense =
            List.repeat height (List.repeat width unassigned)

        updateRowFromSparse : Int -> List CellColor -> List CellColor
        updateRowFromSparse r rowCells =
            List.indexedMap (updateColumnFromSparse r) rowCells

        updateColumnFromSparse : Int -> Int -> CellColor -> CellColor
        updateColumnFromSparse r c mClr =
            case Dict.get ( r, c ) sparse of
                Nothing ->
                    mClr

                Just newMClr ->
                    newMClr
    in
    List.indexedMap updateRowFromSparse dense


initialModel : Model
initialModel =
    { grid = initializeGrid gridWidth gridHeight initialCells
    , gridWidth = gridWidth
    , gridHeight = gridHeight
    , initialCells = initialCells
    , viewportWidth = 1.0 -- placeholder
    , viewportHeight = 1.0 -- placeholder
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, refreshViewport )


refreshViewport : Cmd Msg
refreshViewport =
    Task.attempt ViewPortChanged Dom.getViewport



---- UPDATE ----


type Msg
    = NoOp
    | ViewPortChanged (Result Dom.Error Dom.Viewport)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ViewPortChanged (Ok viewport) ->
            let
                newModel =
                    { model
                        | viewportWidth = viewport.viewport.width
                        , viewportHeight = viewport.viewport.height
                        , error = Nothing
                    }
            in
            ( newModel, Cmd.none )

        ViewPortChanged (Err _) ->
            ( { model | error = Just "no viewport" }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        []
        (svgGrid model)


svgGrid : Model -> Element msg
svgGrid model =
    let
        cellSize =
            cellSizeFromViewport model.viewportWidth model.viewportHeight
    in
    column [ centerX, centerY, Border.color (rgb 0 0 0), Border.width 1 ] <|
        List.map (rowView gridWidth cellSize) model.grid


rowView : Int -> Int -> List CellColor -> Element msg
rowView w cellSize r =
    row [ centerX, centerY, width (px (w * cellSize)), height (px cellSize) ] <|
        List.map (dot cellSize) r


dot : Int -> CellColor -> Element msg
dot cellSize clr =
    let
        cx =
            String.fromInt <| round (toFloat cellSize / 2)

        cy =
            String.fromInt <| round (toFloat cellSize / 2)

        r =
            String.fromInt <|
                round (0.9 * toFloat cellSize / 2)

        s =
            String.fromInt cellSize
    in
    Element.html <|
        S.svg [ SA.height s ]
            ([ S.rect
                [ SA.fill "lightgray"
                , SA.stroke "black"
                , SA.strokeWidth "3"
                , SA.width s
                , SA.height s
                , SA.x "0"
                , SA.y "0"
                ]
                []
             ]
                ++ (case clr of
                        Nothing ->
                            []

                        Just c ->
                            [ S.circle
                                [ SA.cx cx
                                , SA.cy cy
                                , SA.r r
                                , SA.fill c
                                , SA.stroke "black"
                                , SA.strokeWidth "2"
                                ]
                                []
                            ]
                   )
            )


cellSizeFromViewport : Float -> Float -> Int
cellSizeFromViewport width height =
    min (width / toFloat gridWidth) (height / toFloat gridHeight) |> (\x -> x * 0.9) |> round



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
