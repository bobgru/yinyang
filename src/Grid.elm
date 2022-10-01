module Grid exposing (..)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Html.Events exposing (custom)
import Json.Decode as Decode
import Location exposing (..)
import Msg exposing (..)
import Svg as S
import Svg.Attributes as SA
import Task exposing (..)



---- MODEL ----


type alias Model =
    SparseModel


type alias SparseModel =
    { width : Int
    , height : Int
    , cells : Dict.Dict Location CellColor
    }


type alias DenseModel =
    { width : Int
    , height : Int
    , cells : List (List CellColor)
    }


type alias SparseModelInput =
    { width : Int
    , height : Int
    , cells : List ( Location, CellColor )
    }


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


sparseFromInput : SparseModelInput -> SparseModel
sparseFromInput cells =
    -- assumes cells are legal positions
    { width = cells.width, height = cells.height, cells = Dict.fromList cells.cells }


denseFromSparse : SparseModel -> DenseModel
denseFromSparse sparse =
    let
        dense : DenseModel
        dense =
            { width = sparse.width
            , height = sparse.height
            , cells = List.repeat sparse.height (List.repeat sparse.width unassigned)
            }

        updateRowFromSparse : Int -> List CellColor -> List CellColor
        updateRowFromSparse rowIndex rowCells =
            List.indexedMap (updateColumnFromSparse rowIndex) rowCells

        updateColumnFromSparse : Int -> Int -> CellColor -> CellColor
        updateColumnFromSparse rowIndex columnIndex clr =
            case Dict.get ( rowIndex, columnIndex ) sparse.cells of
                Nothing ->
                    clr

                Just newClr ->
                    newClr
    in
    { width = sparse.width
    , height = sparse.height
    , cells = List.indexedMap updateRowFromSparse dense.cells
    }


updateGrid : Location -> CellColor -> SparseModel -> SparseModel
updateGrid loc clr sparse =
    let
        newClr =
            case Dict.get loc sparse.cells of
                Nothing ->
                    clr

                Just oldClr ->
                    if oldClr == clr then
                        unassigned

                    else
                        clr
    in
    { sparse | cells = Dict.insert loc newClr sparse.cells }



---- VIEW ----


svgGrid : Float -> Float -> SparseModel -> Element Msg
svgGrid viewportWidth viewportHeight sparse =
    let
        dense =
            denseFromSparse sparse

        cellSizeFromViewport : Float -> Float -> Int -> Int -> Int
        cellSizeFromViewport width height gridWidth gridHeight =
            min (width / toFloat gridWidth) (height / toFloat gridHeight)
                |> (\x -> x * 0.9)
                |> round

        cellSize : Int
        cellSize =
            cellSizeFromViewport viewportWidth viewportHeight dense.width dense.height
    in
    column [ centerX, centerY, Border.color (rgb 0 0 0), Border.width 1 ] <|
        List.indexedMap (\r rw -> rowView r dense.width cellSize rw) dense.cells


rowView : Int -> Int -> Int -> List CellColor -> Element Msg
rowView rowIndex gridWidth cellSize rowCells =
    row
        [ centerX
        , centerY
        , width (px (gridWidth * cellSize))
        , height (px cellSize)
        ]
    <|
        List.indexedMap (\columnIndex cell -> dot ( rowIndex, columnIndex ) cellSize cell) rowCells


onRightClick loc =
    Html.Events.custom "contextmenu"
        (Decode.succeed
            { message = CellRightClicked loc
            , stopPropagation = True
            , preventDefault = True
            }
        )


dot : Location -> Int -> CellColor -> Element Msg
dot loc cellSize clr =
    let
        cx =
            String.fromInt <| round (toFloat cellSize / 2)

        cy =
            String.fromInt <| round (toFloat cellSize / 2)

        radius =
            String.fromInt <|
                round (0.8 * toFloat cellSize / 2)

        side =
            String.fromInt cellSize

        square =
            S.rect
                [ SA.fill "lightgray"
                , SA.stroke "black"
                , SA.strokeWidth "3"
                , SA.width side
                , SA.height side
                , SA.x "0"
                , SA.y "0"
                ]
                []

        circle fillClr =
            S.circle
                [ SA.cx cx
                , SA.cy cy
                , SA.r radius
                , SA.fill fillClr
                , SA.stroke "darkgray"
                , SA.strokeWidth "3"
                ]
                []
    in
    el
        [ width (px cellSize)
        , pointer
        , onClick (CellLeftClicked loc)
        , htmlAttribute <| onRightClick loc
        ]
    <|
        Element.html <|
            S.svg [ SA.height side ]
                (square
                    :: (case clr of
                            Nothing ->
                                []

                            Just fillClr ->
                                [ circle fillClr ]
                       )
                )
