module Grid exposing (..)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseLeave)
import Html.Events exposing (custom, onMouseOver)
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
    , cells : Dict.Dict Location Cell
    , highlightedCell : Maybe Location
    }


type alias Cell =
    { color : CellColor
    , locked : Bool
    }


type alias DenseModel =
    { width : Int
    , height : Int
    , cells : List (List Cell)
    , highlightedCell : Maybe Location
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
    let
        newCells =
            List.map (\( loc, clr ) -> ( loc, { color = clr, locked = True } )) cells.cells
    in
    { width = cells.width
    , height = cells.height
    , cells = Dict.fromList newCells
    , highlightedCell = Nothing
    }


denseFromSparse : SparseModel -> DenseModel
denseFromSparse sparse =
    let
        dense : DenseModel
        dense =
            { width = sparse.width
            , height = sparse.height
            , cells = List.repeat sparse.height (List.repeat sparse.width { color = unassigned, locked = False })
            , highlightedCell = sparse.highlightedCell
            }

        updateRowFromSparse : Int -> List Cell -> List Cell
        updateRowFromSparse rowIndex rowCells =
            List.indexedMap (updateColumnFromSparse rowIndex) rowCells

        updateColumnFromSparse : Int -> Int -> Cell -> Cell
        updateColumnFromSparse rowIndex columnIndex cell =
            case Dict.get ( rowIndex, columnIndex ) sparse.cells of
                Nothing ->
                    cell

                Just newCell ->
                    newCell
    in
    { width = sparse.width
    , height = sparse.height
    , cells = List.indexedMap updateRowFromSparse dense.cells
    , highlightedCell = dense.highlightedCell
    }


updateGrid : Location -> CellColor -> SparseModel -> SparseModel
updateGrid loc clr sparse =
    let
        newCell =
            case Dict.get loc sparse.cells of
                Nothing ->
                    { color = clr, locked = False }

                Just oldCell ->
                    if oldCell.locked then
                        oldCell

                    else if oldCell.color == clr then
                        { oldCell | color = unassigned }

                    else
                        { oldCell | color = clr }
    in
    { sparse | cells = Dict.insert loc newCell sparse.cells }


updateHighlightedCell : SparseModel -> Maybe Location -> SparseModel
updateHighlightedCell sparse mloc =
    { sparse | highlightedCell = mloc }



---- VIEW ----


view : Float -> Float -> SparseModel -> Element Msg
view viewportWidth viewportHeight sparse =
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
    column
        [ centerX
        , centerY
        , Border.color (rgb 0 0 0)
        , Border.width 1
        , onMouseLeave (CellHighlighted Nothing)
        ]
    <|
        List.indexedMap (\r rw -> rowView r dense.width cellSize dense.highlightedCell rw) dense.cells


rowView : Int -> Int -> Int -> Maybe Location -> List Cell -> Element Msg
rowView rowIndex gridWidth cellSize mHighlightedCell rowCells =
    row
        [ centerX
        , centerY
        , width (px (gridWidth * cellSize))
        , height (px cellSize)
        ]
    <|
        List.indexedMap (\columnIndex cell -> dot ( rowIndex, columnIndex ) cellSize mHighlightedCell cell) rowCells


dot : Location -> Int -> Maybe Location -> Cell -> Element Msg
dot loc cellSize mHighlightedCell cell =
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

        square locked =
            let
                defaultFillColor =
                    if locked then
                        "lightgreen"

                    else
                        "lightgray"

                fillColor =
                    case mHighlightedCell of
                        Nothing ->
                            defaultFillColor

                        Just loc2 ->
                            if loc == loc2 then
                                "yellow"

                            else
                                defaultFillColor
            in
            S.rect
                [ SA.fill fillColor
                , SA.stroke "black"
                , SA.strokeWidth "3"
                , SA.width side
                , SA.height side
                , SA.x "0"
                , SA.y "0"
                , onMouseOver (CellHighlighted (Just loc))
                ]
                []

        circle fillColor =
            S.circle
                [ SA.cx cx
                , SA.cy cy
                , SA.r radius
                , SA.fill fillColor
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
                (square cell.locked
                    :: (case cell.color of
                            Nothing ->
                                []

                            Just fillClr ->
                                [ circle fillClr ]
                       )
                )


onRightClick loc =
    Html.Events.custom "contextmenu"
        (Decode.succeed
            { message = CellRightClicked loc
            , stopPropagation = True
            , preventDefault = True
            }
        )
