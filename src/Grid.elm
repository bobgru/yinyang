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
import Set as Set
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
    , connectedCells : Set.Set Location
    , errorCells : Set.Set Location
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
    , connectedCells : Set.Set Location
    , errorCells : Set.Set Location
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
    , connectedCells = Set.empty
    , errorCells = Set.empty
    }


sparseFromDense : DenseModel -> SparseModel
sparseFromDense dense =
    let
        foldRow : Int -> List Cell -> Dict.Dict Location Cell
        foldRow rowIndex rowCells =
            let
                singletons =
                    List.indexedMap (\columnIndex cell -> Dict.singleton ( rowIndex, columnIndex ) cell) rowCells
            in
            List.foldr Dict.union Dict.empty singletons

        newCells : Dict.Dict Location Cell
        newCells =
            List.foldr Dict.union Dict.empty <| List.indexedMap foldRow dense.cells
    in
    { cells = newCells
    , width = dense.width
    , height = dense.height
    , highlightedCell = dense.highlightedCell
    , connectedCells = dense.connectedCells
    , errorCells = dense.errorCells
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
            , connectedCells = sparse.connectedCells
            , errorCells = sparse.errorCells
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
    , connectedCells = dense.connectedCells
    , errorCells = dense.errorCells
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
    let
        connectedCellsPlus : Set.Set Location
        connectedCellsPlus =
            if Set.isEmpty connectedCells then
                case mloc of
                    Nothing ->
                        Set.empty

                    Just loc ->
                        Set.singleton loc

            else
                connectedCells

        connectedCells : Set.Set Location
        connectedCells =
            case mloc of
                Nothing ->
                    Set.empty

                Just loc ->
                    let
                        mCell =
                            Dict.get loc sparse.cells

                        clr =
                            Maybe.map (\c2 -> c2.color) mCell |> Maybe.withDefault unassigned
                    in
                    if clr == unassigned then
                        Set.empty

                    else
                        getConnectedCells (matchColor clr) sparse.cells loc

        errorCells : Set.Set Location
        errorCells =
            let
                errorInComplement =
                    case mloc of
                        Nothing ->
                            False

                        Just loc ->
                            let
                                mCell =
                                    Dict.get loc sparse.cells

                                clr =
                                    Maybe.map (\c2 -> c2.color) mCell |> Maybe.withDefault unassigned
                            in
                            if clr == unassigned then
                                False

                            else
                                let
                                    connectedComplements : List (Set.Set Location)
                                    connectedComplements =
                                        getConnectedComplements connectedCellsPlus sparse
                                in
                                List.length connectedComplements > 1

                twoByTwoErrors =
                    case mloc of
                        Nothing ->
                            Set.empty

                        Just loc ->
                            getErrorCells sparse loc
            in
            if errorInComplement then
                Set.union twoByTwoErrors connectedCellsPlus

            else
                twoByTwoErrors
    in
    { sparse
        | highlightedCell = mloc
        , connectedCells = connectedCells
        , errorCells = errorCells
    }


getErrorCells : SparseModel -> Location -> Set.Set Location
getErrorCells sparse loc =
    case Dict.get loc sparse.cells of
        Nothing ->
            Set.empty

        Just cell ->
            getErrorCells2 sparse loc cell


getErrorCells2 : SparseModel -> Location -> Cell -> Set.Set Location
getErrorCells2 sparse loc cell =
    let
        sameColor : Location -> Bool
        sameColor loc2 =
            case Dict.get loc2 sparse.cells of
                Nothing ->
                    False

                Just cell2 ->
                    cell2.color == cell.color

        isGroupError : List Location -> Bool
        isGroupError testCells =
            List.all sameColor testCells

        neighborGroups : Location -> List (List Location)
        neighborGroups ( r, c ) =
            [ [ ( r - 1, c - 1 ), ( r - 1, c ), ( r, c - 1 ), ( r, c ) ]
            , [ ( r - 1, c ), ( r - 1, c + 1 ), ( r, c ), ( r, c + 1 ) ]
            , [ ( r, c - 1 ), ( r, c ), ( r + 1, c - 1 ), ( r + 1, c ) ]
            , [ ( r, c ), ( r, c + 1 ), ( r + 1, c ), ( r + 1, c + 1 ) ]
            ]
    in
    Set.fromList <| List.concat <| List.filter isGroupError <| neighborGroups loc


matchColor : CellColor -> Maybe Cell -> Bool
matchColor clr mCell =
    case mCell of
        Nothing ->
            False

        Just cell ->
            cell.color == clr


matchOpposite : CellColor -> Maybe Cell -> Bool
matchOpposite clr mCell =
    case mCell of
        Nothing ->
            True

        Just cell ->
            cell.color /= clr


matchUnassigned : CellColor -> Maybe Cell -> Bool
matchUnassigned _ mCell =
    case mCell of
        Nothing ->
            True

        Just cell ->
            cell.color == unassigned


getConnectedCells : (Maybe Cell -> Bool) -> Dict.Dict Location Cell -> Location -> Set.Set Location
getConnectedCells match cells loc =
    let
        mCell =
            Dict.get loc cells
    in
    if match mCell then
        getConnCells2 match cells loc

    else
        Set.empty


getConnCells2 : (Maybe Cell -> Bool) -> Dict.Dict Location Cell -> Location -> Set.Set Location
getConnCells2 match cells loc =
    let
        sameColorCells =
            Dict.filter (\_ c2 -> match (Just c2)) cells
    in
    getConnCells3 sameColorCells (Set.singleton loc) Set.empty


getConnCells3 : Dict.Dict Location Cell -> Set.Set Location -> Set.Set Location -> Set.Set Location
getConnCells3 cells seeds connCells =
    let
        neighbors ( r, c ) =
            [ ( r - 1, c ), ( r + 1, c ), ( r, c - 1 ), ( r, c + 1 ) ]

        inGrid : Location -> Dict.Dict Location Cell -> Bool
        inGrid l d =
            case Dict.get l d of
                Nothing ->
                    False

                Just _ ->
                    True

        isValid loc =
            not (Set.member loc connCells)
                && inGrid loc cells

        newSeeds =
            Set.filter isValid <|
                Set.fromList <|
                    List.concat <|
                        List.map neighbors <|
                            Set.toList seeds
    in
    if Set.isEmpty newSeeds then
        connCells

    else
        let
            newConnCells =
                Set.union connCells newSeeds
        in
        getConnCells3 cells newSeeds newConnCells


complementFromSparse : SparseModel -> CellColor -> SparseModel
complementFromSparse sparse clr =
    let
        d =
            denseFromSparse sparse

        s =
            sparseFromDense d

        newCells =
            Dict.filter (\_ c -> matchOpposite clr (Just c)) s.cells
    in
    { sparse | cells = newCells }


getConnectedComplements : Set.Set Location -> SparseModel -> List (Set.Set Location)
getConnectedComplements connCells sparse =
    -- TODO try to avoid calling with empty connCells
    if Set.isEmpty connCells then
        []

    else
        let
            -- Get color of connected cells so we can look for the opposite.
            -- connCells is non-empty so default for connLoc and connClr is just for type-checking.
            connLoc =
                Set.toList connCells |> List.head |> Maybe.withDefault ( 0, 0 )

            connClr =
                Dict.get connLoc sparse.cells |> Maybe.map (\c -> c.color) |> Maybe.withDefault black

            -- Complement the entire sparse model. This means we have to create a dense model then
            -- convert back to sparse first.
            compSparse =
                complementFromSparse sparse connClr
        in
        getConnectedComplements2 compSparse.cells []



-- sparse is already the complement so we are really finding all the connected cell groups within it


getConnectedComplements2 : Dict.Dict Location Cell -> List (Set.Set Location) -> List (Set.Set Location)
getConnectedComplements2 cells acc =
    if Dict.isEmpty cells then
        acc

    else
        let
            -- cells cannot be empty, so default is irrelevant
            loc =
                x4

            x1 : List ( Location, Cell )
            x1 =
                Dict.toList cells

            x2 : Maybe ( Location, Cell )
            x2 =
                List.head x1

            x3 : Maybe Location
            x3 =
                Maybe.map (\( k, _ ) -> k) x2

            x4 : Location
            x4 =
                Maybe.withDefault ( 0, 0 ) x3

            connCells =
                let
                    connCells2 =
                        getConnectedCells (\_ -> True) cells loc
                in
                if Set.isEmpty connCells2 then
                    Set.singleton loc

                else
                    connCells2

            newAcc =
                connCells :: acc

            newCells =
                Dict.filter (\k _ -> not (Set.member k connCells)) cells
        in
        getConnectedComplements2 newCells newAcc



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
        List.indexedMap
            (\r rw ->
                rowView r
                    dense.width
                    cellSize
                    dense.highlightedCell
                    dense.connectedCells
                    dense.errorCells
                    rw
            )
            dense.cells


rowView : Int -> Int -> Int -> Maybe Location -> Set.Set Location -> Set.Set Location -> List Cell -> Element Msg
rowView rowIndex gridWidth cellSize mHighlightedCell connectedCells errorCells rowCells =
    row
        [ centerX
        , centerY
        , width (px (gridWidth * cellSize))
        , height (px cellSize)
        ]
    <|
        List.indexedMap
            (\columnIndex cell ->
                dot ( rowIndex, columnIndex )
                    cellSize
                    mHighlightedCell
                    connectedCells
                    errorCells
                    cell
            )
            rowCells


dot : Location -> Int -> Maybe Location -> Set.Set Location -> Set.Set Location -> Cell -> Element Msg
dot loc cellSize mHighlightedCell connectedCells errorCells cell =
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
                isError =
                    Set.member loc errorCells

                inConnectedSet =
                    Set.member loc connectedCells

                highlighted =
                    case mHighlightedCell of
                        Nothing ->
                            False

                        Just loc2 ->
                            loc == loc2

                defaultFillColor =
                    if locked then
                        "lightgreen"

                    else
                        "lightgray"

                fillColor =
                    if isError then
                        "red"

                    else if inConnectedSet then
                        "blue"

                    else if highlighted then
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
