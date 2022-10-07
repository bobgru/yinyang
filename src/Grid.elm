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
    { grid : SparseGrid
    , highlightedCell : Maybe Location
    , connectedCells : Set.Set Location
    , errorCells : Set.Set Location
    }


type alias SparseGrid =
    { width : Int
    , height : Int
    , cells : SparseCells
    }


type alias SparseCells =
    Dict.Dict Location Cell


type alias Cell =
    { color : CellColor
    , locked : Bool
    }


type alias DenseGrid =
    { width : Int
    , height : Int
    , cells : DenseCells
    }


type alias DenseCells =
    List (List Cell)


type alias SparseGridInput =
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


initialModel : SparseGridInput -> Model
initialModel cells =
    -- assumes cells are legal positions
    let
        newCells =
            Dict.fromList <|
                List.map (\( loc, clr ) -> ( loc, { color = clr, locked = True } )) cells.cells

        grid : SparseGrid
        grid =
            { width = cells.width
            , height = cells.height
            , cells = newCells
            }
                |> denseFromSparse
                |> sparseFromDense
    in
    { grid = grid
    , highlightedCell = Nothing
    , connectedCells = Set.empty
    , errorCells = Set.empty
    }


sparseFromDense : DenseGrid -> SparseGrid
sparseFromDense dense =
    let
        foldRow : Int -> List Cell -> SparseCells
        foldRow r cells =
            let
                singletons =
                    List.indexedMap (\c cell -> Dict.singleton ( r, c ) cell) cells
            in
            List.foldr Dict.union Dict.empty singletons

        newCells : SparseCells
        newCells =
            List.foldr Dict.union Dict.empty <| List.indexedMap foldRow dense.cells
    in
    { width = dense.width
    , height = dense.height
    , cells = newCells
    }


denseFromSparse : SparseGrid -> DenseGrid
denseFromSparse sparse =
    let
        denseCells : DenseCells
        denseCells =
            List.repeat sparse.height
                (List.repeat sparse.width { color = unassigned, locked = False })

        updateRowFromSparse : Int -> List Cell -> List Cell
        updateRowFromSparse r cells =
            List.indexedMap (updateColumnFromSparse r) cells

        updateColumnFromSparse : Int -> Int -> Cell -> Cell
        updateColumnFromSparse r c cell =
            Maybe.withDefault cell <|
                Dict.get ( r, c ) sparse.cells
    in
    { width = sparse.width
    , height = sparse.height
    , cells = List.indexedMap updateRowFromSparse denseCells
    }


updateCellColor : Location -> CellColor -> Model -> Model
updateCellColor loc clr sparse =
    let
        newCell =
            case Dict.get loc sparse.grid.cells of
                Nothing ->
                    { color = clr, locked = False }

                Just oldCell ->
                    if oldCell.locked then
                        oldCell

                    else if oldCell.color == clr then
                        { oldCell | color = unassigned }

                    else
                        { oldCell | color = clr }

        newCells =
            Dict.insert loc newCell sparse.grid.cells

        newGrid =
            { width = sparse.grid.width
            , height = sparse.grid.height
            , cells = newCells
            }
    in
    { sparse | grid = newGrid }


highlightedCells : Model -> Maybe Location -> Model
highlightedCells sparse mloc =
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
                            Dict.get loc sparse.grid.cells

                        clr =
                            Maybe.map (\c2 -> c2.color) mCell |> Maybe.withDefault unassigned
                    in
                    if clr == unassigned then
                        Set.empty

                    else
                        getConnectedCells (matchColor clr) sparse.grid.cells loc

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
                                    Dict.get loc sparse.grid.cells

                                clr =
                                    Maybe.map (\c2 -> c2.color) mCell |> Maybe.withDefault unassigned
                            in
                            if clr == unassigned then
                                False

                            else
                                let
                                    connectedComplements : List (Set.Set Location)
                                    connectedComplements =
                                        getConnectedComplements connectedCellsPlus sparse.grid
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


getErrorCells : Model -> Location -> Set.Set Location
getErrorCells sparse loc =
    case Dict.get loc sparse.grid.cells of
        Nothing ->
            Set.empty

        Just cell ->
            if cell.color == unassigned then
                Set.empty

            else
                getErrorCells2 sparse loc cell


getErrorCells2 : Model -> Location -> Cell -> Set.Set Location
getErrorCells2 sparse loc cell =
    let
        sameColor : Location -> Bool
        sameColor loc2 =
            case Dict.get loc2 sparse.grid.cells of
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


getConnectedCells : (Maybe Cell -> Bool) -> SparseCells -> Location -> Set.Set Location
getConnectedCells match cells loc =
    let
        mCell =
            Dict.get loc cells
    in
    if match mCell then
        getConnCells2 match cells loc

    else
        Set.empty


getConnCells2 : (Maybe Cell -> Bool) -> SparseCells -> Location -> Set.Set Location
getConnCells2 match cells loc =
    let
        sameColorCells =
            Dict.filter (\_ c2 -> match (Just c2)) cells
    in
    getConnCells3 sameColorCells (Set.singleton loc) Set.empty


getConnCells3 : SparseCells -> Set.Set Location -> Set.Set Location -> Set.Set Location
getConnCells3 cells seeds connCells =
    let
        neighbors ( r, c ) =
            [ ( r - 1, c ), ( r + 1, c ), ( r, c - 1 ), ( r, c + 1 ) ]

        inGrid : Location -> SparseCells -> Bool
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


complementFromSparse : SparseGrid -> CellColor -> SparseGrid
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


getConnectedComplements : Set.Set Location -> SparseGrid -> List (Set.Set Location)
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

            -- Complement the entire sparse grid. This means we have to create a dense grid then
            -- convert back to sparse first.
            compSparse =
                complementFromSparse sparse connClr
        in
        getConnectedComplements2 compSparse []



-- sparse is already the complement so we are really finding all the connected cell groups within it


getConnectedComplements2 : SparseGrid -> List (Set.Set Location) -> List (Set.Set Location)
getConnectedComplements2 sparse acc =
    let
        cells =
            sparse.cells
    in
    if Dict.isEmpty cells then
        acc

    else
        let
            -- cells cannot be empty, so default is irrelevant
            loc =
                Dict.toList cells
                    |> List.head
                    |> Maybe.map (\( k, _ ) -> k)
                    |> Maybe.withDefault ( 0, 0 )

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

            newSparse =
                { sparse | cells = newCells }
        in
        getConnectedComplements2 newSparse newAcc



---- VIEW ----


view : Float -> Float -> Model -> Element Msg
view viewportWidth viewportHeight model =
    let
        width =
            model.grid.width

        height =
            model.grid.height

        cellSize : Int
        cellSize =
            min (viewportWidth / toFloat width) (viewportHeight / toFloat height)
                |> (\x -> x * 0.9)
                |> round

        dense =
            denseFromSparse model.grid
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
                    width
                    cellSize
                    model.highlightedCell
                    model.connectedCells
                    model.errorCells
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
