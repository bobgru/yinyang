module Grid exposing (..)

-- import Element.Border as Border

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Events as EE exposing (onMouseLeave)
import Html.Attributes as HA
import Html.Events as HE exposing (onClick, onMouseOver)
import Json.Decode as Decode
import Location exposing (..)
import Msg exposing (..)
import Set as Set
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Task exposing (..)
import Tree as Tree



-- MODEL ----


type alias Model =
    { grid : SparseGrid
    , highlightedCell : Maybe Location
    , connectedCells : Set.Set Location
    , errorCells : Set.Set Location
    , isWin : Bool
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
    String


unassigned : CellColor
unassigned =
    "unassigned"


black : CellColor
black =
    "black"


white : CellColor
white =
    "white"


initialModel : SparseGridInput -> Model
initialModel cells =
    -- assumes cells are legal positions
    let
        newCells : SparseCells
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
    , isWin = False
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

        newModel =
            { sparse | grid = newGrid }
    in
    { newModel | isWin = checkWin newModel }


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
                errorInComplement : Bool
                errorInComplement =
                    checkErrorInComplement sparse mloc connectedCellsPlus

                twoByTwoErrors : Set.Set Location
                twoByTwoErrors =
                    getSquares sparse.grid.cells

                checkerboardErrors : Set.Set Location
                checkerboardErrors =
                    getCheckerboardErrors sparse.grid.cells
            in
            if errorInComplement then
                List.foldr Set.union Set.empty [ twoByTwoErrors, checkerboardErrors, connectedCellsPlus ]

            else
                List.foldr Set.union Set.empty [ twoByTwoErrors, checkerboardErrors ]
    in
    { sparse
        | highlightedCell = mloc
        , connectedCells = connectedCells
        , errorCells = errorCells
    }


checkWin : Model -> Bool
checkWin sparse =
    let
        blackCells =
            Dict.filter (\_ cell -> matchBlack cell) sparse.grid.cells

        blackLocations =
            blackCells |> Dict.keys |> Set.fromList
    in
    case Dict.toList blackCells |> List.head of
        Nothing ->
            False

        Just ( locBlack, _ ) ->
            let
                whiteCells =
                    Dict.filter (\_ cell -> matchWhite cell) sparse.grid.cells

                whiteLocations =
                    whiteCells |> Dict.keys |> Set.fromList
            in
            case Dict.toList whiteCells |> List.head of
                Nothing ->
                    False

                Just ( locWhite, _ ) ->
                    let
                        connectedBlackCells =
                            getConnectedCells (matchColor black) blackCells locBlack
                                |> Set.union (Set.singleton locBlack)

                        blackHasTwoByTwoErrors =
                            checkSquaresInLocations blackLocations

                        connectedWhiteCells =
                            getConnectedCells (matchColor white) whiteCells locWhite
                                |> Set.union (Set.singleton locWhite)

                        whiteHasTwoByTwoErrors =
                            checkSquaresInLocations whiteLocations
                    in
                    not blackHasTwoByTwoErrors
                        && not whiteHasTwoByTwoErrors
                        && Set.size connectedBlackCells
                        + Set.size connectedWhiteCells
                        == sparse.grid.width
                        * sparse.grid.height


checkSquaresInLocations : Set.Set Location -> Bool
checkSquaresInLocations locs =
    getSquaresForLocations locs |> Set.isEmpty |> not


getSquares : SparseCells -> Set.Set Location
getSquares cells =
    let
        blacks =
            Dict.filter (\_ cell -> matchBlack cell) cells
                |> Dict.keys
                |> Set.fromList

        blackSquares =
            getSquaresForLocations blacks

        whites =
            Dict.filter (\_ cell -> matchWhite cell) cells
                |> Dict.keys
                |> Set.fromList

        whiteSquares =
            getSquaresForLocations whites
    in
    Set.union blackSquares whiteSquares


getSquaresForLocations : Set.Set Location -> Set.Set Location
getSquaresForLocations locs =
    let
        isSquare : List Location -> Bool
        isSquare =
            List.all (\loc -> Set.member loc locs)

        possibleSquares : Location -> List (List Location)
        possibleSquares ( r, c ) =
            [ [ ( r - 1, c - 1 ), ( r - 1, c ), ( r, c - 1 ), ( r, c ) ]
            , [ ( r - 1, c ), ( r - 1, c + 1 ), ( r, c ), ( r, c + 1 ) ]
            , [ ( r, c - 1 ), ( r, c ), ( r + 1, c - 1 ), ( r + 1, c ) ]
            , [ ( r, c ), ( r, c + 1 ), ( r + 1, c ), ( r + 1, c + 1 ) ]
            ]

        getSquaresForLocation : Location -> Set.Set Location
        getSquaresForLocation loc =
            Set.fromList <| List.concat <| List.filter isSquare <| possibleSquares loc
    in
    List.foldr Set.union Set.empty <| List.map getSquaresForLocation <| Set.toList locs


checkErrorInComplement : Model -> Maybe Location -> Set.Set Location -> Bool
checkErrorInComplement sparse mloc connectedCellsPlus =
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


getCheckerboardErrors : SparseCells -> Set.Set Location
getCheckerboardErrors cells =
    let
        isPartOfCheckerboard : Location -> Cell -> Bool
        isPartOfCheckerboard loc _ =
            getCheckerboardCells cells loc
                |> Set.isEmpty
                |> not
    in
    Dict.filter isPartOfCheckerboard cells
        |> Dict.toList
        |> List.map Tuple.first
        |> Set.fromList


getCheckerboardCells : SparseCells -> Location -> Set.Set Location
getCheckerboardCells cells loc =
    let
        allSameColor : List Location -> Bool
        allSameColor ls =
            List.map (\l -> Dict.get l cells) ls
                |> List.map (Maybe.map (\c -> c.color))
                |> List.map (Maybe.withDefault unassigned)
                |> Set.fromList
                |> (\s -> Set.size s == 1 && arbitraryElementIsAssigned s)

        arbitraryElementIsAssigned : Set.Set CellColor -> Bool
        arbitraryElementIsAssigned s =
            case Set.toList s |> List.head of
                Nothing ->
                    False

                Just clr ->
                    clr /= unassigned

        notAllSameColor : List Location -> Bool
        notAllSameColor ls =
            List.map (\l -> Dict.get l cells) ls
                |> List.map (Maybe.map (\c -> c.color))
                |> List.map (Maybe.withDefault unassigned)
                |> Set.fromList
                |> (\s -> Set.size s == 2 && not (Set.member "unassigned" s))

        isGroupError : ( List Location, List Location ) -> Bool
        isGroupError ( d1, d2 ) =
            allSameColor d1
                && allSameColor d2
                && notAllSameColor (List.concat [ d1, d2 ])

        neighborDiagonals : Location -> List ( List Location, List Location )
        neighborDiagonals ( rr, cc ) =
            let
                fromLowerRight ( r, c ) =
                    ( [ ( r - 1, c - 1 ), ( r, c ) ], [ ( r - 1, c ), ( r, c - 1 ) ] )

                fromLowerLeft ( r, c ) =
                    ( [ ( r - 1, c ), ( r, c + 1 ) ], [ ( r - 1, c + 1 ), ( r, c ) ] )

                fromUpperRight ( r, c ) =
                    ( [ ( r, c - 1 ), ( r + 1, c ) ], [ ( r, c ), ( r + 1, c - 1 ) ] )

                fromUpperLeft ( r, c ) =
                    ( [ ( r, c ), ( r + 1, c + 1 ) ], [ ( r, c + 1 ), ( r + 1, c ) ] )
            in
            [ fromLowerRight ( rr, cc )
            , fromLowerLeft ( rr, cc )
            , fromUpperRight ( rr, cc )
            , fromUpperLeft ( rr, cc )
            ]
    in
    Set.fromList <|
        List.concat <|
            List.map (\( x, y ) -> List.concat [ x, y ]) <|
                List.filter isGroupError <|
                    neighborDiagonals loc


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


matchWhite : Cell -> Bool
matchWhite cell =
    cell.color == white


matchBlack : Cell -> Bool
matchBlack cell =
    cell.color == black


matchUnassigned : Cell -> Bool
matchUnassigned cell =
    cell.color == unassigned


getConnectedCells : (Maybe Cell -> Bool) -> SparseCells -> Location -> Set.Set Location
getConnectedCells match cells loc =
    let
        mCell =
            Dict.get loc cells
    in
    if match mCell then
        let
            sameColorCells =
                Dict.filter (\_ c2 -> match (Just c2)) cells
        in
        getConnCells2 sameColorCells (Set.singleton loc) Set.empty

    else
        Set.empty


getConnCells2 : SparseCells -> Set.Set Location -> Set.Set Location -> Set.Set Location
getConnCells2 cells seeds connCells =
    let
        neighbors ( r, c ) =
            [ ( r - 1, c ), ( r + 1, c ), ( r, c - 1 ), ( r, c + 1 ) ]

        isValid loc =
            not (Set.member loc connCells)
                && Dict.member loc cells

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
        getConnCells2 cells newSeeds newConnCells


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
    case Dict.toList cells |> List.head of
        Nothing ->
            acc

        Just ( loc, _ ) ->
            let
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



--- VIEW ----


view : Float -> Float -> Model -> Bool -> Bool -> Bool -> Element Msg
view viewportWidth viewportHeight model showErrors showWins showPolylines =
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

        connectedCellsPolyline =
            let
                xRange =
                    List.range 0 (height - 1)

                yRange =
                    List.range 0 (width - 1)

                pairs xs ys =
                    List.concat <| List.map (\x -> List.map (\y -> ( x, y )) ys) xs

                cs =
                    List.map f <| pairs xRange yRange

                f ( x, y ) =
                    let
                        c =
                            Maybe.withDefault
                                (Cell unassigned False)
                            <|
                                Dict.get ( x, y ) model.grid.cells
                    in
                    ( ( x, y ), c )

                callSvgDot =
                    \( ( x, y ), c ) -> svgDot ( x, y ) cellSize model showErrors showWins c
            in
            el
                [ Element.width (px (cellSize * width))
                , Element.height (px (cellSize * height))
                , centerX
                , centerY
                ]
            <|
                Element.html <|
                    S.svg
                        [ SA.width (String.fromInt <| cellSize * width)
                        , SA.height (String.fromInt <| cellSize * height)
                        ]
                    <|
                        List.map callSvgDot cs
                            ++ (if showPolylines then
                                    [ polylineView cellSize height model.connectedCells ]

                                else
                                    []
                               )
    in
    [ connectedCellsPolyline ]
        |> column
            [ Element.width fill
            , Element.height fill
            , alignRight
            , centerY
            , EE.onMouseLeave (CellHighlighted Nothing)
            , htmlAttribute (HA.id "game_grid")
            , Background.color (rgb255 0xAA 0xEE 0xAA)
            ]


polylineView : Int -> Int -> Set.Set Location -> S.Svg Msg
polylineView cellSize height connectedCells =
    let
        cvtCoord x =
            round ((toFloat x * toFloat cellSize) + (toFloat cellSize / 2))

        cvtPoint ( x, y ) =
            ( cvtCoord y, cvtCoord x )

        loc =
            Maybe.withDefault ( 0, 0 ) <| List.head <| Set.toList connectedCells

        tree =
            Tree.fromLocationSet loc connectedCells

        pts =
            Tree.toPath tree |> List.map cvtPoint

        ptsStr =
            Tree.toPolylinePointsWithConversion cvtPoint tree

        myCircle ( x, y ) =
            S.circle
                [ SA.cx (String.fromInt x)
                , SA.cy (String.fromInt y)
                , SA.r (String.fromInt (round (toFloat cellSize / 20)))
                , SA.fill "#808080"
                , SA.stroke "none"
                ]
                []

        strokeWidth =
            String.fromInt <| round <| toFloat cellSize * 0.5

        svgHeight =
            String.fromInt (cellSize * height)
    in
    S.svg [ SA.height svgHeight, SA.width svgHeight ] <|
        S.polyline
            [ SA.fill "none"
            , SA.strokeLinejoin "round"
            , SA.strokeLinecap "round"
            , SA.strokeWidth strokeWidth
            , SA.stroke "yellow"
            , SA.points ptsStr
            ]
            []
            :: List.map myCircle pts


svgDot : Location -> Int -> Model -> Bool -> Bool -> Cell -> S.Svg Msg
svgDot loc cellSize model showErrors showWins cell =
    let
        bgUnassigned =
            "#CCCCCC"

        bgLocked =
            "#AAEEAA"

        bgError =
            "#AA6666"

        bgConnected =
            "#6666AA"

        bgHighlighted =
            "#AAAAAA"

        bgWin =
            "orange"

        squareEdge =
            "#AAAAAA"

        circleEdge =
            "#AAAAAA"

        cvtCornerCoord dim xx =
            round (toFloat xx * toFloat dim)

        cvtCenterCoord dim xx =
            round ((toFloat xx * toFloat dim) + (toFloat dim / 2))

        ( x, y ) =
            loc

        cx =
            String.fromInt <| cvtCenterCoord cellSize y

        cy =
            String.fromInt <| cvtCenterCoord cellSize x

        radius =
            String.fromInt <|
                round (0.8 * toFloat cellSize / 2)

        side =
            String.fromInt cellSize

        square locked =
            let
                isError =
                    Set.member loc model.errorCells

                inConnectedSet =
                    Set.member loc model.connectedCells

                highlighted =
                    case model.highlightedCell of
                        Nothing ->
                            False

                        Just loc2 ->
                            loc == loc2

                defaultFillColor =
                    if locked then
                        bgLocked

                    else
                        bgUnassigned

                fillColor =
                    if model.isWin && showWins then
                        bgWin

                    else if isError && showErrors then
                        bgError

                    else if inConnectedSet then
                        bgConnected

                    else if highlighted then
                        bgHighlighted

                    else
                        defaultFillColor

                -- Transposing x and y.
                xStr =
                    String.fromInt <| cvtCornerCoord cellSize y

                yStr =
                    String.fromInt <| cvtCornerCoord cellSize x
            in
            S.rect
                ([ SA.fill fillColor
                 , SA.width side
                 , SA.height side
                 , SA.x xStr
                 , SA.y yStr
                 , HE.onMouseOver (CellHighlighted (Just loc))
                 , HE.onClick (CellLeftClicked loc)
                 , onSvgRightClick loc
                 , SA.cursor "pointer"
                 ]
                    ++ (if highlighted || inConnectedSet || isError then
                            []

                        else
                            [ SA.stroke squareEdge
                            , SA.strokeWidth "3"
                            ]
                       )
                )
                []

        circle fillColor =
            S.circle
                [ SA.cx cx
                , SA.cy cy
                , SA.r radius
                , SA.fill fillColor
                , SA.stroke circleEdge
                , SA.strokeWidth "3"
                , HE.onClick (CellLeftClicked loc)
                , onSvgRightClick loc
                , SA.cursor "pointer"
                ]
                []
    in
    S.svg []
        (square cell.locked
            :: (if cell.color == "unassigned" then
                    []

                else
                    [ circle cell.color ]
               )
        )


onRightClick : Location -> Attribute Msg
onRightClick loc =
    HE.custom "contextmenu"
        (Decode.succeed
            { message = CellRightClicked loc
            , stopPropagation = True
            , preventDefault = True
            }
        )
        |> htmlAttribute


onSvgRightClick : Location -> S.Attribute Msg
onSvgRightClick loc =
    SE.custom "contextmenu"
        (Decode.succeed
            { message = CellRightClicked loc
            , stopPropagation = True
            , preventDefault = True
            }
        )
