module Main exposing (..)

import Browser
import Browser.Dom as Dom
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
    , viewportWidth : Float
    , viewportHeight : Float
    , error : Maybe String
    }


type alias Grid =
    List (List (Maybe String))


unassigned : Maybe String
unassigned =
    Nothing


black : Maybe String
black =
    Just "black"


white : Maybe String
white =
    Just "white"


initialGrid : Grid
initialGrid =
    [ [ black, black, unassigned ]
    , [ black, unassigned, white ]
    , [ unassigned, white, unassigned ]
    ]


initialModel : Model
initialModel =
    { grid = initialGrid
    , viewportWidth = 100.0
    , viewportHeight = 100.0
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
    column [ centerX, centerY ] <|
        List.map (rowView cellSize) model.grid


rowView : Int -> List (Maybe String) -> Element msg
rowView cellSize r =
    row [ centerX, centerY, width (px (3 * cellSize)), height (px cellSize), explain Debug.todo ] <|
        List.map (dot cellSize) r


dot : Int -> Maybe String -> Element msg
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
    min (width / 3) (height / 3) |> (\x -> x * 0.9) |> round



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
