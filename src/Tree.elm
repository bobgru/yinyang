module Tree exposing (..)

import Location exposing (..)
import Set as Set


type Tree a
    = Node a (List (Tree a))
    | Leaf a


singleton : a -> Tree a
singleton x =
    Leaf x


tree : a -> List (Tree a) -> Tree a
tree x ts =
    Node x ts


fromLocationSet : Location -> Set.Set Location -> Tree Location
fromLocationSet loc locs =
    let
        locs2 =
            Set.remove loc locs

        neighbors ( r, c ) =
            [ ( r - 1, c ), ( r + 1, c ), ( r, c - 1 ), ( r, c + 1 ) ]

        isValid k =
            Set.member k locs2

        seeds =
            List.filter isValid <|
                neighbors loc

        locs3 =
            Set.foldl (\k s -> Set.remove k s) locs2 (Set.fromList seeds)
    in
    if List.isEmpty seeds then
        singleton loc

    else
        tree loc (List.map (\k -> fromLocationSet k locs3) seeds)


toPath : Tree Location -> List Location
toPath t =
    case t of
        Leaf x ->
            [ x ]

        Node x cs ->
            x :: (List.concat <| List.intersperse [ x ] <| List.map toPath cs) ++ [ x ]


toPolylinePoints : Tree Location -> String
toPolylinePoints t =
    let
        p =
            toPath t

        locToString ( x, y ) =
            String.join "," [ String.fromInt x, String.fromInt y ]
    in
    String.join " " <| List.map locToString p


toPolylinePointsWithConversion : (Location -> Location) -> Tree Location -> String
toPolylinePointsWithConversion f t =
    let
        p =
            toPath t |> List.map f

        locToString ( x, y ) =
            String.join "," [ String.fromInt x, String.fromInt y ]
    in
    String.join " " <| List.map locToString p
