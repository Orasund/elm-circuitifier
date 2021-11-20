module Position exposing (PositionOps, diagonalDirections, hexagonalDirections, neighbors, squareOps, triangluarOps)


type alias PositionOps position =
    { directions : List (position -> position)
    , toPoint : position -> ( Float, Float )
    }


squareOps : PositionOps ( Int, Int )
squareOps =
    { directions =
        [ \( x, y ) -> ( x + 1, y )
        , \( x, y ) -> ( x - 1, y )
        , \( x, y ) -> ( x, y + 1 )
        , \( x, y ) -> ( x, y - 1 )
        ]
    , toPoint =
        Tuple.mapBoth toFloat toFloat
    }


triangluarOps : PositionOps ( Int, Int, Bool )
triangluarOps =
    { directions =
        [ \( x, y, b ) ->
            ( x
            , internalApplyBool b y
            , not b
            )
        , \( x, y, b ) -> ( x + 1, y, not b )
        , \( x, y, b ) -> ( x - 1, y, not b )
        ]
    , toPoint =
        \( x, y, b ) ->
            ( x, y )
                |> squareOps.toPoint
                |> (if internalIsEven y then
                        identity

                    else
                        Tuple.mapBoth ((+) 0.5) ((*) (sqrt 3 / 2))
                   )
                |> (if b then
                        identity

                    else
                        Tuple.mapBoth ((+) 0.5) ((+) -(sqrt 3 / 6))
                   )
    }


neighbors :
    { validator : pos -> Bool
    , directions : List (pos -> pos)
    }
    -> pos
    -> List pos
neighbors args pos =
    args.directions
        |> List.filterMap
            (\f ->
                let
                    newPos =
                        f pos
                in
                if newPos |> args.validator then
                    Just newPos

                else
                    Nothing
            )


diagonalDirections : List (( Int, Int ) -> ( Int, Int ))
diagonalDirections =
    [ \( x, y ) -> ( x + 1, y + 1 )
    , \( x, y ) -> ( x - 1, y - 1 )
    , \( x, y ) -> ( x - 1, y + 1 )
    , \( x, y ) -> ( x + 1, y - 1 )
    ]


hexagonalDirections : List (( Int, Int ) -> ( Int, Int ))
hexagonalDirections =
    squareOps.directions
        ++ [ \( x, y ) ->
                ( internalApplyBool (internalIsEven x) x
                , y
                )
           , \( x, y ) ->
                ( internalApplyBool (not <| internalIsEven x) x
                , y
                )
           ]


internalApplyBool : Bool -> Int -> Int
internalApplyBool b =
    if b then
        (+) 1

    else
        (-) 1


internalIsEven : Int -> Bool
internalIsEven n =
    modBy 2 n == 0
