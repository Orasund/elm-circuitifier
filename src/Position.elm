module Position exposing (PositionOps, diagonalDirections, hexagonalDirections, hexagonalOps, neighbors, squareOps, triangluarOps)


type alias PositionOps position =
    { directions : List (position -> position)
    , toPoint : position -> ( Float, Float )
    , zero : position
    }


squareOps : PositionOps ( Int, Int )
squareOps =
    { directions =
        [ \( x, y ) -> ( x + 1, y )
        , \( x, y ) -> ( x - 1, y )
        , \( x, y ) -> ( x, y + 1 )
        , \( x, y ) -> ( x, y - 1 )
        ]
    , toPoint = Tuple.mapBoth toFloat toFloat
    , zero = ( 0, 0 )
    }


triangluarOps : PositionOps ( Int, Int, Int )
triangluarOps =
    { directions =
        [ \( x, y, b ) ->
            ( x
            , y + b
            , -b
            )
        , \( x, y, b ) -> ( x + 1, y, -b )
        , \( x, y, b ) -> ( x - 1, y, -b )
        ]
    , toPoint =
        \( x, y, b ) ->
            ( x, y )
                |> squareOps.toPoint
                |> (if b > 0 then
                        identity

                    else
                        Tuple.mapBoth identity ((+) -(sqrt 3 / 6))
                   )
    , zero = ( 0, 0, 1 )
    }


hexagonalOps : PositionOps ( Int, Int )
hexagonalOps =
    { directions =
        squareOps.directions
            ++ [ \( x, y ) ->
                    ( internalApplyBool (internalIsEven y) x
                    , y - 1
                    )
               , \( x, y ) ->
                    ( internalApplyBool (internalIsEven y) x
                    , y + 1
                    )
               ]
    , toPoint =
        \( x, y ) ->
            ( x, y )
                |> squareOps.toPoint
                |> (if not <| internalIsEven y then
                        Tuple.mapFirst ((+) -0.5)

                    else
                        identity
                   )
    , zero = ( 0, 0 )
    }


neighbors :
    { directions : List (pos -> pos), validator : pos -> Bool }
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
        (+) -1


internalIsEven : Int -> Bool
internalIsEven n =
    modBy 2 n == 0
