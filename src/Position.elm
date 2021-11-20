module Position exposing (diagonalDirections, hexagonalDirections, neighbors, squareDirections, triangluarDirections)


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


triangluarDirections : List (( Int, Int, Bool ) -> ( Int, Int, Bool ))
triangluarDirections =
    [ \( x, y, b ) ->
        ( x
        , internalApplyBool b y
        , not b
        )
    , \( x, y, b ) -> ( x + 1, y, not b )
    , \( x, y, b ) -> ( x - 1, y, not b )
    ]


squareDirections : List (( Int, Int ) -> ( Int, Int ))
squareDirections =
    [ \( x, y ) -> ( x + 1, y )
    , \( x, y ) -> ( x - 1, y )
    , \( x, y ) -> ( x, y + 1 )
    , \( x, y ) -> ( x, y - 1 )
    ]


diagonalDirections : List (( Int, Int ) -> ( Int, Int ))
diagonalDirections =
    [ \( x, y ) -> ( x + 1, y + 1 )
    , \( x, y ) -> ( x - 1, y - 1 )
    , \( x, y ) -> ( x - 1, y + 1 )
    , \( x, y ) -> ( x + 1, y - 1 )
    ]


hexagonalDirections : List (( Int, Int ) -> ( Int, Int ))
hexagonalDirections =
    let
        isEven : Int -> Bool
        isEven n =
            modBy 2 n == 0
    in
    squareDirections
        ++ [ \( x, y ) ->
                ( internalApplyBool (isEven x) x
                , y
                )
           , \( x, y ) ->
                ( internalApplyBool (not <| isEven x) x
                , y
                )
           ]


internalApplyBool : Bool -> Int -> Int
internalApplyBool b =
    if b then
        (+) 1

    else
        (-) 1
