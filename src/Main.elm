module Main exposing (circle, main)

import Browser
import Bytes exposing (Bytes)
import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings as Settings exposing (fill)
import Canvas.Settings.Line as Line
import Color exposing (Color)
import Color.Convert
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events as Events
import Image exposing (Image)
import Image.Color
import Position
import Random exposing (Seed)
import Task
import Time


maxPotential : { width : Float, height : Float } -> Int
maxPotential { width, height } =
    round (width + height) // 4


size : Float
size =
    150


zoom : Float
zoom =
    10


stepSize : { width : Float, height : Float } -> Int
stepSize { width, height } =
    1 + round ((width * height) / 1000)


lineWidth =
    zoom / 4


type alias Cell =
    { from : ( Int, Int )
    , to : List ( Int, Int )
    , potential : Int
    , color : Color
    }


type alias Model =
    { grid : Dict ( Int, Int ) (Maybe Cell)
    , player : ( Int, Int )
    , seed : Seed
    , running : Bool
    , potential : Int
    , width : Float
    , height : Float
    , image : Dict ( Int, Int ) Color
    }


type Msg
    = Frame
    | GotSeed Seed
    | Select
    | GotBytes Bytes
    | UploadedFile File


init : () -> ( Model, Cmd Msg )
init () =
    ( new { image = [], width = 1, height = 1 }
    , Random.independentSeed |> Random.generate GotSeed
    )


new : { image : List ( ( Int, Int ), Color ), width : Float, height : Float } -> Model
new flag =
    let
        ( x, y ) =
            ( round <| flag.width / 2, round <| flag.height / 2 )
    in
    { grid =
        flag.image
            |> setWhiteAs ( 90, 100 )
            |> Dict.fromList
            |> Dict.map (\_ _ -> Nothing)
    , player = ( x, y )
    , seed = Random.initialSeed 42
    , running = False
    , potential = maxPotential { width = flag.width, height = flag.height }
    , width = flag.width
    , height = flag.height
    , image = flag.image |> Dict.fromList
    }


add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


areLinked : ( Int, Int ) -> ( Int, Int ) -> Model -> Bool
areLinked p1 p2 model =
    [ ( p1, p2 ), ( p2, p1 ) ]
        |> List.any
            (\( pos1, pos2 ) ->
                model.grid
                    |> Dict.get pos1
                    |> Maybe.map
                        (Maybe.map (.from >> (==) pos2)
                            >> Maybe.withDefault False
                        )
                    |> Maybe.withDefault False
            )


validPositions : Model -> List ( Float, ( Int, Int ) )
validPositions model =
    let
        inbounds v maxV =
            0 < v && v < round maxV

        straights =
            model.player
                |> Position.neighbors
                    { validator =
                        \( x, y ) ->
                            inbounds x model.width
                                && inbounds y model.height
                                && (model.grid
                                        |> Dict.member ( x, y )
                                        |> not
                                   )
                    , directions = Position.squareDirections
                    }

        from =
            model.grid
                |> Dict.get model.player
                |> Maybe.andThen (Maybe.map .from)
                |> Maybe.withDefault model.player

        diagonals =
            model.player
                |> Position.neighbors
                    { validator =
                        \( x, y ) ->
                            let
                                sub ( x1, y1 ) ( x2, y2 ) =
                                    ( x2 - x1, y2 - y1 )

                                (( relX, relY ) as move) =
                                    ( x, y ) |> sub model.player
                            in
                            inbounds x model.width
                                && inbounds y model.height
                                && (model.grid
                                        |> Dict.member (move |> add model.player)
                                        |> not
                                   )
                                && ((( relX, 0 ) |> add model.player) /= from)
                                && ((( 0, relY ) |> add model.player) /= from)
                                && (model
                                        |> areLinked (( relX, 0 ) |> add model.player) (( 0, relY ) |> add model.player)
                                        |> not
                                   )
                    , directions = Position.diagonalDirections
                    }

        vecTo ( x1, y1 ) ( x2, y2 ) =
            ( x1 - x2, y1 - y2 )
    in
    (straights ++ diagonals)
        |> List.map
            (\pos ->
                ( if from |> vecTo model.player |> (==) (model.player |> vecTo pos) then
                    2

                  else
                    1
                , pos
                )
            )


applyMove : ( ( Int, Int ), Seed ) -> Model -> Model
applyMove ( newPlayer, seed ) model =
    let
        newPotential =
            model.potential - 2
    in
    { model
        | player = newPlayer
        , seed = seed
        , potential = newPotential
        , grid =
            model.grid
                |> Dict.update model.player
                    (Maybe.map
                        (Maybe.map
                            (\cell ->
                                { cell | to = newPlayer :: cell.to }
                            )
                        )
                    )
                |> Dict.insert newPlayer
                    ({ from = model.player
                     , to = []
                     , potential = newPotential
                     , color =
                        model.image
                            |> Dict.get newPlayer
                            |> Maybe.withDefault Color.black
                     }
                        |> Just
                    )
    }


moveBack : Model -> Model
moveBack model =
    let
        newPotential =
            min (model.potential + 1)
                (maxPotential { height = model.height, width = model.width })
    in
    case model.grid |> Dict.get model.player of
        Just (Just { from }) ->
            { model
                | player = from
                , potential = newPotential
                , grid =
                    model.grid
                        |> Dict.update model.player
                            (Maybe.map
                                (Maybe.map
                                    (\cell ->
                                        { cell | potential = newPotential }
                                    )
                                )
                            )
            }

        _ ->
            let
                ( pos, seed ) =
                    model.seed
                        |> Random.step
                            (Random.map2
                                (\x y -> ( x, y ))
                                (Random.int 0 (round model.width))
                                (Random.int 0 (round model.height))
                            )
            in
            if model.grid |> Dict.member pos then
                { model | seed = seed }

            else
                { model
                    | player = pos
                    , seed = seed
                    , grid = model.grid |> Dict.insert pos Nothing
                }


updateFrame : Model -> Model
updateFrame model =
    let
        moves =
            model
                |> validPositions
    in
    case moves of
        [] ->
            moveBack model

        head :: tail ->
            if model.potential > 0 then
                model
                    |> applyMove
                        (model.seed
                            |> Random.step (Random.weighted head tail)
                        )

            else
                moveBack { model | potential = maxPotential { width = model.width, height = model.height } // 2 }


convertImage : Image -> List ( ( Int, Int ), Color )
convertImage image =
    image
        |> Image.Color.toList2d
        |> List.indexedMap
            (\y list ->
                list
                    |> List.indexedMap (\x color -> ( ( x, y ), color ))
            )
        |> List.concat


setWhiteAs : ( Float, Float ) -> List ( a, Color ) -> List ( a, Color )
setWhiteAs ( minValue, maxValue ) =
    List.filter
        (\( _, color ) ->
            let
                { alpha } =
                    color
                        |> Color.toHsla
            in
            color
                |> Color.Convert.colorToLab
                |> (\{ l } ->
                        (alpha < 0.5)
                            || ((minValue <= l) && (l <= maxValue))
                   )
        )


resizeBy : Float -> List ( ( Int, Int ), Color ) -> List ( ( Int, Int ), Color )
resizeBy factor list =
    list
        |> List.foldl
            (\( ( x, y ), color ) ->
                Dict.update ( round (toFloat x * factor), round (toFloat y * factor) )
                    (\maybe ->
                        maybe
                            |> Maybe.map (\v -> { v | value = v.value + 1 })
                            |> Maybe.withDefault { value = 1, color = color }
                            |> Just
                    )
            )
            Dict.empty
        |> Dict.toList
        |> List.filterMap
            (\( pos, v ) ->
                if v.value > round ((1 / (factor * factor)) * 4 / 8) then
                    Just ( pos, v.color )

                else
                    Nothing
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame ->
            ( List.repeat (stepSize { width = model.width, height = model.height }) ()
                |> List.foldl (\() -> updateFrame)
                    model
            , Cmd.none
            )

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        Select ->
            ( model, Select.file [ "image/png", "image/bmp" ] UploadedFile )

        UploadedFile file ->
            ( model, file |> File.toBytes |> Task.perform GotBytes )

        GotBytes bytes ->
            case bytes |> Image.decode of
                Just image ->
                    let
                        dim =
                            image |> Image.dimensions

                        factor =
                            size / toFloat (max dim.width dim.height)

                        ( width, height ) =
                            ( round (toFloat dim.width * factor)
                            , round (toFloat dim.height * factor)
                            )

                        img =
                            image
                                |> convertImage
                                |> resizeBy factor
                    in
                    if img == [] then
                        ( model, Cmd.none ) |> Debug.log "no transparency"

                    else
                        ( new
                            { image = img
                            , width = toFloat width
                            , height = toFloat height
                            }
                            |> (\m ->
                                    let
                                        ( x, y ) =
                                            m.player
                                    in
                                    { m
                                        | grid =
                                            m.grid
                                                |> Dict.insert ( x, y ) Nothing
                                        , running =
                                            True
                                    }
                               )
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none ) |> Debug.log "no image"


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round <| model.width * zoom, round <| model.height * zoom )
            []
            --style "border" "10px solid rgba(0,0,0,0.1)" ]
            ([ clearScreen { width = model.width, height = model.height }
             , bigCircle
                { width = model.width
                , height = model.height
                , potential = model.potential
                , color = Color.black
                }
                model.player
             ]
                ++ (model.grid
                        |> Dict.toList
                        |> List.filterMap
                            (\( pos, maybe ) ->
                                maybe
                                    |> Maybe.map (\cell -> ( pos, cell ))
                            )
                        |> List.concatMap (viewCell { width = model.width, height = model.height })
                   )
            )
        , Html.button [ Events.onClick Select ] [ Html.text "Select Image" ]
        ]


clearScreen : { width : Float, height : Float } -> Renderable
clearScreen { width, height } =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) (width * zoom) (height * zoom) ]


adjustColor : { width : Float, height : Float } -> Int -> Color -> Color
adjustColor { width, height } potential color =
    let
        { l, a, b } =
            color
                |> Color.Convert.colorToLab

        _ =
            1 - toFloat potential / toFloat (maxPotential { width = width, height = height })
    in
    { l = max 0 (l * 90 / 100)
    , a = a
    , b = b
    }
        |> Color.Convert.labToColor



--Color.hsl hue saturation lightness


line : { width : Float, height : Float, potential : Int, color : Color } -> ( Int, Int ) -> ( Int, Int ) -> Renderable
line { width, height, potential, color } ( x1, y1 ) ( x2, y2 ) =
    Canvas.path ( toFloat x1 * zoom, toFloat y1 * zoom ) [ Canvas.lineTo ( toFloat x2 * zoom, toFloat y2 * zoom ) ]
        |> List.singleton
        |> shapes
            [ Settings.stroke (color |> adjustColor { width = width, height = height } potential)
            , Line.lineWidth lineWidth
            , Line.lineCap Line.RoundCap
            ]


bigCircle : { width : Float, height : Float, potential : Int, color : Color } -> ( Int, Int ) -> Renderable
bigCircle { width, height, potential, color } ( x, y ) =
    Canvas.circle ( toFloat x * zoom, toFloat y * zoom ) (lineWidth * 2)
        |> List.singleton
        |> shapes
            [ Settings.fill
                (color
                    |> adjustColor { width = width, height = height } potential
                )
            ]


circle : { width : Float, height : Float, potential : Int, color : Color } -> ( Int, Int ) -> Renderable
circle { width, height, potential, color } ( x, y ) =
    Canvas.circle ( toFloat x * zoom, toFloat y * zoom ) (lineWidth * 1.5)
        |> List.singleton
        |> shapes
            [ Settings.fill
                (color
                    |> adjustColor { width = width, height = height } potential
                )
            ]


viewCell : { width : Float, height : Float } -> ( ( Int, Int ), Cell ) -> List Renderable
viewCell { width, height } ( pos, cell ) =
    (cell.from
        |> line
            { width = width
            , height = height
            , potential = cell.potential
            , color = cell.color
            }
            pos
    )
        :: (if cell.to == [] then
                circle
                    { width = width
                    , height = height
                    , potential = cell.potential
                    , color = cell.color
                    }
                    pos
                    |> List.singleton

            else
                cell.to
                    |> List.map
                        (line
                            { width = width
                            , height = height
                            , potential = cell.potential
                            , color = cell.color
                            }
                            pos
                        )
           )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every 2 (always Frame)

    else
        Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
