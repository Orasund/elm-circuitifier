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
import Position exposing (PositionOps)
import Random exposing (Seed)
import Task
import Time


type alias Position =
    ( Int, Int )


positionOps : PositionOps Position
positionOps =
    Position.squareOps


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
    { from : Position
    , to : List Position
    , potential : Int
    , color : Color
    }


type alias Model =
    { grid : Dict Position (Maybe Cell)
    , player : Position
    , seed : Seed
    , running : Bool
    , potential : Int
    , width : Float
    , height : Float
    , image : Dict ( Int, Int ) Color
    }


type Msg
    = Frame
    | GotBytes Bytes
    | GotSeed Seed
    | Select
    | UploadedFile File


init : () -> ( Model, Cmd Msg )
init () =
    ( new { image = [], width = 1, height = 1 }
    , Random.independentSeed |> Random.generate GotSeed
    )


new : { image : List ( ( Int, Int ), Color ), width : Float, height : Float } -> Model
new flag =
    { grid =
        flag.image
            |> setWhiteAs ( 90, 100 )
            |> List.map (\( ( x, y ), _ ) -> ( ( x, y ), Nothing ))
            |> Dict.fromList
    , player = positionOps.zero
    , seed = Random.initialSeed 42
    , running = False
    , potential = maxPotential { width = flag.width, height = flag.height }
    , width = flag.width
    , height = flag.height
    , image = flag.image |> Dict.fromList
    }


validPositions : Model -> List ( Float, Position )
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
                    , directions = positionOps.directions
                    }

        from =
            model.grid
                |> Dict.get model.player
                |> Maybe.andThen (Maybe.map .from)
                |> Maybe.withDefault model.player

        vecTo ( x1, y1 ) ( x2, y2 ) =
            ( x1 - x2, y1 - y2 )
    in
    straights
        |> List.map
            (\pos ->
                ( if from |> vecTo model.player |> (==) (model.player |> vecTo pos) then
                    2

                  else
                    1
                , pos
                )
            )


applyMove : ( Position, Seed ) -> Model -> Model
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
                            |> Dict.get
                                (newPlayer
                                    |> positionOps.toPoint
                                    |> Tuple.mapBoth round round
                                )
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
                            image |> convertImage |> resizeBy factor
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
                                    { m
                                        | grid = m.grid |> Dict.insert m.player Nothing
                                        , running = True
                                    }
                               )
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none ) |> Debug.log "no image"

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        Select ->
            ( model, Select.file [ "image/png", "image/bmp" ] UploadedFile )

        UploadedFile file ->
            ( model, file |> File.toBytes |> Task.perform GotBytes )


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
                (positionOps.toPoint model.player)
             ]
                ++ (model.grid
                        |> Dict.toList
                        |> List.filterMap
                            (\( pos, maybe ) ->
                                maybe
                                    |> Maybe.map (\cell -> ( positionOps.toPoint pos, cell ))
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


pointToPixel : { width : Float, height : Float } -> ( Float, Float ) -> ( Float, Float )
pointToPixel args ( x, y ) =
    ( args.width / 2 + x * zoom, args.height / 2 + y * zoom )


line : { width : Float, height : Float, potential : Int, color : Color } -> ( Float, Float ) -> ( Float, Float ) -> Renderable
line { width, height, potential, color } p1 p2 =
    let
        dimensions =
            { width = width
            , height = height
            }
    in
    Canvas.path (p1 |> pointToPixel dimensions) [ Canvas.lineTo (p2 |> pointToPixel dimensions) ]
        |> List.singleton
        |> shapes
            [ Settings.stroke (color |> adjustColor dimensions potential)
            , Line.lineWidth lineWidth
            , Line.lineCap Line.RoundCap
            ]


bigCircle : { width : Float, height : Float, potential : Int, color : Color } -> ( Float, Float ) -> Renderable
bigCircle { width, height, potential, color } ( x, y ) =
    let
        dimensions =
            { width = width
            , height = height
            }
    in
    Canvas.circle ( x, y ) (lineWidth * 2)
        |> List.singleton
        |> shapes
            [ Settings.fill
                (color
                    |> adjustColor dimensions potential
                )
            ]


circle : { width : Float, height : Float, potential : Int, color : Color } -> ( Float, Float ) -> Renderable
circle { width, height, potential, color } p =
    let
        dimensions =
            { width = width
            , height = height
            }
    in
    Canvas.circle (p |> pointToPixel dimensions) (lineWidth * 1.5)
        |> List.singleton
        |> shapes
            [ Settings.fill
                (color
                    |> adjustColor dimensions potential
                )
            ]


viewCell : { width : Float, height : Float } -> ( ( Float, Float ), Cell ) -> List Renderable
viewCell { width, height } ( pos, cell ) =
    (cell.from
        |> positionOps.toPoint
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
                    |> List.map positionOps.toPoint
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
        Time.every 2 (\_ -> Frame)

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
