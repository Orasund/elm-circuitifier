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


maxPotential : { height : Float, width : Float } -> Int
maxPotential { height, width } =
    round (width + height) // 4


size : Float
size =
    150


zoom : Float
zoom =
    10


stepSize : { height : Float, width : Float } -> Int
stepSize { height, width } =
    1 + round ((width * height) / 1000)


lineWidth : Float
lineWidth =
    zoom / 4


type alias Cell =
    { color : Color, from : Position, potential : Int, to : List Position }


type alias Model =
    { grid : Dict Position (Maybe Cell), height : Float, image : Dict ( Int, Int ) Color, player : Position, potential : Int, running : Bool, seed : Seed, width : Float }


type Msg
    = Frame
    | GotBytes Bytes
    | GotSeed Seed
    | Select
    | UploadedFile File


init : () -> ( Model, Cmd Msg )
init () =
    ( new { height = 1, image = [], width = 1 }
    , Random.independentSeed |> Random.generate GotSeed
    )


new : { height : Float, image : List ( ( Int, Int ), Color ), width : Float } -> Model
new flag =
    { grid = flag.image |> setWhiteAs ( 90, 100 ) |> List.map (\( ( x, y ), _ ) -> ( ( x, y ), Nothing )) |> Dict.fromList
    , height = flag.height
    , image = flag.image |> Dict.fromList
    , player = positionOps.zero
    , potential = maxPotential { height = flag.height, width = flag.width }
    , running = False
    , seed = Random.initialSeed 42
    , width = flag.width
    }


validPositions : Model -> List ( Float, Position )
validPositions model =
    let
        inbounds v maxV =
            0 < v && v < round maxV

        straights =
            model.player
                |> Position.neighbors
                    { directions = positionOps.directions
                    , validator =
                        \( x, y ) ->
                            inbounds x model.width
                                && inbounds y model.height
                                && (model.grid |> Dict.member ( x, y ) |> not)
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
        | grid =
            model.grid
                |> Dict.update model.player
                    (Maybe.map (Maybe.map (\cell -> { cell | to = newPlayer :: cell.to })))
                |> Dict.insert newPlayer
                    ({ color = model.image |> Dict.get (newPlayer |> positionOps.toPoint |> Tuple.mapBoth round round) |> Maybe.withDefault Color.black
                     , from = model.player
                     , potential = newPotential
                     , to = []
                     }
                        |> Just
                    )
        , player = newPlayer
        , potential = newPotential
        , seed = seed
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
                | grid =
                    model.grid
                        |> Dict.update model.player
                            (Maybe.map (Maybe.map (\cell -> { cell | potential = newPotential })))
                , player = from
                , potential = newPotential
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
                    | grid = model.grid |> Dict.insert pos Nothing
                    , player = pos
                    , seed = seed
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
                moveBack { model | potential = maxPotential { height = model.height, width = model.width } // 2 }


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
                            |> Maybe.withDefault { color = color, value = 1 }
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
            ( List.repeat (stepSize { height = model.height, width = model.width }) ()
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
                            { height = toFloat height
                            , image = img
                            , width = toFloat width
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
            ( round (model.width * zoom), round (model.height * zoom) )
            []
            --style "border" "10px solid rgba(0,0,0,0.1)" ]
            ([ clearScreen { height = model.height, width = model.width }
             , bigCircle
                { color = Color.black
                , height = model.height
                , potential = model.potential
                , width = model.width
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
                        |> List.concatMap (viewCell { height = model.height, width = model.width })
                   )
            )
        , Html.button [ Events.onClick Select ] [ Html.text "Select Image" ]
        ]


clearScreen : { height : Float, width : Float } -> Renderable
clearScreen { height, width } =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) (width * zoom) (height * zoom) ]


adjustColor : { height : Float, width : Float } -> Int -> Color -> Color
adjustColor { height, width } potential color =
    let
        { a, b, l } =
            color
                |> Color.Convert.colorToLab

        _ =
            1 - toFloat potential / toFloat (maxPotential { height = height, width = width })
    in
    { a = a
    , b = b
    , l = max 0 (l * 90 / 100)
    }
        |> Color.Convert.labToColor



--Color.hsl hue saturation lightness


pointToPixel : { height : Float, width : Float } -> ( Float, Float ) -> ( Float, Float )
pointToPixel args ( x, y ) =
    ( args.width / 2 + x * zoom, args.height / 2 + y * zoom )


line : { color : Color, height : Float, potential : Int, width : Float } -> ( Float, Float ) -> ( Float, Float ) -> Renderable
line { color, height, potential, width } p1 p2 =
    let
        dimensions =
            { height = height
            , width = width
            }
    in
    Canvas.path (p1 |> pointToPixel dimensions) [ Canvas.lineTo (p2 |> pointToPixel dimensions) ]
        |> List.singleton
        |> shapes
            [ Settings.stroke (color |> adjustColor dimensions potential)
            , Line.lineWidth lineWidth
            , Line.lineCap Line.RoundCap
            ]


bigCircle : { color : Color, height : Float, potential : Int, width : Float } -> ( Float, Float ) -> Renderable
bigCircle { color, height, potential, width } ( x, y ) =
    let
        dimensions =
            { height = height
            , width = width
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


circle : { color : Color, height : Float, potential : Int, width : Float } -> ( Float, Float ) -> Renderable
circle { color, height, potential, width } p =
    let
        dimensions =
            { height = height
            , width = width
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


viewCell : { height : Float, width : Float } -> ( ( Float, Float ), Cell ) -> List Renderable
viewCell { height, width } ( pos, cell ) =
    (cell.from
        |> positionOps.toPoint
        |> line
            { color = cell.color
            , height = height
            , potential = cell.potential
            , width = width
            }
            pos
    )
        :: (if cell.to == [] then
                circle
                    { color = cell.color
                    , height = height
                    , potential = cell.potential
                    , width = width
                    }
                    pos
                    |> List.singleton

            else
                cell.to
                    |> List.map positionOps.toPoint
                    |> List.map
                        (line
                            { color = cell.color
                            , height = height
                            , potential = cell.potential
                            , width = width
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
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
