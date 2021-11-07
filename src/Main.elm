module Main exposing (circle, main)

import Bag
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Bytes exposing (Bytes)
import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings as Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Canvas.Settings.Line as Line
import Color exposing (Color)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Http
import Image exposing (Image)
import Image.Color
import Random exposing (Seed)
import Task
import Time


link =
    "https://image.spreadshirtmedia.net/image-server/v1/mp/compositions/T1303A1MPA3323PT24X0Y0D163964850FS6360/views/1,width=378,height=378,appearanceId=1,backgroundColor=FFFFFF,noPt=true/psychedelic-poster.jpg"


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
    }


newCell : ( Int, Int ) -> Int -> Cell
newCell from potential =
    { from = from, to = [], potential = potential }


type alias Model =
    { grid : Dict ( Int, Int ) (Maybe Cell)
    , player : ( Int, Int )
    , seed : Seed
    , running : Bool
    , potential : Int
    , width : Float
    , height : Float
    }


type Msg
    = Frame
    | GotSeed Seed
    | Select
    | GotBytes Bytes
    | UploadedFile File


init : () -> ( Model, Cmd Msg )
init () =
    ( new { grid = Dict.empty, width = 1, height = 1 }
    , Random.independentSeed |> Random.generate GotSeed
    )


new : { grid : Dict ( Int, Int ) (Maybe Cell), width : Float, height : Float } -> Model
new flag =
    let
        ( x, y ) =
            ( round <| flag.width / 2, round <| flag.height / 2 )
    in
    { grid = flag.grid
    , player = ( x, y )
    , seed = Random.initialSeed 42
    , running = False
    , potential = maxPotential { width = flag.width, height = flag.height }
    , width = flag.width
    , height = flag.height
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


validMoves : Model -> List ( Float, ( Int, Int ) )
validMoves model =
    let
        inbounds v maxV =
            0 < v && v < round maxV

        straights =
            [ ( 1, 0 )
            , ( 0, 1 )
            , ( -1, 0 )
            , ( 0, -1 )
            ]
                |> List.filter
                    (\move ->
                        let
                            ( x, y ) =
                                move |> add model.player
                        in
                        inbounds x model.width
                            && inbounds y model.height
                            && (model.grid
                                    |> Dict.member (move |> add model.player)
                                    |> not
                               )
                    )

        from =
            model.grid
                |> Dict.get model.player
                |> Maybe.andThen (Maybe.map .from)
                |> Maybe.withDefault model.player

        diagonals =
            [ ( 1, 1 )
            , ( -1, 1 )
            , ( -1, -1 )
            , ( 1, -1 )
            ]
                |> List.filter
                    (\(( relX, relY ) as move) ->
                        let
                            ( x, y ) =
                                move |> add model.player
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
                    )

        vecTo ( x1, y1 ) ( x2, y2 ) =
            ( x1 - x2, y1 - y2 )
    in
    (straights ++ diagonals)
        |> List.map
            (\pos ->
                ( if from |> vecTo model.player |> (==) pos then
                    2

                  else
                    1
                , pos
                )
            )


applyMove : ( ( Int, Int ), Seed ) -> Model -> Model
applyMove ( move, seed ) model =
    let
        newPlayer =
            move |> add model.player

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
                |> Dict.insert newPlayer (newCell model.player newPotential |> Just)
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
                |> validMoves
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
                    |> List.indexedMap
                        (\x color ->
                            ( ( x, y ), color )
                        )
            )
        |> List.concat


toBinary : ( Float, Float ) -> List ( ( Int, Int ), Color ) -> List ( Int, Int )
toBinary ( minValue, maxValue ) =
    List.filterMap
        (\( ( x, y ), color ) ->
            color
                |> Color.toHsla
                |> .lightness
                |> (\value ->
                        if minValue <= value && value <= maxValue then
                            Just ( x, y )

                        else
                            Nothing
                   )
        )


resizeBy : Float -> List ( Int, Int ) -> List ( Int, Int )
resizeBy factor list =
    list
        |> List.foldl
            (\( x, y ) -> Bag.insert 1 ( round (toFloat x * factor), round (toFloat y * factor) ))
            Bag.empty
        |> Bag.toList
        |> List.filterMap
            (\( pos, v ) ->
                if v > round ((1 / (factor * factor)) * 4 / 8) then
                    Just pos

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

                        grid =
                            image
                                |> convertImage
                                |> toBinary ( 0.9, 1 )
                                |> resizeBy factor
                                |> List.map (\pos -> ( pos, Nothing ))
                                |> Dict.fromList
                    in
                    if grid == Dict.empty then
                        ( model, Cmd.none ) |> Debug.log "no transparency"

                    else
                        ( new
                            { grid = grid
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
             , bigCircle { width = model.width, height = model.height, potential = model.potential } model.player
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


toColor : { width : Float, height : Float } -> Int -> Color
toColor { width, height } potential =
    let
        muliplier =
            1 - toFloat potential / toFloat (maxPotential { width = width, height = height })

        l =
            muliplier / 8 + 1 / 8
    in
    Color.hsl 0 0 l


line : { width : Float, height : Float, potential : Int } -> ( Int, Int ) -> ( Int, Int ) -> Renderable
line { width, height, potential } ( x1, y1 ) ( x2, y2 ) =
    Canvas.path ( toFloat x1 * zoom, toFloat y1 * zoom ) [ Canvas.lineTo ( toFloat x2 * zoom, toFloat y2 * zoom ) ]
        |> List.singleton
        |> shapes
            [ Settings.stroke (toColor { width = width, height = height } potential)
            , Line.lineWidth lineWidth
            , Line.lineCap Line.RoundCap
            ]


bigCircle : { width : Float, height : Float, potential : Int } -> ( Int, Int ) -> Renderable
bigCircle { width, height, potential } ( x, y ) =
    Canvas.circle ( toFloat x * zoom, toFloat y * zoom ) (lineWidth * 2)
        |> List.singleton
        |> shapes [ Settings.fill (toColor { width = width, height = height } potential) ]


circle : { width : Float, height : Float, potential : Int } -> ( Int, Int ) -> Renderable
circle { width, height, potential } ( x, y ) =
    Canvas.circle ( toFloat x * zoom, toFloat y * zoom ) (lineWidth * 1.5)
        |> List.singleton
        |> shapes [ Settings.fill (toColor { width = width, height = height } potential) ]


viewCell : { width : Float, height : Float } -> ( ( Int, Int ), Cell ) -> List Renderable
viewCell { width, height } ( pos, cell ) =
    (cell.from
        |> line
            { width = width
            , height = height
            , potential = cell.potential
            }
            pos
    )
        :: (if cell.to == [] then
                circle
                    { width = width
                    , height = height
                    , potential = cell.potential
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
                            }
                            pos
                        )
           )
