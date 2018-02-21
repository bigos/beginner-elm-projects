module Main exposing (..)

---- IMPORTS ----
-- import Html.Attributes exposing (src, class, style)

import Html exposing (Html, div, h1, img, p, text)
import Keyboard exposing (downs)
import List exposing (..)
import Svg exposing (path, rect, svg)
import Svg.Attributes
    exposing
        ( d
        , fill
        , fillRule
        , height
        , rx
        , ry
        , stroke
        , strokeLinecap
        , strokeLinejoin
        , strokeWidth
        , viewBox
        , width
        , x
        , y
        )
import Time exposing (Time, second)


---- MODEL ----


type alias Coordinate =
    { x : Int
    , y : Int
    }


type alias FoodItems =
    List Coordinate


type alias Snake =
    List Coordinate


type alias Model =
    { heading : Heading
    , scale : Int
    , height : Int
    , snake : List Coordinate
    , foodItems : FoodItems
    , eaten : Int
    , width : Int
    , time : Maybe Time
    , lastKey : Maybe KeyControl
    , gameField : GameField
    , growthFactor : Int
    , tickInterval : Float
    , debugData : String
    }


type Heading
    = Up
    | Right
    | Down
    | Left


type KeyControl
    = KeyPause
    | KeyLeft
    | KeyUp
    | KeyRight
    | KeyDown
    | KeyOther


type GameField
    = Move
    | Feeding
    | Collision
    | Pause


init : ( Model, Cmd Msg )
init =
    ( { heading = Right
      , scale = 25 -- snake thickness and number of fields
      , height = 400
      , snake =
            [ { x = 6, y = 7 }
            , { x = 5, y = 7 }
            ]
      , foodItems = [ { x = 5, y = 5 }, { x = 7, y = 9 }, { x = 9, y = 12 } ]
      , eaten = 0
      , width = 600
      , time = Nothing
      , lastKey = Nothing
      , gameField = Move
      , growthFactor = 5
      , tickInterval = 0.5
      , debugData = ""
      }
    , Cmd.none
    )


shrinkInt : Int -> Int
shrinkInt n =
    if n == 0 then
        n
    else if n > 0 then
        n - 1
    else
        n + 1


gridWidth model =
    model.width // model.scale


gridHeight model =
    model.height // model.scale


gridCoordinates model =
    map
        (\x ->
            map
                (\y -> ( x, y ))
                (range 0 (gridHeight model - 1))
        )
        (range 0 (gridWidth model - 1))


foodUnderHead c model =
    let
        x =
            (unjustify (head model.snake)).x

        y =
            (unjustify (head model.snake)).y
    in
    c.x == x && c.y == y


foodEaten model =
    let
        x =
            (unjustify (head model.snake)).x

        y =
            (unjustify (head model.snake)).y
    in
    member
        True
        (map (\c -> c.x == x && c.y == y) model.foodItems)


headBitSnake model =
    let
        x =
            (unjustify (head model.snake)).x

        y =
            (unjustify (head model.snake)).y
    in
    member True (map (\c -> c.x == x && c.y == y) (drop 1 model.snake))


headHitWall model =
    let
        h =
            head model.snake
    in
    let
        x =
            (unjustify h).x

        y =
            (unjustify h).y
    in
    x
        <= 0
        || y
        <= 0
        || x
        >= gridWidth model
        || y
        >= gridHeight model


detectCollision model =
    if headHitWall model || headBitSnake model then
        Collision
    else
        model.gameField



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time
    | Keypress Keyboard.KeyCode


cook model =
    if foodEaten model then
        { model
            | gameField = detectCollision model
            , growthFactor = model.growthFactor + 3
            , foodItems = filter (\c -> not (foodUnderHead c model)) model.foodItems
            , debugData = toString ( "** eaten **", head model.snake, model.foodItems )
            , eaten = model.eaten + 1
        }
    else
        { model
            | gameField = detectCollision model
            , growthFactor = shrinkInt model.growthFactor
            , debugData = ""
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg rawModel =
    let
        model =
            cook rawModel
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model
                | time = Just newTime
                , gameField = updateGamefield model (unjust model.lastKey)
                , snake = moveSnake model model.heading
              }
            , Cmd.none
            )

        Keypress key ->
            let
                kk =
                    keyControl key
            in
            ( { model
                | lastKey = Just kk
                , heading = heading model kk
                , gameField = updateGamefield model kk
                , snake = moveSnake model (heading model kk)
              }
            , Cmd.none
            )


updateGamefield model kk =
    if model.gameField == Pause then
        if kk /= KeyPause then
            Move
        else
            Pause
    else if kk == KeyPause then
        Pause
    else
        model.gameField


unjust : Maybe KeyControl -> KeyControl
unjust x =
    Maybe.withDefault KeyRight x


keyControl : Keyboard.KeyCode -> KeyControl
keyControl keycode =
    case keycode of
        32 ->
            KeyPause

        37 ->
            KeyLeft

        38 ->
            KeyUp

        39 ->
            KeyRight

        40 ->
            KeyDown

        _ ->
            KeyOther


heading : Model -> KeyControl -> Heading
heading model kc =
    case kc of
        KeyOther ->
            model.heading

        KeyPause ->
            model.heading

        KeyLeft ->
            Left

        KeyUp ->
            Up

        KeyRight ->
            Right

        KeyDown ->
            Down


butLast : List a -> List a
butLast list =
    take (length list - 1) list


unjustify : Maybe Coordinate -> Coordinate
unjustify e =
    Maybe.withDefault { x = 0, y = 0 } e


snakeGrower growth snake =
    case compare growth 0 of
        GT ->
            snake

        -- do not remove last segment thus making snake grow
        EQ ->
            butLast snake

        LT ->
            butLast (butLast snake)


moveSnake : Model -> Heading -> List Coordinate
moveSnake model heading =
    if model.gameField == Pause || model.gameField == Collision then
        model.snake
    else
        moveSnake2 model heading


moveSnake2 model heading =
    let
        snake =
            model.snake

        growth =
            model.growthFactor

        uhs =
            unjustify (head snake)
    in
    case heading of
        Left ->
            { x = uhs.x - 1
            , y = uhs.y
            }
                :: snakeGrower growth snake

        Up ->
            { x = uhs.x
            , y = uhs.y - 1
            }
                :: snakeGrower growth snake

        Right ->
            { x = uhs.x + 1
            , y = uhs.y
            }
                :: snakeGrower growth snake

        Down ->
            { x = uhs.x
            , y = uhs.y + 1
            }
                :: snakeGrower growth snake



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm beginner's snake" ]

        --, p [] [ text (toString model) ]
        , p [] [ text "Use arrow keys to control the snake and space bar to pause" ]
        , p [] [ text ("Going " ++ toString model.heading) ]
        , div [] [ gameField model ]
        , p []
            [ text ("Eaten " ++ toString model.eaten) ]
        , p []
            [ text
                (if model.gameField == Collision then
                    "Game Over"
                 else if model.gameField == Pause then
                    "Paused, press an arrow key to resume"
                 else
                    ""
                )
            ]
        ]


gameField : Model -> Html.Html msg
gameField model =
    svg
        [ width (toString model.width)
        , height (toString model.height)
        , viewBox ("0 0 " ++ toString model.width ++ " " ++ toString model.height)
        ]
        ([ rect
            -- game rectangle
            [ x "0"
            , y "0"
            , width (toString model.width)
            , height (toString model.height)
            , rx "5"
            , ry "5"
            , fill
                (if model.gameField == Move then
                    "#042"
                 else if model.gameField == Pause then
                    "#8b8"
                 else
                    "#f04"
                )
            ]
            []
         ]
            ++ map
                --food items
                (\c ->
                    rect
                        --food item
                        [ x (nc model (c.x - 1))
                        , y (nc model (c.y - 1))
                        , width (toString model.scale)
                        , height (toString model.scale)
                        , rx (toString (model.scale // 2))
                        , ry (toString (model.scale // 2))
                        , fill "#ffa"
                        ]
                        []
                )
                model.foodItems
            ++ [ path
                    -- snake
                    [ fill "none"
                    , fillRule "evenodd"
                    , stroke "#fa4"
                    , strokeWidth (toString (model.scale - 1))
                    , strokeLinecap "round"
                    , strokeLinejoin "round"
                    , d (buildMcoords model) --snake segments
                    ]
                    []
               ]
        )


nc model i =
    toString (i * model.scale + model.scale // 2)


buildMcoords : Model -> String
buildMcoords model =
    List.foldl (\v a -> a ++ buildOneCoord model v) "M " model.snake


buildOneCoord model v =
    toString (v.x * model.scale) ++ "," ++ toString (v.y * model.scale) ++ " "



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (model.tickInterval * second) Tick
        , Keyboard.downs Keypress
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
