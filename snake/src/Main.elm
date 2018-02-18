module Main exposing (..)

---- IMPORTS ----

import Html exposing (Html, text, div, h1, img, p)
import Html.Attributes exposing (src, class, style)
import List exposing (..)
import Time exposing (Time, second)
import Keyboard exposing (downs)
import Svg exposing (svg, rect, path)
import Svg.Attributes exposing (stroke, width, height, viewBox, x, y, rx, ry
                               , fill, fillRule, strokeWidth, strokeLinecap
                               , strokeLinejoin, d)

---- MODEL ----

type alias Coordinate = { x : Int
                        , y : Int
                        }

type alias Snake = List Coordinate

type alias Model = { heading : Heading
                   , scale : Int
                   , height : Int
                   , snake : List Coordinate
                   , width : Int
                   , time : Maybe Time
                   , lastKey : Maybe KeyControl
                   , gameField : GameField
                   }

type Heading = Up
             | Right
             | Down
             | Left

type KeyControl = KeyPause
                | KeyLeft
                | KeyUp
                | KeyRight
                | KeyDown
                | KeyOther

type GameField = Play
               | Collision

init : ( Model, Cmd Msg )
init =
    ( { heading = Up
      , scale = 35 -- snake thickness and number of fields
      , height = 400
      , snake = [ {x = 6, y = 7}
                , {x = 5, y = 7}
                , {x = 4, y = 7}
                , {x = 3, y = 7}
                , {x = 2, y = 7}
                ]
      , width = 600
      , time = Nothing
      , lastKey = Nothing
      , gameField = Play
      }
    , Cmd.none
    )

headBitSnake model =
    let
        x = (unjustify (head model.snake)).x
        y = (unjustify (head model.snake)).y
    in
        member True (map (\c -> c.x == x && c.y == y) (drop 1 model.snake))

headHitWall model =
    let
        h = head model.snake
    in
        let
            x = (unjustify h).x
            y = (unjustify h).y
        in
            (x <= 0
             || y <=0
             || x >= (model.width//model.scale)
             || y >= (model.height//model.scale))

stateGamefield  model =
    let
        h = head model.snake
    in
        let
            x = (unjustify h).x
            y = (unjustify h).y
        in
            if headHitWall model || headBitSnake model
            then Collision
            else Play

---- UPDATE ----


type Msg
    = NoOp
      | Tick Time
      | Keypress Keyboard.KeyCode

cook model =
    { model |
      gameField = stateGamefield model
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update   msg    rawModel =
    let
        model = cook rawModel
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )
            Tick newTime ->
                ( { model | time = Just newTime }, Cmd.none)
            Keypress key ->
                let kk = keyControl key
                in
                    ( { model |
                        lastKey = Just kk,
                        heading = (heading model kk),
                        snake   = moveSnake model.snake (heading model kk) kk
                      }
                , Cmd.none)

keyControl : Keyboard.KeyCode -> KeyControl
keyControl keycode =
    case keycode of
        32 -> KeyPause
        37 -> KeyLeft
        38 -> KeyUp
        39 -> KeyRight
        40 -> KeyDown
        _  -> KeyOther

heading : Model -> KeyControl -> Heading
heading   model    kc =
    case kc of
        KeyOther -> model.heading
        KeyPause -> model.heading
        KeyLeft  -> Left
        KeyUp    -> Up
        KeyRight -> Right
        KeyDown  -> Down

butLast : List a -> List a
butLast   list =
    take ((length list) - 1) list

unjustify : Maybe Coordinate -> Coordinate
unjustify e =
    case e of
        Nothing ->
            { x=0, y=0 }        --!!!!!!!!!!!!!!!!!!!!
        Just e ->
            e

moveSnake : List Coordinate -> Heading -> KeyControl -> List Coordinate
moveSnake   snake              heading    kc =
    case heading of
        Left ->
            { x = (unjustify (head snake)).x - 1
            , y = (unjustify (head snake)).y } :: butLast snake
        Up ->
            { x = (unjustify (head snake)).x
            , y = (unjustify (head snake)).y - 1 } :: butLast snake

        Right ->
            { x = (unjustify (head snake)).x + 1
            , y = (unjustify (head snake)).y } :: butLast snake

        Down ->
            { x = (unjustify (head snake)).x
            , y = (unjustify (head snake)).y + 1 } :: butLast snake



---- VIEW ----

view : Model -> Html Msg
view model =
    div []
        [ h1  [] [ text "Your Elm App is working!" ]
        , p   [] [ text (toString model) ]
        , div [] [ gameField model ]
        ]

gameField : Model -> Html.Html msg
gameField   model    =
    svg
    [ width (toString model.width)
    , height (toString model.height)
    , viewBox ( "0 0 " ++ (toString model.width) ++ " " ++ (toString model.height))
    ]
-- game rectangle
    [ rect [ x "0", y "0"
           , width (toString model.width)
           , height (toString model.height)
           , rx "5", ry "5", fill (if model.gameField == Play then "#042" else "#f04")
           ] []
    -- snake
    , path [ fill "none", fillRule "evenodd"
           , stroke "#fa4"
           , strokeWidth (toString (model.scale-1))
           , strokeLinecap "round", strokeLinejoin "round"
           , d (buildMcoords model) --snake segments
           ] []
    ]

buildMcoords : Model -> String
buildMcoords model =
    List.foldl (\v a -> a ++ (buildOneCoord model v)) "M " model.snake

buildOneCoord model v =
    (toString (v.x*model.scale)) ++ "," ++ (toString (v.y*model.scale)) ++ " "


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
         Time.every second Tick
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
