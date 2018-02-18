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
                   , height : Int
                   , snake : List Coordinate
                   , width : Int
                   , time : Maybe Time
                   , lastKey : Maybe KeyControl
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

init : ( Model, Cmd Msg )
init =
    ( { heading = Up
      , height = 5
      , snake = [ {x = 2, y = 2}
                , {x = 2, y = 3}
                , {x = 2, y = 4}
                , {x = 2, y = 5}
                , {x = 2, y = 6}
                ]
      , width = 5
      , time = Nothing
      , lastKey = Nothing
      }
    , Cmd.none
    )

---- UPDATE ----


type Msg
    = NoOp
      | Tick Time
      | Keypress Keyboard.KeyCode

update : Msg -> Model -> ( Model, Cmd Msg )
update   msg    model =
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

gridSize = 20                   --grid size in pixels

view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , p [] [ text (toString model)]
        , div [ class "game"
              , style [("background", "#eeffee")]
              ] [gameField model]
        ]

gameField : Model -> Html.Html msg
gameField   model    =
    svg
    [ width "520", height "520", viewBox "0 0 120 120" ]
    [ rect [ x "10", y "10", width "100", height "100", rx "5", ry "5"
           , fill "red" ] []
    , rect [ x "5", y "15", width "100", height "100", rx "5", ry "5"
          , fill "blue" ] []
    , path [ fill "none", fillRule "evenodd", stroke "#db7373", strokeWidth "5"
           , strokeLinecap "round", strokeLinejoin "round"
           , d (buildMcoords model)] []
    ]

buildMcoords : Model -> String
buildMcoords model =
    List.foldl (\v a -> a ++ (buildOneCoord v)) "M " model.snake

buildOneCoord v =
    let scale = 5
    in (toString (v.x*scale)) ++ "," ++ (toString (v.y*scale)) ++ " "


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
