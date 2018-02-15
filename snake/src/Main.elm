module Main exposing (..)

---- IMPORTS ----

import Html exposing (Html, text, div, h1, img, p)
import Html.Attributes exposing (src)
import Time exposing (Time, second)
import Keyboard exposing (downs)

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
      , snake = [ {x = 2, y = 2} ]
      , width = 5
      , time = Nothing
      , lastKey = Nothing
      }
    , Cmd.none
    )


-- zzz1 = { width = 5, height = 5, snake = [{ x = 2, y = 2 }], heading = Up }

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
            ( { model |
                    lastKey = Just (translateKeypress key),
                    heading = (changeHeading model (translateKeypress key))
              }
            , Cmd.none)

translateKeypress : Keyboard.KeyCode -> KeyControl
translateKeypress keycode =
    case keycode of
        32 -> KeyPause
        37 -> KeyLeft
        38 -> KeyUp
        39 -> KeyRight
        40 -> KeyDown
        _  -> KeyOther

changeHeading : Model -> KeyControl -> Heading
changeHeading   model    kc =
    case kc of
        KeyOther -> model.heading
        KeyPause -> model.heading
        KeyLeft  -> Left
        KeyUp    -> Up
        KeyRight -> Right
        KeyDown  -> Down

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , p [] [ text (toString model)]
        ]


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
