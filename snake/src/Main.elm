module Main exposing (..)

---- IMPORTS ----

import Html exposing (Html, text, div, h1, img, p)
import Html.Attributes exposing (src)



---- MODEL ----

type alias Coordinate = { x : Int
                        , y : Int
                        }

type alias Model = { width : Int
                   , height : Int
                   , snake : List Coordinate
                   , heading : Heading
                   }

type alias Snake = List Coordinate

type Heading = Up
             | Right
             | Down
             | Left

init : ( Model, Cmd Msg )
init =
    ( { width = 5
      , height = 5
      , snake = [ {x = 2, y = 2} ]
      , heading = Up
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , p [] [ text (toString model)]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
