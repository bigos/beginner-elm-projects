module Main exposing (..)

---- IMPORTS ----

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)



---- MODEL ----

type alias Coordinate =
    { x : Int
    , y : Int
    }

type alias Snake =
    { segments : List Coordinate }

type alias GameField =
    { width : Int
    , height : Int
    , snake : Snake
    }

type alias Model =
    { gameField : GameField
    , snake : Snake
    }


init : ( Model, Cmd Msg )
init =
    ( { { 5, 5 }
      , [ {2, 2}
        , {2, 3}
        ]
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
