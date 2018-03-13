module Main exposing (..)

import Html exposing (Html, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Random exposing (..)
import Time exposing (Time, second)


---- MODEL ----


type alias Coordinate =
    { x : Int, y : Int }


type alias FoodItems =
    List Coordinate


type alias Model =
    { foodItems : FoodItems }


init : ( Model, Cmd Msg )
init =
    ( { foodItems =
            [ { x = 5, y = 5 }, { x = 7, y = 7 }, { x = 9, y = 9 } ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time
    | NewVal FoodItems


genera =
    list 3
        (Random.map2
            Coordinate
            (int 0 5)
            (int 0 5)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            ( model, Random.generate NewVal genera )

        NewVal i ->
            ( Model i, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , p [] [ text (toString model) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (1 * second) Tick
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
