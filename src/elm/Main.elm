module Main exposing (..)

import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, classList)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    {}


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ createCard
        , createCard
        , createCard
        , createCard
        , createCard
        ]


createCard : Html Msg
createCard =
    div [ class "container" ]
        [ div [ classList [ ( "card", True ), ( "flipped", True ) ] ]
            [ div [ class "card-back" ] []
            , div [ class "front card-bender" ] []
            ]
        ]
