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


type Group
    = A
    | B


type alias Card =
    { id : String
    , group : Group
    , flipped : Bool
    }


type alias Deck =
    List Card


type Model
    = Playing Deck


type Msg
    = NoOp


cards : List String
cards =
    [ "amy"
    , "bender"
    , "fry"
    , "leela"
    , "hubert"
    , "hermes"
    , "zoidberg"
    , "mom"
    ]


initCard : Group -> String -> Card
initCard group name =
    { id = name
    , group = group
    , flipped = False
    }


deck : Deck
deck =
    let
        groupA =
            List.map (initCard A) cards

        groupB =
            List.map (initCard B) cards
    in
    List.concat [ groupA, groupB ]


cardClass : Card -> String
cardClass card =
    "card-" ++ card.id


createCard : Card -> Html Msg
createCard card =
    div [ class "container" ]
        -- try changing ("flipped", False) into ("flipped", True)
        [ div [ classList [ ( "card", True ), ( "flipped", True ) ] ]
            [ div [ class "card-back" ] []
            , div [ class ("front " ++ cardClass card) ] []
            ]
        ]


init : ( Model, Cmd Msg )
init =
    ( Playing deck, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Playing deck ->
            div [ class "wrapper" ] (List.map createCard deck)
