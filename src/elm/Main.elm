module Main exposing (..)

import Html exposing (Html, div, h1, p, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Random


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
    | Guessing Deck Card
    | MatchCard Deck Card Card
    | GameOver Deck


type Msg
    = NoOp
    | Reset
    | Shuffle (List Int)
    | Flip Card



-- MODEL


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


init : ( Model, Cmd Msg )
init =
    let
        model =
            Playing deck

        cmd =
            randomList Shuffle (List.length deck)
    in
    ( model, cmd )



-- UPDATE


randomList : (List Int -> Msg) -> Int -> Cmd Msg
randomList msg len =
    Random.int 0 100
        |> Random.list len
        |> Random.generate msg


shuffleDeck : Deck -> List comparable -> Deck
shuffleDeck deck xs =
    List.map2 (,) deck xs
        |> List.sortBy Tuple.second
        |> List.unzip
        |> Tuple.first


flip : Bool -> Card -> Card -> Card
flip isFlipped a b =
    if (a.id == b.id) && (a.group == b.group) then
        { b | flipped = isFlipped }
    else
        b


checkIfCorrect : Card -> Model -> ( Model, Cmd Msg )
checkIfCorrect card model =
    case model of
        Playing deck ->
            let
                newDeck =
                    List.map (flip True card) deck
            in
            Guessing newDeck card ! []

        Guessing deck guess ->
            let
                newDeck =
                    List.map (flip True card) deck

                -- when all cards are flipped, the game is over
                isOver =
                    List.all .flipped newDeck

                newModel =
                    if isOver then
                        GameOver newDeck
                    else
                        MatchCard newDeck guess card
            in
            newModel ! []

        MatchCard deck guess1 guess2 ->
            if guess1.id == guess2.id then
                {-
                   user has guessed correctly!
                   keep both cards flipped and then run update
                   again to flip the new card that has been just clicked
                -}
                update (Flip card) (Playing deck)
            else
                -- flip the two cards face down because they don't match
                let
                    flipGuess =
                        flip False guess1 >> flip False guess2

                    newDeck =
                        List.map flipGuess deck
                in
                Playing newDeck ! []

        GameOver deck ->
            GameOver deck ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Shuffle xs ->
            let
                newDeck =
                    shuffleDeck deck xs
            in
            Playing newDeck ! []

        Flip card ->
            if card.flipped then
                -- if a user clicks on an image that's flipped already
                -- then don't do anything
                model ! []
            else
                checkIfCorrect card model

        Reset ->
            init


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


cardClass : Card -> String
cardClass card =
    "card-" ++ card.id


createCard : Card -> Html Msg
createCard card =
    div [ class "container" ]
        -- try changing ("flipped", False) into ("flipped", True)
        [ div [ classList [ ( "card", True ), ( "flipped", card.flipped ) ], onClick (Flip card) ]
            [ div [ class "card-back" ] []
            , div [ class ("front " ++ cardClass card) ] []
            ]
        ]


wrapper : Deck -> Html Msg -> Html Msg
wrapper deck overlay =
    div [ class "wrapper" ]
        [ div [] (List.map createCard deck)
        , overlay
        ]


game : Deck -> Html Msg
game deck =
    wrapper deck (text "")


playAgainOverlay : Html Msg
playAgainOverlay =
    div [ class "congrats" ]
        [ p [] [ text "Yay! You win!" ]
        , text "Do you want to "
        , span [ onClick Reset ] [ text "play again?" ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Playing deck ->
            game deck

        Guessing deck _ ->
            game deck

        MatchCard deck _ _ ->
            game deck

        GameOver deck ->
            wrapper deck playAgainOverlay
