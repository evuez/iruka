module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Encode as E
import Markdown
import Ports
import Random
import Swipe exposing (onSwipe)
import Url exposing (Url)
import Url.Parser as P exposing ((<?>))
import Url.Parser.Query as Q


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    update (UrlChanged url)
        { flags = flags
        , swipe = Swipe.init
        , cards = []
        , card = Nothing
        , flipped = False
        , emoji = ""
        }



-- MODEL


type alias Flags =
    { basePath : String, scores : List ( String, Float ) }


type alias Model =
    { flags : Flags, swipe : Swipe.State, cards : List Card, card : Maybe Card, flipped : Bool, emoji : String }


type alias Card =
    { term : String
    , definition : String
    , tags : List String
    , image : Maybe String
    , color : Maybe String
    , link : Maybe String
    , score : Float
    }


type Reaction
    = Yay
    | Nay



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | GotCards (Result Http.Error (List Card))
    | GotCard (Maybe Card)
    | Flip
    | Swipe Swipe.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ flags } as model) =
    case msg of
        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    ( model, Cmd.none )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case P.parse route (fixUrl flags.basePath url) of
                Just (Set (Just setUrl)) ->
                    ( model, getCards setUrl )

                _ ->
                    ( model, Cmd.none )

        GotCards (Ok cards) ->
            let
                cards_ =
                    mergeWeights cards flags.scores
            in
            ( { model | cards = cards_ }, Cmd.batch [ pickCard cards_, saveScores cards_ ] )

        GotCards (Err _) ->
            ( model, Cmd.none )

        GotCard (Just card) ->
            ( { model | card = Just card, flipped = False }, Cmd.none )

        GotCard Nothing ->
            ( { model | card = Nothing, flipped = False }, Cmd.none )

        Flip ->
            ( { model | flipped = True }, Cmd.none )

        Swipe e ->
            let
                ( swipe_, reaction ) =
                    case Swipe.direction e model.swipe of
                        ( s, Just Swipe.Up ) ->
                            ( s, Just Yay )

                        ( s, Just Swipe.Down ) ->
                            ( s, Just Nay )

                        ( s, _ ) ->
                            ( s, Nothing )

                cards_ =
                    case reaction of
                        Just reaction_ ->
                            updateScore model.cards model.card (getScore reaction_)

                        Nothing ->
                            model.cards
            in
            case reaction of
                Just reaction_ ->
                    ( { model | swipe = swipe_, emoji = getEmoji reaction_, cards = cards_ }, Cmd.batch [ pickCard cards_, saveScores cards_ ] )

                Nothing ->
                    ( { model | swipe = swipe_ }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "iruka", body = [ cardsView model ] }


cardsView : Model -> Html Msg
cardsView model =
    case model.card of
        Just c ->
            main_ [ style "border-color" (Maybe.withDefault "" c.color) ]
                [ section
                    [ class "front"
                    , classList [ ( "hidden", model.flipped ) ]
                    , attribute "data-emoji" model.emoji
                    ]
                    (frontView c)
                , section [ class "back", classList [ ( "hidden", not model.flipped ) ] ] (backView c)
                ]

        Nothing ->
            main_ [] [ section [ class "error" ] [ h1 [] [ text "Error" ], p [] [ text "No card found." ] ] ]


frontView : Card -> List (Html Msg)
frontView { term, tags, score } =
    [ div [ onClick Flip, class "wrapper" ] [ h1 [ class "term" ] [ text term ] ]
    , nav [ class "tags" ] (List.map (\x -> span [ class "label" ] [ text x ]) tags)
    ]


backView : Card -> List (Html Msg)
backView { definition, image, link } =
    [ div (class "wrapper" :: onSwipe Swipe) [ article [ class "definition" ] (showDefinition definition) ]
    , nav []
        (List.filterMap identity
            [ Maybe.map (showExtra "image") image
            , Maybe.map (showExtra "link") link
            ]
        )
    ]


showDefinition : String -> List (Html Msg)
showDefinition definition =
    case definition of
        "" ->
            [ text "Definition missing." ]

        _ ->
            Markdown.toHtml Nothing definition


showExtra : String -> String -> Html Msg
showExtra name url =
    a [ class "label", href url ] [ text name ]



-- ROUTER


type Route
    = Set (Maybe String)


route : P.Parser (Route -> a) a
route =
    P.oneOf [ P.map Set (P.top <?> Q.string "set") ]


fixUrl : String -> Url -> Url
fixUrl basePath ({ path } as r) =
    { r | path = String.replace basePath "/" path }



-- ACTIONS


getCards : String -> Cmd Msg
getCards url =
    Http.get { url = url, expect = Http.expectJson GotCards cardsDecoder }



-- CODECS


scoreEncoder : Card -> E.Value
scoreEncoder c =
    E.list identity [ E.float c.score, E.string (cardKey c) ]


scoresEncoder : List Card -> E.Value
scoresEncoder cards =
    E.list scoreEncoder cards


cardDecoder : D.Decoder Card
cardDecoder =
    D.map7 Card
        (D.field "term" D.string)
        (D.field "definition" D.string)
        (D.field "tags" (D.list D.string))
        (D.field "image" (D.nullable D.string))
        (D.field "color" (D.nullable D.string))
        (D.field "link" (D.nullable D.string))
        (D.succeed 9)


cardsDecoder : D.Decoder (List Card)
cardsDecoder =
    D.list cardDecoder



-- HELPERS


pickCard : List Card -> Cmd Msg
pickCard cs =
    Random.generate GotCard (randGen cs)


randGen : List Card -> Random.Generator (Maybe Card)
randGen xs =
    Random.weighted (maybeCardToWeight <| List.head xs) (maybeCardsToWeight <| List.tail xs)


maybeCardToWeight : Maybe Card -> ( Float, Maybe Card )
maybeCardToWeight c =
    case c of
        Just card ->
            cardToWeight card

        Nothing ->
            ( 9, Nothing )


cardToWeight : Card -> ( Float, Maybe Card )
cardToWeight c =
    ( 10 - c.score, Just c )


maybeCardsToWeight : Maybe (List Card) -> List ( Float, Maybe Card )
maybeCardsToWeight cs =
    case cs of
        Just cards ->
            List.map cardToWeight cards

        Nothing ->
            []


cardKey : Card -> String
cardKey { term, tags } =
    String.concat [ term, "|", String.join "," <| List.sort tags ]


mergeWeights : List Card -> List ( String, Float ) -> List Card
mergeWeights cards scores =
    List.map (\c -> { c | score = Maybe.withDefault 9 (findScore scores c) }) cards


findScore : List ( String, Float ) -> Card -> Maybe Float
findScore scores card =
    let
        key =
            cardKey card
    in
    List.head <| List.filterMap (scoreOrNothing key) scores


scoreOrNothing : String -> ( String, Float ) -> Maybe Float
scoreOrNothing key ( k, score ) =
    if key == k then
        Just score

    else
        Nothing


getScore : Reaction -> Float
getScore r =
    case r of
        Yay ->
            1

        Nay ->
            -1


getEmoji : Reaction -> String
getEmoji r =
    case r of
      Yay ->
        "🧙"
      Nay ->
        "🧟"


updateScore : List Card -> Maybe Card -> Float -> List Card
updateScore cards card val =
    case card of
        Just card_ ->
            List.map
                (\c ->
                    if c == card_ && c.score + val > 0 && c.score + val < 10 then
                        { c | score = c.score + val }

                    else
                        c
                )
                cards

        Nothing ->
            cards


saveScores : List Card -> Cmd Msg
saveScores cards =
    Ports.saveScores (scoresEncoder cards)
