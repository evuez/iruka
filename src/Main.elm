module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Debug
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Markdown
import Random
import Url exposing (Url)
import Url.Parser as P exposing ((<?>))
import Url.Parser.Query as Q



-- main : Program (Maybe OwnerRecord) Model Msg


main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    update (UrlChanged url) { cards = Array.fromList [], card = Nothing, flipped = False }



-- MODEL


type alias Model =
    { cards : Array Card, card : Maybe Card, flipped : Bool }


type alias Card =
    { term : String
    , definition : String
    , tags : List String
    , image : Maybe String
    , color : Maybe String
    , link : Maybe String
    }



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | GotCards (Result Http.Error (Array Card))
    | GotCard (Maybe Card)
    | Flip
    | Next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    ( model, Cmd.none )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case P.parse route url of
                Just (Set (Just setUrl)) ->
                    ( model, Cmd.batch [ getCards setUrl ] )

                _ ->
                    ( model, Cmd.none )

        GotCards (Ok cards) ->
            ( { model | cards = cards }, Cmd.batch [ pickCard cards ] )

        GotCards (Err _) ->
            ( model, Cmd.none )

        GotCard (Just card) ->
            ( { model | card = Just card, flipped = False }, Cmd.none )

        GotCard Nothing ->
            ( { model | card = Nothing, flipped = False }, Cmd.none )

        Flip ->
            ( { model | flipped = True }, Cmd.none )

        Next ->
            ( model, Cmd.batch [ pickCard model.cards ] )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "okil", body = [ cardsView model ] }


cardsView : Model -> Html Msg
cardsView model =
    case model.card of
        Just c ->
            main_ []
                [ div [ class "front", classList [ ( "hidden", model.flipped ) ] ]
                    [ h1 [ onClick Flip, class "term" ] [ text c.term ]
                    , nav [ class "tags" ] (List.map (\x -> span [ class "label" ] [ text x ]) c.tags)
                    ]
                , div [ class "back", classList [ ( "hidden", not model.flipped ) ] ] (backView c)
                ]

        Nothing ->
            main_ [] [ div [ class "error" ] [ h1 [] [ text "Error" ], p [] [ text "No card found." ] ] ]


backView : Card -> List (Html Msg)
backView { definition, image, link } =
    [ div [ onClick Next, class "definition" ] (showDefinition definition)
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



-- ACTIONS


getCards : String -> Cmd Msg
getCards url =
    Http.get { url = url, expect = Http.expectJson GotCards cardsDecoder }



-- DECODERS


cardDecoder : D.Decoder Card
cardDecoder =
    D.map6 Card
        (D.field "term" D.string)
        (D.field "definition" D.string)
        (D.field "tags" (D.list D.string))
        (D.field "image" (D.nullable D.string))
        (D.field "color" (D.nullable D.string))
        (D.field "link" (D.nullable D.string))


cardsDecoder : D.Decoder (Array Card)
cardsDecoder =
    D.array cardDecoder



-- HELPERS


pickCard : Array Card -> Cmd Msg
pickCard cs =
    Random.generate GotCard (randomElem cs)


randomElem : Array a -> Random.Generator (Maybe a)
randomElem xs =
    Random.map (\i -> Array.get i xs) (Random.int 0 (Array.length xs - 1))
