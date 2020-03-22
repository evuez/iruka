module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Markdown
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
        , cards = Array.fromList []
        , card = Nothing
        , flipped = False
        , emoji = ""
        }



-- MODEL


type alias Flags =
    { basePath : String }


type alias Model =
    { flags : Flags, swipe : Swipe.State, cards : Array Card, card : Maybe Card, flipped : Bool, emoji : String }


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

        Swipe e ->
            case Swipe.direction e model.swipe of
                ( swipe_, Just Swipe.Up ) ->
                    ( { model | swipe = swipe_, emoji = "🧙" }, Cmd.batch [ pickCard model.cards ] )

                ( swipe_, Just Swipe.Down ) ->
                    ( { model | swipe = swipe_, emoji = "🧟" }, Cmd.batch [ pickCard model.cards ] )

                ( swipe_, _ ) ->
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
frontView { term, tags } =
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
