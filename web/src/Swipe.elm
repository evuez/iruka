module Swipe exposing (Direction(..), Event, State, direction, init, onSwipe)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json


type alias Coords =
    { clientX : Float, clientY : Float }


type Event
    = TouchStart Coords
    | TouchEnd Coords


type Direction
    = Right
    | Left
    | Up
    | Down


type alias State =
    { origin : Maybe Coords }


init : State
init =
    State Nothing


direction : Event -> State -> ( State, Maybe Direction )
direction e { origin } =
    case e of
        TouchStart coords ->
            ( { origin = Just coords }, Nothing )

        TouchEnd coords ->
            case origin of
                Nothing ->
                    ( init, Nothing )

                Just origin_ ->
                    ( init, getDirection (coords.clientX - origin_.clientX) (coords.clientY - origin_.clientY) )


getDirection : Float -> Float -> Maybe Direction
getDirection dx dy =
    if abs dx > abs dy then
        if dx > 0 then
            Just Right

        else if dx < 0 then
            Just Left

        else
            Nothing

    else if dy > 0 then
        Just Down

    else if dy < 0 then
        Just Up

    else
        Nothing


onSwipe : (Event -> msg) -> List (Attribute msg)
onSwipe msg =
    [ onTouchStart msg
    , onTouchEnd msg
    ]


onTouchStart : (Event -> msg) -> Attribute msg
onTouchStart msg =
    touchDecoder "targetTouches"
        |> Json.map TouchStart
        |> Json.map msg
        |> on "touchstart"


onTouchEnd : (Event -> msg) -> Attribute msg
onTouchEnd msg =
    touchDecoder "changedTouches"
        |> Json.map TouchEnd
        |> Json.map msg
        |> on "touchend"



-- DECODERS


touchDecoder : String -> Json.Decoder Coords
touchDecoder eventType =
    Json.at [ eventType, "0" ] coordsDecoder


coordsDecoder : Json.Decoder Coords
coordsDecoder =
    Json.map2 Coords
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
