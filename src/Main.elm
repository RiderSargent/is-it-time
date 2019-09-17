module Main exposing (Model, Msg(..), formatMonth, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Task
import Time exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "container" ]
        [ div
            [ id "is-it-time" ]
            [ viewIsItTime model ]
        , div
            [ class "debug" ]
            [ viewTime model ]
        , pre
            [ class "debug" ]
            [ model
                |> Debug.toString
                |> text
            ]
        ]


viewIsItTime : Model -> Html msg
viewIsItTime model =
    let
        weekday =
            Time.toWeekday model.zone model.time

        hour =
            Time.toHour model.zone model.time

        isFriday =
            if weekday == Fri then
                True

            else
                False

        isNoon =
            if hour == 12 then
                True

            else
                False
    in
    if isFriday && isNoon then
        text "It's time!"

    else
        text "No"


viewTime : Model -> Html msg
viewTime model =
    let
        year =
            String.fromInt (Time.toYear model.zone model.time)

        month =
            formatMonth (Time.toMonth model.zone model.time)

        day =
            toZeroPaddedString (Time.toDay model.zone model.time)

        weekday =
            formatWeekday (Time.toWeekday model.zone model.time)

        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            toZeroPaddedString (Time.toMinute model.zone model.time)

        second =
            toZeroPaddedString (Time.toSecond model.zone model.time)
    in
    text (year ++ "-" ++ month ++ "-" ++ day ++ " " ++ weekday ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second)


toZeroPaddedString : Int -> String
toZeroPaddedString digits =
    if digits < 10 then
        "0" ++ String.fromInt digits

    else
        String.fromInt digits


formatMonth : Month -> String
formatMonth month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


formatWeekday : Weekday -> String
formatWeekday weekday =
    case weekday of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"
