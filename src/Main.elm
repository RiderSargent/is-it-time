module Main exposing (Model, Msg(..), formatMonth, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
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
        []
        [ div
            []
            [ viewShortDate model ]
        , div
            []
            [ viewTime model ]
        , div
            []
            [ viewNandosTime model ]
        , pre
            []
            [ model
                |> Debug.toString
                |> text
            ]
        ]


viewNandosTime : Model -> Html msg
viewNandosTime model =
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
        text "Yes!"

    else
        text "No"


viewShortDate : Model -> Html msg
viewShortDate model =
    let
        tz =
            model.zone

        posix =
            model.time

        year =
            String.fromInt (Time.toYear tz posix)

        month =
            formatMonth (Time.toMonth tz posix)

        day =
            zeroPad (Time.toDay tz posix)

        weekday =
            formatWeekday (Time.toWeekday tz posix)
    in
    text (year ++ "-" ++ month ++ "-" ++ day ++ " " ++ weekday)


viewTime : Model -> Html msg
viewTime model =
    let
        tz =
            model.zone

        posix =
            model.time

        hour =
            String.fromInt (Time.toHour tz posix)

        minute =
            zeroPad (Time.toMinute tz posix)

        second =
            zeroPad (Time.toSecond tz posix)
    in
    text (hour ++ ":" ++ minute ++ ":" ++ second)


zeroPad : Int -> String
zeroPad digits =
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
