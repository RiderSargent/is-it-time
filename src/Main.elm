module Main exposing (Model, Msg(..), formatMonth, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Task
import Time
    exposing
        ( Month(..)
        , Weekday(..)
        , every
        , millisToPosix
        , posixToMillis
        , toDay
        , toHour
        , toMinute
        , toMonth
        , toSecond
        , toWeekday
        , toYear
        )



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "container" ]
        [ div
            [ id "is-it-time" ]
            [ viewIsItTime model ]
        , pre
            [ class "debug" ]
            [ viewCountdown model
            , text "\n"
            , model
                |> nextFridayNoon
                |> formatHumanTime model.zone
                |> text
            ]
        ]


intervalInMs : Time.Posix -> Time.Posix -> Int
intervalInMs start finish =
    posixToMillis finish - posixToMillis start


weekInMs : Int
weekInMs =
    7 * dayInMs


dayInMs : Int
dayInMs =
    24 * hourInMs


hourInMs : Int
hourInMs =
    60 * minuteInMs


minuteInMs : Int
minuteInMs =
    60 * 1000


viewCountdown : Model -> Html msg
viewCountdown model =
    nextFridayNoon model
        |> intervalInMs model.time
        |> formatInterval
        |> text


nextFridayNoon : Model -> Time.Posix
nextFridayNoon model =
    -- 2019-09-13 Fri 12:00:00 UTC is 1568372400000
    millisToPosix (1568372400000 + weekInMs)


viewIsItTime : Model -> Html msg
viewIsItTime model =
    let
        weekday =
            toWeekday model.zone model.time

        hour =
            toHour model.zone model.time
    in
    if weekday == Fri && hour == 12 then
        text "It's time!"

    else
        text "No"


formatHumanTime : Time.Zone -> Time.Posix -> String
formatHumanTime tz time =
    let
        year =
            String.fromInt (toYear tz time)

        month =
            formatMonth (toMonth tz time)

        day =
            formatZeroPadded (toDay tz time)

        weekday =
            formatWeekday (toWeekday tz time)

        hour =
            formatZeroPadded (toHour tz time)

        minute =
            formatZeroPadded (toMinute tz time)

        second =
            formatZeroPadded (toSecond tz time)

        strings =
            [ year, "-", month, "-", day, " ", weekday, " ", hour, ":", minute, ":", second ]
    in
    List.foldr (++) "" strings


formatInterval : Int -> String
formatInterval interval =
    let
        days =
            String.fromInt <| interval // dayInMs

        hours =
            String.fromInt <| remainderBy dayInMs interval // hourInMs

        minutes =
            String.fromInt <| remainderBy hourInMs interval // minuteInMs

        seconds =
            String.fromInt <| remainderBy minuteInMs interval // 1000

        strings =
            [ days, " days, ", hours, " hours, ", minutes, " minutes, ", seconds, " seconds" ]
    in
    List.foldr (++) "" strings


formatZeroPadded : Int -> String
formatZeroPadded digits =
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
    every 1000 Tick



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
