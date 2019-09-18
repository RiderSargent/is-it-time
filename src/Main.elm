module Main exposing (Model, Msg(..), formatMonth, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Task
import Time exposing (..)



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
            [ viewHumanTime model.zone model.time
            , text "\n"
            , viewHumanTime model.zone friNoon
            , text "\n"
            , friNoon
                |> intervalInMs model.time
                |> formatInterval
                |> text
            ]
        ]


intervalInMs : Posix -> Posix -> Int
intervalInMs start finish =
    posixToMillis finish - posixToMillis start


formatInterval : Int -> String
formatInterval interval =
    let
        numDays =
            String.fromInt <| interval // dayInMs

        numHours =
            String.fromInt <| remainderBy dayInMs interval // hourInMs

        numMinutes =
            String.fromInt <| remainderBy hourInMs interval // minuteInMs

        numSeconds =
            String.fromInt <| remainderBy minuteInMs interval // 1000
    in
    numDays ++ " days, " ++ numHours ++ " hours, " ++ numMinutes ++ " minutes, " ++ numSeconds ++ " seconds"


dayInMs : Int
dayInMs =
    86400000


hourInMs : Int
hourInMs =
    3600000


minuteInMs : Int
minuteInMs =
    60000


friNoon : Posix
friNoon =
    -- 2019-09-13 Fri 12:00:00 UTC
    -- millisToPosix 1568372400000
    millisToPosix (1568372400000 + oneWeekInMs)


oneWeekInMs : Int
oneWeekInMs =
    604800000


viewIsItTime : Model -> Html msg
viewIsItTime model =
    let
        weekday =
            Time.toWeekday model.zone model.time

        hour =
            Time.toHour model.zone model.time
    in
    if weekday == Fri && hour == 12 then
        text "It's time!"

    else
        text "No"


viewHumanTime : Zone -> Posix -> Html msg
viewHumanTime tz time =
    let
        year =
            String.fromInt (Time.toYear tz time)

        month =
            formatMonth (Time.toMonth tz time)

        day =
            toZeroPaddedString (Time.toDay tz time)

        weekday =
            formatWeekday (Time.toWeekday tz time)

        hour =
            String.fromInt (Time.toHour tz time)

        minute =
            toZeroPaddedString (Time.toMinute tz time)

        second =
            toZeroPaddedString (Time.toSecond tz time)
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



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
