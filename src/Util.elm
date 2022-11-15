module Util exposing (..)

import Time exposing (Month(..), Posix, Weekday(..), Zone)


timeZone : Zone
timeZone =
    Time.utc


monthToString : Month -> String
monthToString month =
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


weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


dateStringFrom : Posix -> Posix -> String
dateStringFrom now posix =
    let
        diff =
            Time.posixToMillis now - Time.posixToMillis posix |> Basics.toFloat

        minutes =
            diff / 1000 / 60

        hours =
            minutes / 60

        days =
            hours / 24

        months =
            days / 30

        years =
            days / 365
    in
    if years > 1 || months > 1 || days > 1 then
        "in "
            ++ String.padLeft 2 '0' (String.fromInt (Time.toDay timeZone posix))
            ++ "/"
            ++ monthToString (Time.toMonth timeZone posix)
            ++ "/"
            ++ String.fromInt (Time.toYear timeZone posix)
            ++ " ("
            ++ weekdayToString (Time.toWeekday timeZone posix)
            ++ ") at "
            ++ String.padLeft 2 '0' (String.fromInt (Time.toHour timeZone posix))
            ++ ":"
            ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute timeZone posix))

    else if hours > 1 then
        let
            hours_ =
                floor hours
        in
        if hours_ == 1 then
            "1 hour ago"

        else
            String.fromInt (ceiling hours) ++ " hours ago"

    else if minutes > 1 then
        let
            minutes_ =
                floor minutes
        in
        if minutes_ == 1 then
            "1 minute ago"

        else
            String.fromInt (ceiling minutes) ++ " minutes ago"

    else
        "just now"
