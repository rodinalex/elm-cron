module Cron.Describe
    exposing
        ( describeSchedule
        , scheduleDescription
        , getNextEvent
        )

{-| This module contains the functions to describe the `CronSchedule`

@docs describeSchedule, scheduleDescription, getNextEvent

-}

import Cron.Types exposing (..)
import Time.DateTime exposing (DateTime)
import Time.Date exposing (..)
import Cron.Decoder exposing (getExplicitSchedule)
import Time


{-| A function that puts a `CronSchedule` into a more-or-less readable format.

        Maybe.map describeSchedule <| decodeCronTab "* * * * *" =
        Just {
        minuteDescription = "every minute"
      , hourDescription = "every hour"
      , dayDescription = "every day"
      , monthDescription = "every month"
      , dayOfWeekDescription = "every day of the week"
     }

        Maybe.map describeSchedule <| decodeCronTab "12-27/3 * * JAN MON-FRI" =
        Just {
        minuteDescription = "every 3 minute between 12 and 27"
      , hourDescription = "every hour"
      , dayDescription = "every day"
      , monthDescription = "at month 1"
      , dayOfWeekDescription = "every day of the week between 1 and 5" }

        Maybe.map describeSchedule <| decodeCronTab "12-27/3 * JAN MON-FRI *" =
        Nothing -- Invalid syntax

-}
describeSchedule : CronSchedule -> DescribedCronSchedule
describeSchedule (CronSchedule a b c d e) =
    { minuteDescription = String.join " and " <| List.map (describeCronField "minute") a
    , hourDescription = String.join " and " <| List.map (describeCronField "hour") b
    , dayDescription = String.join " and " <| List.map (describeCronField "day") c
    , monthDescription = String.join " and " <| List.map (describeCronField "month") d
    , dayOfWeekDescription = String.join " and " <| List.map (describeCronField "day of the week") e
    }


{-| A function that puts a `CronSchedule` into a more-or-less readable format.

        Maybe.map scheduleDescription <| decodeCronTab "* * * * *" =
        Just "every minute; every hour; every day; every month; every day of the week"

        Maybe.map scheduleDescription <| decodeCronTab "12-27/3 * * JAN MON-FRI" =
        Just "every 3 minute between 12 and 27; every hour; every day; at month 1; every day of the week between 1 and 5"

        Maybe.map scheduleDescription <| decodeCronTab "12-27/3 * JAN MON-FRI *" =
        Nothing -- Invalid syntax

-}
scheduleDescription : CronSchedule -> String
scheduleDescription cS =
    let
        dCS =
            describeSchedule cS
    in
        dCS.minuteDescription
            ++ "; "
            ++ dCS.hourDescription
            ++ "; "
            ++ dCS.dayDescription
            ++ "; "
            ++ dCS.monthDescription
            ++ "; "
            ++ dCS.dayOfWeekDescription


{-| A function to describe an individual `CronField`
-}
describeCronField : String -> CronField -> String
describeCronField str (CronField bF mStep) =
    case mStep of
        Nothing ->
            case bF of
                Star ->
                    "every " ++ str

                SpecificField n ->
                    "at " ++ str ++ " " ++ toString n

                RangeField m n ->
                    "every " ++ str ++ " between " ++ toString m ++ " and " ++ toString n

        Just step ->
            case bF of
                Star ->
                    "every " ++ toString step ++ " " ++ str

                SpecificField n ->
                    "every " ++ toString step ++ " " ++ str ++ " starting at " ++ toString n

                RangeField m n ->
                    "every " ++ toString step ++ " " ++ str ++ " between " ++ toString m ++ " and " ++ toString n


{-| This function takes a CronTab as the first input and the UNIX timestam in milliseconds as the second one.
The result is a `Maybe String` of the next-event time in the ISO8601 format.
-}
getNextEvent : String -> Time.Time -> Maybe String
getNextEvent cronTab timeStamp =
    let
        schedule =
            getExplicitSchedule cronTab

        currTime =
            Time.DateTime.fromTimestamp timeStamp

        nextTime =
            Time.DateTime.setSecond 0 <| Time.DateTime.addMinutes 1 currTime
    in
        case schedule of
            Nothing ->
                Nothing

            Just sch ->
                Just <| Time.DateTime.toISO8601 <| nextEvent sch nextTime


{-| This function actually computes when the next event is expected. If there is a restriction on both day-of-month and day-of-week,
two calculations are performed: one without the first restriction, the other one without the second one. Then, the results are compared and
the earlier one is taken.
-}
nextEvent : ExplicitSchedule -> DateTime -> DateTime
nextEvent sch nextTime =
    if (sch.days /= List.range 1 31 && sch.daysOfWeek /= List.range 0 6) then
        let
            sch1 =
                { sch | days = List.range 1 31 }

            sch2 =
                { sch | daysOfWeek = List.range 0 6 }

            res1 =
                nextEvent sch1 nextTime

            res2 =
                nextEvent sch2 nextTime
        in
            case Time.DateTime.compare res1 res2 of
                LT ->
                    res1

                _ ->
                    res2
    else
        setNextMinute sch <| setNextHour sch <| setNextDay sch <| setNextWeekDay sch <| setNextMonth sch nextTime


{-| The `set...` functions check whether a field in the tentative next-time belongs to the `ExplicitSchedule`.
If so, the time is passed to a lower-level data segment (month -> day -> hour -> minute).
Otherwise, the value of field is augmented by one and all lower-level fields are set to their lowest respective values
-}
setNextMinute : ExplicitSchedule -> DateTime -> DateTime
setNextMinute sch nxtTime =
    let
        minuteList =
            sch.minutes
    in
        case (List.member (Time.DateTime.minute nxtTime) minuteList) of
            True ->
                nxtTime

            False ->
                nextEvent sch (Time.DateTime.addMinutes 1 nxtTime)


setNextHour : ExplicitSchedule -> DateTime -> DateTime
setNextHour sch nxtTime =
    let
        hourList =
            sch.hours
    in
        case (List.member (Time.DateTime.hour nxtTime) hourList) of
            True ->
                nxtTime

            False ->
                nextEvent sch
                    (Time.DateTime.addHours 1 <|
                        Time.DateTime.setMinute 0 nxtTime
                    )


setNextDay : ExplicitSchedule -> DateTime -> DateTime
setNextDay sch nxtTime =
    let
        dayList =
            sch.days
    in
        case (List.member (Time.DateTime.day nxtTime) dayList) of
            True ->
                nxtTime

            False ->
                nextEvent sch
                    (Time.DateTime.addDays 1 <|
                        Time.DateTime.setMinute 0 <|
                            Time.DateTime.setHour 0 nxtTime
                    )


setNextWeekDay : ExplicitSchedule -> DateTime -> DateTime
setNextWeekDay sch nxtTime =
    let
        weekdayList =
            List.map dayOfWeekConversion sch.daysOfWeek
    in
        case (List.member (Time.DateTime.weekday nxtTime) weekdayList) of
            True ->
                nxtTime

            False ->
                nextEvent sch
                    (Time.DateTime.addDays 1 <|
                        Time.DateTime.setMinute 0 <|
                            Time.DateTime.setHour 0 nxtTime
                    )


setNextMonth : ExplicitSchedule -> DateTime -> DateTime
setNextMonth sch nxtTime =
    let
        monthList =
            sch.months
    in
        case (List.member (Time.DateTime.month nxtTime) monthList) of
            True ->
                nxtTime

            False ->
                nextEvent sch
                    (Time.DateTime.addMonths 1 <|
                        Time.DateTime.setMinute 0 <|
                            Time.DateTime.setHour 0 <|
                                Time.DateTime.setDay 1 nxtTime
                    )


{-| Function for converting the numerical value of weekdays to the format used in `Time.Date` module
-}
dayOfWeekConversion : Int -> Time.Date.Weekday
dayOfWeekConversion n =
    case n of
        0 ->
            Sun

        1 ->
            Mon

        2 ->
            Tue

        3 ->
            Wed

        4 ->
            Thu

        5 ->
            Fri

        6 ->
            Sat

        _ ->
            Sun
