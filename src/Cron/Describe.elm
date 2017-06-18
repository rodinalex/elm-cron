module Cron.Describe
    exposing
        ( describeSchedule
        , scheduleDescription
        )

{-| This module contains the functions to describe the `CronSchedule`

@docs describeSchedule, scheduleDescription

-}

import Cron.Types exposing (..)


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
