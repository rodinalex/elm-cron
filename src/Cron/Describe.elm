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
-}
describeSchedule : CronSchedule -> DescribedCronSchedule
describeSchedule (CronSchedule a b c d e) =
    { minuteDescription = String.join " and " <| List.map (describeCronField "minute") a
    , hourDescription = String.join " and " <| List.map (describeCronField "hour") b
    , dayDescription = String.join " and " <| List.map (describeCronField "day") c
    , monthDescription = String.join " and " <| List.map (describeCronField "month") d
    , dayOfWeekDescription = String.join " and " <| List.map (describeCronField "day of the week") e
    }


{-| A function to turn the `CronSchedule` into a string
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
