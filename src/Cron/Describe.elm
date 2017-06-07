module Cron.Describe exposing (describeSchedule)

{-| @docs describeSchedule
-}

import Cron.Types exposing (..)


{-| A function that puts a `Maybe` parsed `CronSchedule` into a more-or-less readable format.
-}
describeSchedule : Maybe CronSchedule -> Maybe DescribedCronSchedule
describeSchedule sch =
    case sch of
        Nothing ->
            Nothing

        Just (CronSchedule a b c d e) ->
            Just
                { minuteDescription = String.join " and " <| List.map (describeCronField "minute") a
                , hourDescription = String.join " and " <| List.map (describeCronField "hour") b
                , dayDescription = String.join " and " <| List.map (describeCronField "day") c
                , monthDescription = String.join " and " <| List.map (describeCronField "month") d
                , dayOfWeekDescription = String.join " and " <| List.map (describeCronField "day of the week") e
                }


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
