module Cron.Decoder exposing (decodeCronTab)

{-| @docs decodeCronTab
-}

import Combine as C
import Cron.Parsers exposing (..)
import Cron.Types exposing (..)


{-| Function to decode the standard CronTab.
-}
decodeCronTab : String -> Maybe CronSchedule
decodeCronTab str =
    let
        splitStr =
            String.split " " (String.toUpper str)
    in
        case splitStr of
            [ a, b, c, d, e ] ->
                Maybe.map5 CronSchedule
                    (decodeMinuteCronField a)
                    (decodeHourCronField b)
                    (decodeDayCronField c)
                    (decodeMonthCronField d)
                    (decodeDayOfWeekCronField e)

            _ ->
                Nothing


{-| Decoder for the Minute CronTab entry.
-}
decodeMinuteCronField : String -> Maybe (List CronField)
decodeMinuteCronField str =
    case C.parse (cronFieldParser simpleBaseFieldParser) str of
        Ok ( _, stream, result ) ->
            case stream.input of
                "" ->
                    if List.member False <| List.map (checkValidity 0 59) result then
                        Nothing
                    else
                        Just result

                _ ->
                    Nothing

        Err _ ->
            Nothing


{-| Decoder for the Hour CronTab entry.
-}
decodeHourCronField : String -> Maybe (List CronField)
decodeHourCronField str =
    case C.parse (cronFieldParser simpleBaseFieldParser) str of
        Ok ( _, stream, result ) ->
            case stream.input of
                "" ->
                    if List.member False <| List.map (checkValidity 0 23) result then
                        Nothing
                    else
                        Just result

                _ ->
                    Nothing

        Err _ ->
            Nothing


{-| Decoder for the Day CronTab entry.
-}
decodeDayCronField : String -> Maybe (List CronField)
decodeDayCronField str =
    case C.parse (cronFieldParser simpleBaseFieldParser) str of
        Ok ( _, stream, result ) ->
            case stream.input of
                "" ->
                    if List.member False <| List.map (checkValidity 1 31) result then
                        Nothing
                    else
                        Just result

                _ ->
                    Nothing

        Err _ ->
            Nothing


{-| Decoder for the Month CronTab entry.
-}
decodeMonthCronField : String -> Maybe (List CronField)
decodeMonthCronField str =
    case C.parse (cronFieldParser monthBaseFieldParser) str of
        Ok ( _, stream, result ) ->
            case stream.input of
                "" ->
                    if List.member False <| List.map (checkValidity 1 12) result then
                        Nothing
                    else
                        Just result

                _ ->
                    Nothing

        Err _ ->
            Nothing


{-| Decoder for the Day-of-Week CronTab entry.
-}
decodeDayOfWeekCronField : String -> Maybe (List CronField)
decodeDayOfWeekCronField str =
    case C.parse (cronFieldParser dayOfWeekBaseFieldParser) str of
        Ok ( _, stream, result ) ->
            case stream.input of
                "" ->
                    if List.member False <| List.map (checkValidity 0 6) result then
                        Nothing
                    else
                        Just result

                _ ->
                    Nothing

        Err _ ->
            Nothing


{-| Function to check whether the numerical values of a given field are valid.
-}
checkValidity : Int -> Int -> CronField -> Bool
checkValidity mn mx (CronField bF mStep) =
    let
        validStep stp =
            case stp of
                Nothing ->
                    True

                Just l ->
                    l > 0
    in
        case bF of
            Star ->
                validStep mStep

            SpecificField n ->
                (n <= mx) && (n >= mn) && validStep mStep

            RangeField m n ->
                (n <= mx) && (n >= mn) && (m < n) && validStep mStep
