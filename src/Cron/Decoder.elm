module Cron.Decoder
    exposing
        ( decodeCronTab
        , getExplicitSchedule
        )

{-| This module contains the function to turn a Crontab `String` into `CronSchedule`
@docs decodeCronTab, getExplicitSchedule
-}

import Combine as C
import Cron.Parsers exposing (..)
import Cron.Types exposing (..)


{-| Function to decode the standard CronTab.

        decodeCronTab "* * * * *" =
        Just (
        CronSchedule
          ([CronField Star Nothing])
          ([CronField Star Nothing])
          ([CronField Star Nothing])
          ([CronField Star Nothing])
          ([CronField Star Nothing])
        )

        decodeCronTab "12-27/3 * * JAN MON-FRI" =
        Just (
        CronSchedule
          ([CronField (RangeField 12 27) (Just 3)])
          ([CronField Star Nothing])
          ([CronField Star Nothing])
          ([CronField (SpecificField 1) Nothing])
          ([CronField (RangeField 1 5) Nothing])
        )

        decodeCronTab "12-27/3 * JAN MON-FRI *" =
        Nothing -- Invalid syntax

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
    if str == "" then
        Nothing
    else
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
    if str == "" then
        Nothing
    else
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
    if str == "" then
        Nothing
    else
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
    if str == "" then
        Nothing
    else
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
    if str == "" then
        Nothing
    else
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
                    l > 0 && l <= mx
    in
        case bF of
            Star ->
                validStep mStep

            SpecificField n ->
                (n <= mx) && (n >= mn) && (mStep == Nothing)

            RangeField m n ->
                (n <= mx) && (n >= mn) && (m < n) && validStep mStep


{-| This function obtains the `ExplicitSchedule` from a CronTab
-}
getExplicitSchedule : String -> Maybe ExplicitSchedule
getExplicitSchedule cronTab =
    let
        mCronSchedule =
            decodeCronTab cronTab
    in
        case mCronSchedule of
            Nothing ->
                Nothing

            Just sch ->
                Just
                    { minutes = allowedMinutes sch
                    , hours = allowedHours sch
                    , days = allowedDays sch
                    , months = allowedMonths sch
                    , daysOfWeek = allowedDaysOfWeek sch
                    }


{-| Explicit values for minutes
-}
allowedMinutes : CronSchedule -> List Int
allowedMinutes (CronSchedule x _ _ _ _) =
    allowedValues x 0 59


{-| Explicit values for hours
-}
allowedHours : CronSchedule -> List Int
allowedHours (CronSchedule _ x _ _ _) =
    allowedValues x 0 23


{-| Explicit values for days
-}
allowedDays : CronSchedule -> List Int
allowedDays (CronSchedule _ _ x _ _) =
    allowedValues x 1 31


{-| Explicit values for months
-}
allowedMonths : CronSchedule -> List Int
allowedMonths (CronSchedule _ _ _ x _) =
    allowedValues x 1 12


{-| Explicit values for days-of-week
-}
allowedDaysOfWeek : CronSchedule -> List Int
allowedDaysOfWeek (CronSchedule _ _ _ _ x) =
    allowedValues x 0 6


{-| Function for obtaining the allowed values give the list of `CronField`s, as well as the minimum and maximum values
-}
allowedValues : List CronField -> Int -> Int -> List Int
allowedValues lst mn mx =
    List.concat (List.map (allowedValuesAux mn mx) lst)


allowedValuesAux : Int -> Int -> CronField -> List Int
allowedValuesAux mn mx (CronField numF mStep) =
    case numF of
        Star ->
            case mStep of
                Nothing ->
                    List.range mn mx

                Just stp ->
                    stepRange mn mx stp

        SpecificField n ->
            [ n ]

        RangeField m n ->
            case mStep of
                Nothing ->
                    List.range m n

                Just stp ->
                    stepRange mn mx stp


stepRange : Int -> Int -> Int -> List Int
stepRange mn mx stp =
    List.filter (\x -> x <= mx) (List.map (\x -> mn + x * stp) (List.range 0 mx))
