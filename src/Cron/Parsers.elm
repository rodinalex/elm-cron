module Cron.Parsers
    exposing
        ( cronFieldParser
        , dayOfWeekBaseFieldParser
        , monthBaseFieldParser
        , simpleBaseFieldParser
        )

import Combine as C exposing ((*>), (<$), (<$>), (<*), (<*>), (<|>), end)
import Combine.Num as CN
import Cron.Types exposing (..)


{-| A parser for converting the alternative cron notation for months into integers: `JAN` -> `1`, `FEB` -> `2`, etc.
-}
monthNameParser : C.Parser s Int
monthNameParser =
    (1 <$ C.string "JAN")
        <|> (2 <$ C.string "FEB")
        <|> (3 <$ C.string "MAR")
        <|> (4 <$ C.string "APR")
        <|> (5 <$ C.string "MAY")
        <|> (6 <$ C.string "JUN")
        <|> (7 <$ C.string "JUL")
        <|> (8 <$ C.string "AUG")
        <|> (9 <$ C.string "SEP")
        <|> (10 <$ C.string "OCT")
        <|> (11 <$ C.string "NOV")
        <|> (12 <$ C.string "DEC")


{-| A parser for converting the alternative cron notation for days-of-week into integers: `SUN` -> `0`, `MON` -> `1`, etc.
-}
dayNameParser : C.Parser s Int
dayNameParser =
    (0 <$ C.string "SUN")
        <|> (1 <$ C.string "MON")
        <|> (2 <$ C.string "TUE")
        <|> (3 <$ C.string "WED")
        <|> (4 <$ C.string "THU")
        <|> (5 <$ C.string "FRI")
        <|> (6 <$ C.string "SAT")


{-| Simple parser for the BaseField. This is used for minutes, hours, and days-of-month.

        import Combine exposing (parse)
        parse simpleBaseFieldParser "*"
        -- Ok Star

        parse simpleBaseFieldParser "5"
        -- Ok SpecificField 5

        parse simpleBaseFieldParser "5-10"
        -- Ok RangeField 5 10

        parse simpleBaseFieldParser "5-a"
        -- Err

-}
simpleBaseFieldParser : C.Parser s BaseField
simpleBaseFieldParser =
    (Star <$ C.string "*")
        <|> C.map RangeField (CN.int <* C.string "-")
        <*> CN.int
        <|> C.map SpecificField CN.int


{-| Parser for the BaseField for the month entry. Same as `simpleBaseFieldParser`, but also handles the alternative notation.

        import Combine exposing (parse)
        parse monthBaseFieldParser "*"
        -- Ok Star

        parse monthBaseFieldParser "5"
        -- Ok SpecificField 5

        parse monthBaseFieldParser "JUN-OCT"
        -- Ok RangeField 6 10

        parse monthBaseFieldParser "5-a"
        -- Err

-}
monthBaseFieldParser : C.Parser s BaseField
monthBaseFieldParser =
    simpleBaseFieldParser
        <|> C.map RangeField (monthNameParser <* C.string "-")
        <*> monthNameParser
        <|> C.map SpecificField monthNameParser


{-| Parser for the BaseField for the day-of-week entry. Same as `simpleBaseFieldParser`, but also handles the alternative notation.

        import Combine exposing (parse)
        parse dayOfWeekBaseFieldParser "*"
        -- Ok Star

        parse dayOfWeekBaseFieldParser "5"
        -- Ok SpecificField 5

        parse dayOfWeekBaseFieldParser "SUN-FRI"
        -- Ok RangeField 0 5

        parse dayOfWeekBaseFieldParser "5-a"
        -- Err

-}
dayOfWeekBaseFieldParser : C.Parser s BaseField
dayOfWeekBaseFieldParser =
    simpleBaseFieldParser
        <|> C.map RangeField (dayNameParser <* C.string "-")
        <*> dayNameParser
        <|> C.map SpecificField dayNameParser


{-| Parser for the step option of the cron field. On success, it returns `Nothing` if no step option exists or `Just n` if it does, where `n` is the integer after `/`

        import Combine exposing (parse)
        parse stepParser ""
        -- Ok Nothing

        parse stepParser "5"
        -- Err

        parse stepParser "/5"
        -- Ok (Just 5)

-}
stepParser : C.Parser s (Maybe Int)
stepParser =
    (C.string "/" *> C.map Just CN.int <* C.string "")
        <|> (Nothing <$ C.string "")


{-| Parser which takes an appropriate `BaseField` parser and returns a `CronField` on success.

        import Combine exposing (parse)
        parse (cronFieldParser dayOfWeekBaseFieldParser) "MON-FRI/2"
        -- Ok CronField (RangeField 1 5) (Just 2)

        parse (cronFieldParser dayOfWeekBaseFieldParser) "MON-FRI/"
        -- Err

        parse (cronFieldParser simpleBaseFieldParser) "5"
        -- Ok CronField (SpecificField 5) Nothing

-}
cronFieldParser : C.Parser s BaseField -> C.Parser s (List CronField)
cronFieldParser bFP =
    C.sepBy (C.string ",") (CronField <$> bFP <*> stepParser)
