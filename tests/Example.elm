module Example exposing (..)

import Cron.Decoder exposing (decodeCronTab)
import Cron.Types exposing (..)
import Expect
import Test exposing (..)


decoderTests : Test
decoderTests =
    describe "Decoder"
        [ assertParse "* * * * *" <| CronSchedule starField starField starField starField starField
        , assertParse "1 * * * *" <| CronSchedule (specificField 1) starField starField starField starField
        , assertParse "* 1 * * *" <| CronSchedule starField (specificField 1) starField starField starField
        , assertParse "* * 1 * *" <| CronSchedule starField starField (specificField 1) starField starField
        , assertParse "* * * 1 *" <| CronSchedule starField starField starField (specificField 1) starField
        , assertParse "* * * * 1" <| CronSchedule starField starField starField starField (specificField 1)
        , assertParse "1,2 * * * *" <|
            CronSchedule (specificField 1 ++ specificField 2) starField starField starField starField
        , assertParse "* 1,2 * * *" <|
            CronSchedule starField (specificField 1 ++ specificField 2) starField starField starField
        , assertParse "* * 1,2 * *" <|
            CronSchedule starField starField (specificField 1 ++ specificField 2) starField starField
        , assertParse "* * * 1,2 *" <|
            CronSchedule starField starField starField (specificField 1 ++ specificField 2) starField
        , assertParse "* * * * 1,2" <|
            CronSchedule starField starField starField starField (specificField 1 ++ specificField 2)
        , assertParse "1-2 * * * *" <|
            CronSchedule (rangeField 1 2) starField starField starField starField
        , assertParse "* 1-2 * * *" <|
            CronSchedule starField (rangeField 1 2) starField starField starField
        , assertParse "* * 1-2 * *" <|
            CronSchedule starField starField (rangeField 1 2) starField starField
        , assertParse "* * * 1-2 *" <|
            CronSchedule starField starField starField (rangeField 1 2) starField
        , assertParse "* * * * 1-2" <|
            CronSchedule starField starField starField starField (rangeField 1 2)
        , assertParse "*/1 * * * *" <|
            CronSchedule (starField_ 1) starField starField starField starField
        , assertParse "* */1 * * *" <|
            CronSchedule starField (starField_ 1) starField starField starField
        , assertParse "* * */1 * *" <|
            CronSchedule starField starField (starField_ 1) starField starField
        , assertParse "* * * */1 *" <|
            CronSchedule starField starField starField (starField_ 1) starField
        , assertParse "* * * * */1" <|
            CronSchedule starField starField starField starField (starField_ 1)
        , assertNoParse "* * * * * *"
        , assertNoParse "* * * *"
        , assertNoParse "*/1/1 * * * *"
        , assertNoParse "1-2-3 * * * *"
        , assertNoParse "1,,2 * * * *"
        , assertNoParse "a * * * *"
        ]


starField : List CronField
starField =
    [ CronField Star Nothing ]


starField_ : Int -> List CronField
starField_ i =
    [ CronField Star (Just i) ]


specificField : Int -> List CronField
specificField i =
    [ CronField (SpecificField i) Nothing ]


rangeField : Int -> Int -> List CronField
rangeField x y =
    [ CronField (RangeField x y) Nothing ]


assertParse : String -> CronSchedule -> Test
assertParse tab res =
    test (tab ++ " should parse") <|
        \_ -> Expect.equal (decodeCronTab tab) (Just res)


assertNoParse : String -> Test
assertNoParse tab =
    test (tab ++ " should not parse") <|
        \_ -> Expect.equal (decodeCronTab tab) Nothing
