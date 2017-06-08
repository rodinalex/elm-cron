module Cron.Types
    exposing
        ( BaseField(..)
        , CronField(..)
        , CronSchedule(..)
        , DescribedCronSchedule
        )

{-| This module contains the types used in parsing cron expressions.

@docs BaseField, CronField, CronSchedule, DescribedCronSchedule

-}


{-| Basic unit of Crontab. It can be one of the following three cases:

  - `Star` corresponds to `*`
  - `SpecificField n` describes a specific value given by an integer `n`
  - `RangeField m n` describes the range `m`-`n` with `m`<`n`

-}
type BaseField
    = Star
    | SpecificField Int
    | RangeField Int Int


{-| `CronField` includes the `BaseField` and a possible step. For example, a field `*/12`
will be rendered as

    sF : CronField

    sf =
        CronField (BaseField Star) (Just 12)

-}
type CronField
    = CronField BaseField (Maybe Int)


{-| `CronSchedule` contains the `CronField`'s for the five parameters:

    cS : CronSchedule
    cS =
        CronSchedule minutes hours days months day_of_week

-}
type CronSchedule
    = CronSchedule (List CronField) (List CronField) (List CronField) (List CronField) (List CronField)


{-| A record containing readable descriptions for the Crontab fields.
-}
type alias DescribedCronSchedule =
    { minuteDescription : String
    , hourDescription : String
    , dayDescription : String
    , monthDescription : String
    , dayOfWeekDescription : String
    }
