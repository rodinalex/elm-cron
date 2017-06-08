# elm-cron
[![Build Status](https://travis-ci.org/rodinalex/elm-cron.svg?branch=master)](https://travis-ci.org/rodinalex/elm-cron)

The purpose of this library is to provide a tool for parsing standard five-field Crontabs. Upon success, a `CronSchedule` is generated. This structure can be further processed to turn it either into a record containing descriptions of inidividual fields or a single string describing the whole Crontab:

```
        
Maybe.map describeSchedule <| decodeCronTab "12-27/3 * * JAN MON-FRI" =
        Just {
        minuteDescription = "every 3 minute between 12 and 27"
      , hourDescription = "every hour"
      , dayDescription = "every day"
      , monthDescription = "at month 1"
      , dayOfWeekDescription = "every day of the week between 1 and 5" }        
      
Maybe.map scheduleDescription <| decodeCronTab "12-27/3 * * JAN MON-FRI" =
        Just "every 3 minute between 12 and 27; every hour; every day; at month 1; every day of the week between 1 and 5"      
```
