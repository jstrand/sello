module TimeSheet exposing (..)

import Time exposing (..)
import String exposing (fromInt)

type alias TimeOfDay =
  { hour: Int
  , minute: Int
  }

timeString : TimeOfDay -> String
timeString time =
  let
    format = fromInt >> String.padLeft 2 '0'
  in
    format time.hour ++ ":" ++ format time.minute

-- 08:00 -> { hour = 8, minute = 0}
timeOfDay : String -> TimeOfDay
timeOfDay hourAndMinute =
  let
    valueOrZero = String.toInt >> Maybe.withDefault 0
  in
  case String.split ":" hourAndMinute of
    hour::minutes::[] -> TimeOfDay (valueOrZero hour) (valueOrZero minutes)
    _ -> TimeOfDay 0 0

type alias Report =
  { date: Posix
  , start: TimeOfDay
  -- , end: Time
  }

-- Given a date return a list with that date and the following
-- 6 days
-- weekDates : Date -> List Date
-- weekDates startDate =
--   let
--     addToStart offset = addDays offset startDate
--   in
--     List.range 0 6
--     |> List.map addToStart


-- -- Give a day in the week, how far is it walking backwards to a monday?
-- mondayOffset : Weekday -> Int
-- mondayOffset weekday =
--   case weekday of
--     Mon -> 0
--     Tue -> -1
--     Wed -> -2
--     Thu -> -3
--     Fri -> -4
--     Sat -> -5
--     Sun -> -6


-- -- Given a date, go back to the monday of that week
-- mondayOfWeek : Date -> Date
-- mondayOfWeek date =
--   let
--     dayInWeek = weekday date
--     offset = mondayOffset dayInWeek
--   in
--     addDays offset date
