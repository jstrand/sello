module TimeSheet exposing (..)

import Time.Date exposing (..)

type alias TimeOfDay =
  { hour: Int
  , minute: Int
  }

type alias Report =
  { date: Date
  , start: TimeOfDay
  -- , end: Time
  }


-- Given a date return a list with that date and the following
-- 6 days
weekDates : Date -> List Date
weekDates startDate =
  let
    addToStart offset = addDays offset startDate
  in
    List.range 0 6
    |> List.map addToStart


-- Give a day in the week, how far is it walking backwards to a monday?
mondayOffset : Weekday -> Int
mondayOffset weekday =
  case weekday of
    Mon -> 0
    Tue -> -1
    Wed -> -2
    Thu -> -3
    Fri -> -4
    Sat -> -5
    Sun -> -6


-- Given a date, go back to the monday of that week
mondayOfWeek : Date -> Date
mondayOfWeek date =
  let
    dayInWeek = weekday date
    offset = mondayOffset dayInWeek
  in
    addDays offset date
