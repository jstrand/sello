module Report exposing (..)

import String exposing (fromInt)
import Date exposing (Date)
import Time exposing (Month(..))

-- Minutes since midnight 0 -> 00:00
type alias Minutes = Int
type alias Hours = Int

type alias Report =
  { day: Date
  , start: Minutes
  , minutesUntilStop: Minutes
  , pausedMinutes: Minutes
  }

hours : Minutes -> Hours
hours h = h * 60

hoursAndMinutes : Int -> Int -> Int
hoursAndMinutes h m = hours h + m

workedMinutes report =
  report.minutesUntilStop - report.pausedMinutes

getHoursAndMinutes : Minutes -> (Hours, Minutes)
getHoursAndMinutes m =
  (floor <| (toFloat m) / 60.0, modBy 60 m)

showAsHoursAndMinutes : Minutes -> String
showAsHoursAndMinutes time =
  let (h, m) = getHoursAndMinutes time 
      format = fromInt >> String.padLeft 2 '0'
  in
    format h ++ ":" ++ format m

testShowAsHoursAndMinutes =
  showAsHoursAndMinutes (hoursAndMinutes 8 10) == "08:10"
  &&
  showAsHoursAndMinutes (hoursAndMinutes 0 0) == "00:00"

testReport1 = Report (Date.fromCalendarDate 2018 Oct 12) (hours 8) (hours 9) (hours 1)

testWorkedMinutes =
    workedMinutes testReport1 == hours 8

getStart : Report -> Minutes
getStart report = report.start

getEnd : Report -> Minutes
getEnd report = report.start + report.minutesUntilStop

testGetEnd =
  (getEnd testReport1 |> showAsHoursAndMinutes) == "17:00"

getPause : Report -> Int
getPause report = report.pausedMinutes

testGetPause =
  (getPause testReport1 |> showAsHoursAndMinutes) == "01:00"

parseReport : String -> String -> Report
parseReport start stop =
  testReport1
