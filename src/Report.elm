module Report exposing (..)

import String exposing (fromInt)
import Date exposing (Date)
import Time exposing (Month(..))

-- Minutes since midnight 0 -> 00:00
type alias Minutes = Int
type alias Hours = Int

type alias Report =
  { start: Minutes
  , minutesUntilStop: Minutes
  , pausedMinutes: Minutes
  }

type alias ReportValidation =
  { startValid: Bool
  , endValid: Bool
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

testReport1 = Report (hours 8) (hours 9) (hours 1)

testWorkedMinutes =
    workedMinutes testReport1 == hours 8

getStart : Report -> Minutes
getStart report = report.start

getEnd : Report -> Minutes
getEnd report = report.start + report.minutesUntilStop

getDiff : Report -> Minutes
getDiff report = report.minutesUntilStop - report.pausedMinutes

testGetEnd =
  (getEnd testReport1 |> showAsHoursAndMinutes) == "17:00"

getPause : Report -> Int
getPause report = report.pausedMinutes

testGetPause =
  (getPause testReport1 |> showAsHoursAndMinutes) == "01:00"

inRange : Int -> Int -> Int -> Bool
inRange min max value = value >= min && value <= max

validHours : Int -> Bool
validHours = inRange 0 23

validMinutes : Int -> Bool
validMinutes = inRange 0 59

-- 8 -> 8h -> 480
-- 8:30 -> 8h 30m -> 510
parseTime : String -> Maybe Minutes
parseTime time =
  let hoursAndMinutesList = String.split ":" time |> List.map String.toInt
--      hoursOnly = List.length hoursAndMinutesList == 1
--      hoursAndMinutes = List.length hoursAndMinutes == 2
  in
    case hoursAndMinutesList of
    (Just h::[]) -> if validHours h then Just <| hours h else Nothing
    (Just h::Just m::[]) -> if validHours h && validMinutes m then Just <| hours h + m else Nothing
    _ -> Nothing

isValidTimeInput : String -> Bool
isValidTimeInput time =
  case parseTime time of
  Just _ -> True
  _ -> False

parseReport : String -> String -> Report
parseReport start stop =
  let startInMinutes = parseTime start |> Maybe.withDefault 0
      stopInMinutes = parseTime stop |> Maybe.withDefault 0
      minutesUntilStop = stopInMinutes - startInMinutes
      pauseInMinutes = 0
  in
    Report startInMinutes minutesUntilStop pauseInMinutes

-- In case of no error return Nothing, otherwise message
inputError : String -> String -> Maybe String
inputError start stop =
  let maybeStart = parseTime start
      maybeStop = parseTime stop
  in
    case (maybeStart, maybeStop) of
      (Just startMinutes, Just stopMinutes) -> if startMinutes > stopMinutes then Just "You have to start before you stop" else Nothing
      (Just _, Nothing) -> Just "Somethings up with the stop time"
      (Nothing, Just _) -> Just "Somethings up with the start time"
      _ -> Just "Get real"
    