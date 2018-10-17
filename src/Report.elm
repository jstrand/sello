module Report exposing (..)

import String exposing (fromInt)
import Date exposing (Date, toRataDie)
import Time exposing (Month(..))

import Json.Decode as Decode
import Json.Encode as Encode

-- Minutes since midnight 0 -> 00:00
type alias Minutes = Int
type alias Hours = Int

type alias ReportInput =
  { date: Date
  , start: String
  , stop: String
  , pause: String
  , expected: String
  }

saveStart input start =
  { input | start = start }

saveStop input stop =
  { input | stop = stop }

savePause input pause =
  { input | pause = pause }

saveExpected input expected =
  { input | expected = expected}

reportToInput : Date -> Report -> ReportInput
reportToInput date report =
  ReportInput
    date
    (showAsHoursAndMinutes (getStart report))
    (showAsHoursAndMinutes (getEnd report))
    (showAsHoursAndMinutes (getPause report))
    (showAsHoursAndMinutes (getExpected report))

type alias Report =
  { start: Minutes
  , minutesUntilStop: Minutes
  , pausedMinutes: Minutes
  , expected: Minutes
  }

type alias ReportValidation =
  { startValid: Bool
  , endValid: Bool
  }

hours : Minutes -> Hours
hours h = h * 60

hoursAndMinutes : Int -> Int -> Int
hoursAndMinutes h m = hours h + m

getWorkedMinutes report =
  report.minutesUntilStop - report.pausedMinutes

getHoursAndMinutes : Minutes -> (Hours, Minutes)
getHoursAndMinutes m =
  (floor <| (toFloat (abs m)) / 60.0, modBy 60 (abs m))

showAsHoursAndMinutes : Minutes -> String
showAsHoursAndMinutes time =
  let sign = if time < 0 then "-" else ""
      (h, m) = getHoursAndMinutes time 
      format = fromInt >> String.padLeft 2 '0'
  in
    sign ++ format h ++ ":" ++ format m

getStart : Report -> Minutes
getStart report = report.start

getEnd : Report -> Minutes
getEnd report = report.start + report.minutesUntilStop

getPause : Report -> Int
getPause report = report.pausedMinutes

getExpected : Report -> Minutes
getExpected report = report.expected

getDiff : Report -> Minutes
getDiff report = getWorkedMinutes report - getExpected report

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
  in
    case hoursAndMinutesList of
    [Just h] -> if validHours h then Just <| hours h else Nothing
    [Just h, Just m] -> if validHours h && validMinutes m then Just <| hours h + m else Nothing
    [Nothing, Just m] -> if validMinutes m then Just m else Nothing
    _ -> Nothing

isValidTimeInput : String -> Bool
isValidTimeInput time =
  case parseTime time of
  Just _ -> True
  _ -> False

parseReportInput : ReportInput -> Report
parseReportInput report =
  let startInMinutes = parseTime report.start |> Maybe.withDefault 0
      stopInMinutes = parseTime report.stop |> Maybe.withDefault 0
      minutesUntilStop = stopInMinutes - startInMinutes
      expectedMinutes = parseTime report.expected |> Maybe.withDefault 0
      pauseInMinutes = parseTime report.pause |> Maybe.withDefault 0
  in
    Report startInMinutes minutesUntilStop pauseInMinutes expectedMinutes


inputOk : ReportInput -> Bool
inputOk = List.isEmpty << inputErrors

inputErrors : ReportInput -> List String
inputErrors input =
  let maybeStart = parseTime input.start
      maybeStop = parseTime input.stop
      maybePause = parseTime input.pause
      maybeExpected = parseTime input.expected
      startBeforeStopError =
        case (maybeStart, maybeStop) of
          (Just startMinutes, Just stopMinutes) ->
            if startMinutes >= stopMinutes then
              ["You have to start before you stop"]
            else
              []
          _ -> []
      nothingError caption value =
        case value of
          Just _ -> []
          _ -> [caption]
  in
    startBeforeStopError
    ++ nothingError "What up with start" maybeStart
    ++ nothingError "What up with stop" maybeStop
    ++ nothingError "What up with pause" maybePause
    ++ nothingError "What up with expected" maybeExpected


encodeReport : Int -> Report -> Encode.Value
encodeReport ratadie report = 
  Encode.object
    [ ("dateAsRataDie", ratadie |> Encode.int)
    , ("start", report.start |> Encode.int)
    , ("duration", report.minutesUntilStop |> Encode.int)
    , ("paused", report.pausedMinutes |> Encode.int)
    , ("expected", report.expected |> Encode.int)
    ]
