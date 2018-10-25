module Report exposing (..)

import String exposing (fromInt)
import Date exposing (Date, toRataDie)
import Time exposing (Month(..))
import Dict exposing (Dict)
import Tuple exposing (pair)

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

saveStart start input =
  { input | start = start }

saveStop stop input =
  { input | stop = stop }

savePause pause input =
  { input | pause = pause }

saveExpected expected input =
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
  , stop: Minutes
  , pause: Minutes
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
  report.stop - report.start - report.pause

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
getEnd report = report.stop

getPause : Report -> Int
getPause report = report.pause

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
      expectedMinutes = parseTime report.expected |> Maybe.withDefault 0
      pauseInMinutes = parseTime report.pause |> Maybe.withDefault 0
  in
    Report startInMinutes stopInMinutes pauseInMinutes expectedMinutes


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
    , ("stop", report.stop |> Encode.int)
    , ("pause", report.pause |> Encode.int)
    , ("expected", report.expected |> Encode.int)
    ]

decodeReport : Decode.Decoder Report
decodeReport =
  Decode.map4 Report
      (Decode.field "start" Decode.int)
      (Decode.field "stop" Decode.int)
      (Decode.field "pause" Decode.int)
      (Decode.field "expected" Decode.int)

type alias ReportDict = Dict Int Report

encodeReports : ReportDict -> Encode.Value
encodeReports reports =
  Dict.map encodeReport reports
  |> Dict.values
  |> Encode.list identity

decodeReportWithDate : Decode.Decoder (Int, Report)
decodeReportWithDate =
  Decode.map2
    pair
    (Decode.field "dateAsRataDie" Decode.int)
    decodeReport

decodeReportsPerDay : Decode.Decoder (List (Int, Report))
decodeReportsPerDay = 
  Decode.list decodeReportWithDate

decodeReports : Decode.Decoder ReportDict
decodeReports =
  Decode.map Dict.fromList decodeReportsPerDay

runningTotal : ReportDict -> Date -> Minutes
runningTotal reports date =
  let
    before reportForDay _ = reportForDay <= (Date.toRataDie date) 
    reportsUntil = Dict.filter before reports |> Dict.values
    sum report acc = acc + (getDiff report)
  in
    List.foldl sum 0 reportsUntil

saveReport : ReportInput -> ReportDict -> ReportDict
saveReport reportInput reports =
  Dict.insert (Date.toRataDie reportInput.date) (parseReportInput reportInput) reports

getReport : ReportDict -> Date -> Maybe Report
getReport reports date =
  Dict.get (Date.toRataDie date) reports

defaultInput : Date -> ReportInput
defaultInput date =
  ReportInput date "" "" "" "08:00"

getReportInput : ReportDict -> Date -> ReportInput
getReportInput reports date =
  getReport reports date
  |> Maybe.map (reportToInput date)
  |> Maybe.withDefault (defaultInput date)
