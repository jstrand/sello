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
  let
    format getter =
      report
      |> getter
      |> Maybe.map showAsHoursAndMinutes
      |> Maybe.withDefault ""
  in
    ReportInput
      date
      (format getStart)
      (format getEnd)
      (format getPause)
      (format getExpected)


type alias Report =
  { start: Maybe Minutes
  , stop: Maybe Minutes
  , pause: Maybe Minutes
  , expected: Maybe Minutes
  }

emptyReport =
  Report Nothing Nothing Nothing Nothing

hours : Minutes -> Hours
hours h = h * 60

hoursAndMinutes : Int -> Int -> Int
hoursAndMinutes h m = hours h + m

getWorkedMinutesWithNegative : Report -> Maybe Minutes
getWorkedMinutesWithNegative report =
  case (report.start, report.stop, report.pause) of
    (Just start, Just stop, maybePause)
      -> Just <| stop - start - (Maybe.withDefault 0 maybePause)
    _ -> Nothing

getWorkedMinutes : Report -> Maybe Minutes
getWorkedMinutes report =
  let
      negativeIsNothing x = if x < 0 then Nothing else Just x
  in
    getWorkedMinutesWithNegative report
    |> Maybe.andThen negativeIsNothing

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

getStart : Report -> Maybe Minutes
getStart report = report.start

getEnd : Report -> Maybe Minutes
getEnd report = report.stop

getPause : Report -> Maybe Minutes
getPause report = report.pause

getExpected : Report -> Maybe Minutes
getExpected report = report.expected

getDiff : Report -> Maybe Minutes
getDiff report =
  getDiffResult report
  |> Result.toMaybe
  |> Maybe.withDefault Nothing

getDiffResult : Report -> Result String (Maybe Minutes)
getDiffResult report =
  case (getWorkedMinutes report, getExpected report) of
  (Just worked, Just expected) -> worked - expected |> Just |> Ok
  (Just _, Nothing) -> Err "No expected time!"
  (Nothing, Just 0) -> Ok Nothing
  (Nothing, Just _) -> Err "No worked time!"
  _ -> Ok Nothing

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
  Report
    (parseTime report.start)
    (parseTime report.stop)
    (parseTime report.pause)
    (parseTime report.expected)


inputOk : ReportInput -> Bool
inputOk = List.isEmpty << inputErrors

checkStartBeforeStop start stop =
  if start >= stop then
    ["You have to start before you stop"]
  else
    []

inputErrors : ReportInput -> List String
inputErrors input =
  let
      report = parseReportInput input
      startBeforeStopError =
        case (parseTime input.start, parseTime input.stop) of
          (Just start, Just stop) ->
            checkStartBeforeStop start stop
          _ -> []
      nothingError caption value =
        case (parseTime value, value) of
          (Just _, _) -> []
          (Nothing, "") -> []
          (Nothing, _) -> [caption]
      tooMuchPause =
        if Maybe.withDefault 0 (getWorkedMinutesWithNegative report) < 0 then
          ["Too much pause!"]
        else
          []
  in
    startBeforeStopError
    ++ nothingError "What up with start" input.start
    ++ nothingError "What up with stop" input.stop
    ++ nothingError "What up with pause" input.pause
    ++ nothingError "What up with expected" input.expected
    ++ tooMuchPause


encodeReport : Int -> Report -> Encode.Value
encodeReport ratadie report =
  let
    maybeEncode name maybeValue =
      case maybeValue of
          Just value -> [(name, value |> Encode.int)]
          Nothing -> []
  in
    Encode.object
      ([ ("dateAsRataDie", ratadie |> Encode.int)
      ]
      ++ maybeEncode "start" report.start
      ++ maybeEncode "stop" report.stop
      ++ maybeEncode "pause" report.pause
      ++ maybeEncode "expected" report.expected
      )

decodeReport : Decode.Decoder Report
decodeReport =
  let
    maybeDecodeInt fieldName =
      Decode.maybe (Decode.field fieldName Decode.int)
  in
    Decode.map4 Report
      (maybeDecodeInt "start")
      (maybeDecodeInt "stop")
      (maybeDecodeInt "pause")
      (maybeDecodeInt "expected")

type alias ReportDict = Dict Int Report

noReports = Dict.empty

reportsToList : ReportDict -> List (Date, Report)
reportsToList reports =
  reports
  |> Dict.toList
  |> List.map (Tuple.mapFirst Date.fromRataDie) 

encodeReports : ReportDict -> Encode.Value
encodeReports reports =
  Dict.filter (\key value -> value /= emptyReport) reports
  |> Dict.map encodeReport
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
    diff report = Maybe.withDefault 0 (getDiff report)
    sum report acc = acc + (diff report)
  in
    List.foldl sum 0 reportsUntil

saveReport : ReportInput -> ReportDict -> ReportDict
saveReport reportInput reports =
  Dict.insert (Date.toRataDie reportInput.date) (parseReportInput reportInput) reports

getReport : ReportDict -> Date -> Maybe Report
getReport reports date =
    Dict.get (Date.toRataDie date) reports

getReportOrEmpty : ReportDict -> Date -> Report
getReportOrEmpty reports date =
  Maybe.withDefault
    emptyReport
    (getReport reports date)

defaultInput : Date -> ReportInput
defaultInput date =
  ReportInput date "" "" "" "08:00"

getReportInput : ReportDict -> Date -> ReportInput
getReportInput reports date =
  getReport reports date
  |> Maybe.map (reportToInput date)
  |> Maybe.withDefault (defaultInput date)
