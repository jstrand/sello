module Main exposing (..)

import Tuple exposing (pair)
import Html exposing (Html)
import Html.Attributes as Att
import Html.Events as Events
import Browser
import Time
import DateFormat
import Dict exposing (Dict)
import Date exposing (Date)
import Json.Encode as Encode
import Json.Decode as Decode
import Http

import Report exposing (..)

defaultInput : Date -> ReportInput
defaultInput date =
  ReportInput date "08:00" "17:00" "01:00" "08:00"

dates : List Date
dates =
  let dayInInterval = Date.fromCalendarDate 2018 Time.Oct 12
      start = Date.floor Date.Year dayInInterval
      stop = Date.ceiling Date.Year dayInInterval
  in
    Date.range Date.Day 1 start stop

type alias ReportDict = Dict Int Report

type AppStatus = Loading | Saving | Ready | Failure String

type alias Model =
  { reports: ReportDict
  , editing: Maybe ReportInput
  , status: AppStatus
  }

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

url = "http://localhost:8001/sello/reports"

saveReports : ReportDict -> Cmd Msg
saveReports reports =
  Http.post url (Http.jsonBody (encodeReports reports)) Decode.value
  |> Http.send SaveResult

loadReports : Cmd Msg
loadReports =
  Http.get url decodeReports
  |> Http.send LoadResult

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

getReportInput : ReportDict -> Date -> ReportInput
getReportInput reports date =
  getReport reports date
  |> Maybe.map (reportToInput date)
  |> Maybe.withDefault (defaultInput date)

type Msg
    = StartEdit Date
    | CancelEdit
    | SaveEdit
    | InputStart String
    | InputStop String
    | InputPause String
    | InputExpected String
    | SaveResult (Result Http.Error Decode.Value)
    | LoadResult (Result Http.Error ReportDict)

startReportInput model date =
  { model | editing = Just (getReportInput model.reports date) }

cancelReportInput model = 
  { model | editing = Nothing }

saveReportInput : Model -> Model
saveReportInput model =
  case model.editing of
    Nothing -> model
    Just input ->
      { model
      | editing = Nothing
      , reports = saveReport input model.reports
      }

saveModelStart start model =
  case model.editing of
    Nothing -> model
    Just input ->
      { model
      | editing = Just <| saveStart input start}

saveModelStop stop model =
  case model.editing of
    Nothing -> model
    Just input ->
      { model
      | editing = Just <| saveStop input stop}

saveModelExpected expected model =
  case model.editing of
    Nothing -> model
    Just input ->
      { model
      | editing = Just <| saveExpected input expected}

saveModelPause pause model =
  case model.editing of
    Nothing -> model
    Just input ->
      { model
      | editing = Just <| savePause input pause}

failWith error model =
  ({ model | status = Failure (Debug.toString error)}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartEdit date -> (startReportInput model date, Cmd.none)
    CancelEdit -> (cancelReportInput model, Cmd.none)
    SaveEdit -> 
      let newModel = saveReportInput model
      in ({ newModel | status = Saving }, saveReports newModel.reports)
    InputStart start -> (saveModelStart start model, Cmd.none)
    InputStop stop -> (saveModelStop stop model, Cmd.none)
    InputPause pause -> (saveModelPause pause model, Cmd.none)
    InputExpected expected -> (saveModelExpected expected model, Cmd.none)
    SaveResult (Ok _) -> ({ model | status = Ready }, Cmd.none)
    SaveResult (Err error) -> failWith error model
    LoadResult (Ok reports) -> ({ model | reports = reports, status = Ready }, Cmd.none)
    LoadResult (Err error) -> failWith error model

editCell readOnly day =
  if readOnly then
    []
  else
    [Html.button [Events.onClick (StartEdit day), Att.class "btn btn-primary"] [Html.text "Edit"]]

viewEmptyDay : Bool -> Date -> ReportDict -> Html Msg
viewEmptyDay editingOtherDay day reports =
  Html.tr []
    [ Html.td [] [Html.text <| Date.toIsoString day]
    , Html.td [] []
    , Html.td [] []
    , Html.td [] []
    , Html.td [] []
    , Html.td [] []
    , Html.td [] []
    , Html.td [] []
    , Html.td [] []
    , Html.td [] (editCell editingOtherDay day)
    ]

viewDay : Bool -> Date -> ReportDict -> Report -> Html Msg
viewDay readOnly day reports report =
  Html.tr []
    [ Html.td [] [Html.text <| Date.toIsoString day]
    , Html.td [] []
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| report.start]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| Report.getEnd report]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| report.pausedMinutes]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| report.expected]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| Report.getWorkedMinutes report]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| Report.getDiff report]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| runningTotal reports day]
    , Html.td [] (editCell readOnly day)
    ]

viewOkButton input =
  let disabledIfErr =
        Report.inputErrors input |> List.isEmpty |> not
      tooltip = Report.inputErrors input |> String.join ", "
  in
    Html.button [Events.onClick SaveEdit, Att.disabled disabledIfErr, Att.title tooltip, Att.class "btn btn-primary"] [Html.text "OK"]

viewCancelButton = Html.button [Events.onClick CancelEdit, Att.class "btn btn-secondary"] [Html.text "Cancel"]

viewTimeInputField event value =
  Html.input
    [ Events.onInput event
    , Att.value value
    , Att.size 1
    , Att.maxlength 5
    , Att.class "form-control"
    ]
    []

editDay : Date -> ReportInput -> ReportDict -> Html Msg
editDay date input reports =
  let
    inputOk = inputErrors input |> List.isEmpty
    valueOrEmpty value = if inputOk then value else ""
    potentialReport = parseReportInput input
    workedTime = potentialReport |> getWorkedMinutes |> Report.showAsHoursAndMinutes |> valueOrEmpty
    diff = potentialReport |> getDiff |> Report.showAsHoursAndMinutes |> valueOrEmpty
    potentialReports = saveReport input reports
    totalValue = runningTotal potentialReports date |> Report.showAsHoursAndMinutes |> valueOrEmpty
  in
    Html.tr []
      [ Html.td [] [Html.text <| Date.toIsoString input.date]
      , Html.td [] []
      , Html.td [] [viewTimeInputField InputStart input.start]
      , Html.td [] [viewTimeInputField InputStop input.stop]
      , Html.td [] [viewTimeInputField InputPause input.pause]
      , Html.td [] [viewTimeInputField InputExpected input.expected]
      , Html.td [] [workedTime |> Html.text]
      , Html.td [] [diff |> Html.text]
      , Html.td [] [totalValue |> Html.text]
      , Html.td [] [viewOkButton input, viewCancelButton]
      ]

viewOrEditDay : Model -> Date -> Html Msg
viewOrEditDay model day =
  let report = getReport model.reports day
      viewDayOrEmpty readOnly =
        report
        |> Maybe.map (viewDay readOnly day model.reports)
        |> Maybe.withDefault (viewEmptyDay readOnly day model.reports)
  in
    case model.editing of
      Nothing -> viewDayOrEmpty (model.status == Saving)
      Just input -> if day == input.date then editDay day input model.reports else viewDayOrEmpty True

header caption = Html.th [] [Html.text caption]

reportHeaders =
  List.map header
    [ "Date"
    , "Day"
    , "Start"
    , "Stop"
    , "Pause"
    , "Expected"
    , "Worked"
    , "Diff"
    , "Total"
    , "Commands"
    ]

viewReports model =
  [ Html.table [Att.class "table"]
    (reportHeaders
    ++ (List.map (viewOrEditDay model) dates)
    )
  ]

viewBusy = 
  [ Html.text "Please wait..." ]

viewError error =
  [ Html.text error ]

view : Model -> Browser.Document Msg
view model =
  let currentContents =
        case model.status of
          Loading -> viewBusy
          Saving -> viewReports model
          Ready -> viewReports model
          Failure error -> viewError error
  in
    Browser.Document "Sello" currentContents
    

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


initReports =
  Dict.empty


init : () -> (Model, Cmd Msg)
init _ = (Model initReports Nothing Loading, loadReports)


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
