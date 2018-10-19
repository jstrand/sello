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

dates : List Date
dates =
  let dayInInterval = Date.fromCalendarDate 2018 Time.Oct 12
      start = Date.floor Date.Year dayInInterval
      stop = Date.ceiling Date.Year dayInInterval
  in
    Date.range Date.Day 1 start stop

type AppStatus = Loading | Saving | Ready | Failure String

type alias Model =
  { reports: ReportDict
  , editing: Maybe ReportInput
  , status: AppStatus
  }

url = "http://localhost:8001/sello/reports"

saveReports : ReportDict -> Cmd Msg
saveReports reports =
  Http.post url (Http.jsonBody (encodeReports reports)) Decode.value
  |> Http.send SaveResult

loadReports : Cmd Msg
loadReports =
  Http.get url decodeReports
  |> Http.send LoadResult

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

saveField updateF model =
  case model.editing of
    Nothing -> model
    Just input ->
      { model
      | editing = Just <| updateF input}

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
    InputStart start -> (saveField (saveStart start) model, Cmd.none)
    InputStop stop -> (saveField (saveStop stop) model, Cmd.none)
    InputPause pause -> (saveField (savePause pause) model, Cmd.none)
    InputExpected expected -> (saveField (saveExpected expected) model, Cmd.none)
    SaveResult (Ok _) -> ({ model | status = Ready }, Cmd.none)
    SaveResult (Err error) -> failWith error model
    LoadResult (Ok reports) -> ({ model | reports = reports, status = Ready }, Cmd.none)
    LoadResult (Err error) -> failWith error model

editCell readOnly day =
  if readOnly then
    emptyCell
  else
    Html.td
      []
      [Html.button [Events.onClick (StartEdit day), Att.class "btn btn-primary"] [Html.text "Edit"]]

cell = Html.td []

emptyCell = cell []

cellWithOne one = cell [one]

textCell value = Html.td [] [Html.text <| value]

timeCell = textCell << Report.showAsHoursAndMinutes

viewEmptyDay : Bool -> Date -> ReportDict -> Html Msg
viewEmptyDay editingOtherDay day reports =
  Html.tr []
    [ textCell <| Date.toIsoString day
    , emptyCell
    , emptyCell
    , emptyCell
    , emptyCell
    , emptyCell
    , emptyCell
    , emptyCell
    , emptyCell
    , editCell editingOtherDay day
    ]

viewDay : Bool -> Date -> ReportDict -> Report -> Html Msg
viewDay readOnly day reports report =
  Html.tr []
    [ textCell <| Date.toIsoString day
    , emptyCell
    , timeCell report.start
    , timeCell <| Report.getEnd report
    , timeCell report.pausedMinutes
    , timeCell report.expected
    , timeCell <| Report.getWorkedMinutes report
    , timeCell <| Report.getDiff report
    , timeCell <| runningTotal reports day
    , editCell readOnly day
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
      [ textCell <| Date.toIsoString input.date
      , emptyCell
      , cell [viewTimeInputField InputStart input.start]
      , cell [viewTimeInputField InputStop input.stop]
      , cell [viewTimeInputField InputPause input.pause]
      , cell [viewTimeInputField InputExpected input.expected]
      , textCell workedTime
      , textCell diff
      , textCell totalValue
      , cell [viewOkButton input, viewCancelButton]
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
