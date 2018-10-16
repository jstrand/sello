import Html exposing (Html)
import Html.Attributes as Att
import Html.Events as Events
import Browser
import Time
import DateFormat
import Dict exposing (Dict)
import Date exposing (Date)

import Report exposing (..)

defaultInput : Date -> ReportInput
defaultInput date =
  ReportInput date "08:00" "17:00" "8:00"

dates : List Date
dates =
  let dayInInterval = Date.fromCalendarDate 2018 Time.Oct 12
      start = Date.floor Date.Year dayInInterval
      stop = Date.ceiling Date.Year dayInInterval
  in
    Date.range Date.Day 1 start stop

type alias ReportDict = Dict Int Report

type alias Model =
  { reports: ReportDict
  , editing: Maybe ReportInput
  }

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
    | InputExpected String

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartEdit date -> (startReportInput model date, Cmd.none)
    CancelEdit -> (cancelReportInput model, Cmd.none)
    SaveEdit -> (saveReportInput model, Cmd.none)
    InputStart start -> (saveModelStart start model, Cmd.none)
    InputStop stop -> (saveModelStop stop model, Cmd.none)
    InputExpected expected -> (saveModelExpected expected model, Cmd.none)

editCell editingOtherDay day =
  if editingOtherDay then
    []
  else
    [Html.button [Events.onClick (StartEdit day)] [Html.text "Edit"]]

viewEmptyDay : Bool -> Date -> Html Msg
viewEmptyDay editingOtherDay day =
  Html.tr []
    [ Html.td [] [Html.text <| Date.toIsoString day]
    , Html.td [] []
    , Html.td [] []
    , Html.td [] []
    , Html.td [] []
    , Html.td [] []
    , Html.td [] (editCell editingOtherDay day)
    ]

viewDay : Bool -> Date -> Report -> Html Msg
viewDay editingOtherDay day report =
  Html.tr []
    [ Html.td [] [Html.text <| Date.toIsoString day]
    , Html.td [] []
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| report.start]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| Report.getEnd report]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| report.expected]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| Report.getWorkedMinutes report]
    , Html.td [] (editCell editingOtherDay day)
    ]

viewOkButton input =
  let disabledIfErr =
        Report.inputErrors input |> List.isEmpty |> not
      tooltip = Report.inputErrors input |> String.join ", "
  in
    Html.button [Events.onClick SaveEdit, Att.disabled disabledIfErr, Att.title tooltip] [Html.text "Ok"]

viewCancelButton = Html.button [Events.onClick CancelEdit] [Html.text "Cancel"]

viewTimeInputField event value =
  Html.input
    [ Events.onInput event
    , Att.value value
    , Att.size 5
    , Att.maxlength 5
    ]
    []

editDay : ReportInput -> Html Msg
editDay input =
  let workedTime = if inputErrors input |> List.isEmpty then parseReportInput input |> getWorkedMinutes |> Report.showAsHoursAndMinutes else ""
  in
    Html.tr []
      [ Html.td [] [Html.text <| Date.toIsoString input.date]
      , Html.td [] []
      , Html.td [] [viewTimeInputField InputStart input.start]
      , Html.td [] [viewTimeInputField InputStop input.stop]
      , Html.td [] [viewTimeInputField InputExpected input.expected]
      , Html.td [] [workedTime |> Html.text]
      , Html.td [] [viewOkButton input, viewCancelButton]
      ]

viewOrEditDay : Model -> Date -> Html Msg
viewOrEditDay model day =
  let report = getReport model.reports day
      viewDayOrEmpty editing =
        report
        |> Maybe.map (viewDay editing day)
        |> Maybe.withDefault (viewEmptyDay editing day)
  in
    case model.editing of
      Nothing -> viewDayOrEmpty False
      Just input -> if day == input.date then editDay input else viewDayOrEmpty True

header caption = Html.th [] [Html.text caption]

reportHeaders =
  List.map header
    [ "Date"
    , "Day"
    , "Start"
    , "Stop"
    , "Expected"
    , "Worked"
    , "Commands"
    ]


view : Model -> Browser.Document Msg
view model =
  Browser.Document
    "Sello"
    [ Html.table []
      (reportHeaders
      ++ (List.map (viewOrEditDay model) dates)
      )
    ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


initReports =
  Dict.empty


init : () -> (Model, Cmd Msg)
init _ = (Model initReports Nothing, Cmd.none)


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
