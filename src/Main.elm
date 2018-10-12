import Html exposing (Html)
import Html.Attributes as Att
import Html.Events as Events
import Browser
import Time
import DateFormat
import Dict exposing (Dict)
import Date exposing (Date)

import Report exposing (..)

type alias ReportInput =
  { date: Date
  , start: String
  , stop: String
  }

saveDate input date =
  { input | date = date }

saveStart input start =
  { input | start = start }

saveStop input stop =
  { input | stop = stop }


inputToReport : ReportInput -> Report
inputToReport reportInput =
  Report.parseReport reportInput.start reportInput.stop

reportToInput : Date -> Report -> ReportInput
reportToInput date report =
  ReportInput date (showAsHoursAndMinutes report.start) (showAsHoursAndMinutes (Report.getEnd report))

defaultInput : Date -> ReportInput
defaultInput date =
  ReportInput date "08:00" "17:00"

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
  Dict.insert (Date.toRataDie reportInput.date) (inputToReport reportInput) reports

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartEdit date -> (startReportInput model date, Cmd.none)
    CancelEdit -> (cancelReportInput model, Cmd.none)
    SaveEdit -> (saveReportInput model, Cmd.none)
    InputStart start -> (saveModelStart start model, Cmd.none)
    InputStop stop -> (saveModelStop stop model, Cmd.none)

viewEmptyDay : Date -> Html Msg
viewEmptyDay day =
  Html.tr []
    [ Html.td [] [Html.text <| Date.toIsoString day]
    , Html.td [] []
    , Html.td [] []
    , Html.td [] [Html.button [Events.onClick (StartEdit day)] [Html.text "Edit"]]
    ]

viewDay : Date -> Report -> Html Msg
viewDay day report =
  Html.tr []
    [ Html.td [] [Html.text <| Date.toIsoString day]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| report.start]
    , Html.td [] [Html.text <| Report.showAsHoursAndMinutes <| Report.getEnd report]
    , Html.td [] [Html.button [Events.onClick (StartEdit day)] [Html.text "Edit"]]
    ]

viewOkButton = Html.button [Events.onClick SaveEdit] [Html.text "Ok"]
viewCancelButton = Html.button [Events.onClick CancelEdit] [Html.text "Cancel"]

editDay : ReportInput -> Html Msg
editDay input =
  Html.tr []
    [ Html.td [] [Html.text <| Date.toIsoString input.date]
    , Html.td [] [Html.input [Events.onInput InputStart] [Html.text <| input.start]]
    , Html.td [] [Html.input [Events.onInput InputStop] [Html.text <| input.stop]]
    , Html.td [] [viewOkButton, viewCancelButton]
    ]

viewOrEditDay : Model -> Date -> Html Msg
viewOrEditDay model day =
  let report = getReport model.reports day
      viewDayOrEmpty =
        report
        |> Maybe.map (viewDay day)
        |> Maybe.withDefault (viewEmptyDay day)
  in
    case model.editing of
      Nothing -> viewDayOrEmpty
      Just input -> if day == input.date then editDay input else viewDayOrEmpty

reportHeaders =
  [ Html.th [] [Html.text "Date"]
  , Html.th [] [Html.text "Day"]
  , Html.th [] [Html.text "Start"]
  , Html.th [] [Html.text "Stop"]
  , Html.th [] [Html.text "Diff"]
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
