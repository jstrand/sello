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
  { date: String
  , start: String
  , stop: String
  }

saveDate input date =
  { input | date = date }

saveStart input start =
  { input | start = start }

saveStop input stop =
  { input | stop = stop }

emptyInput = ReportInput "" "" ""

inputToReport : ReportInput -> Report
inputToReport reportInput =
  Report.parseReport reportInput.start reportInput.stop

dates : List Date
dates =
  let dayInInterval = Date.fromCalendarDate 2018 Time.Oct 12
      start = Date.floor Date.Year dayInInterval
      stop = Date.ceiling Date.Year dayInInterval
  in
    Date.range Date.Day 1 start stop


type alias Model =
  { reports: List Report
  , adding: Maybe ReportInput
  }

type Msg
    = StartAdd
    | CancelAdd
    | ConfirmAdd
    | InputDate String
    | InputStart String
    | InputStop String

startReportInput model =
  { model | adding = Just emptyInput }

cancelReportInput model = 
  { model | adding = Nothing }

saveReportInput model =
  case model.adding of
    Nothing -> model
    Just input ->
      { model
      | adding = Nothing
      , reports = model.reports ++ [inputToReport input]
      }

saveModelDate date model =
  case model.adding of
    Nothing -> model
    Just input ->
      { model
      | adding = Just <| saveDate input date}

saveModelStart start model =
  case model.adding of
    Nothing -> model
    Just input ->
      { model
      | adding = Just <| saveStart input start}

saveModelStop stop model =
  case model.adding of
    Nothing -> model
    Just input ->
      { model
      | adding = Just <| saveStop input stop}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartAdd -> (startReportInput model, Cmd.none)
    CancelAdd -> (cancelReportInput model, Cmd.none)
    ConfirmAdd -> (saveReportInput model, Cmd.none)
    InputDate date -> (saveModelDate date model, Cmd.none)
    InputStart start -> (saveModelStart start model, Cmd.none)
    InputStop stop -> (saveModelStop stop model, Cmd.none)


viewDay : Date -> Html Msg
viewDay day = Html.tr []
   [ Html.td [] [Html.text <| Date.toIsoString day]
   , Html.td [] []
   , Html.td [] []
   , Html.td [] [Html.button [] [Html.text "Edit"]]
   ]

reportHeaders =
  [ Html.th [] [Html.text "Date"]
  , Html.th [] [Html.text "Day"]
  , Html.th [] [Html.text "Start"]
  , Html.th [] [Html.text "Stop"]
  , Html.th [] [Html.text "Diff"]
  ]

viewReportInput input = Html.tr []
  [ Html.td [] [Html.input [Events.onInput InputDate] []]
  , Html.td [] []
  , Html.td [] [Html.input [Events.onInput InputStart] []]
  , Html.td [] [Html.input [Events.onInput InputStop] []]
  , Html.td [] [viewOkButton, viewCancelButton]
  ]

viewAddButton = Html.button [Events.onClick StartAdd] [Html.text "Add"]

viewOkButton = Html.button [Events.onClick ConfirmAdd] [Html.text "Ok"]
viewCancelButton = Html.button [Events.onClick CancelAdd] [Html.text "Cancel"]

viewAddOrInput maybeInput =
  case maybeInput of
    Nothing -> viewAddButton
    Just input -> viewReportInput input

view : Model -> Browser.Document Msg
view model =
  Browser.Document
    "Sello"
    [ Html.table []
      (reportHeaders
      ++ (List.map viewDay dates)
      ++ [viewAddOrInput model.adding]
      )
    ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


initReports =
  []


init : () -> (Model, Cmd Msg)
init _ = (Model initReports Nothing, Cmd.none)


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
