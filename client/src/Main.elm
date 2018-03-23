import Html exposing (Html)
import Html.Attributes as Att
import Html.Events as Events

import TimeSheet exposing (..)
import Time.Date as Date exposing (Date, date)
import Dict exposing (Dict)


type alias ReportInput =
  { date: String
  , start: String
  , stop: String
  }

emptyInput = ReportInput "" "" ""

type alias Model =
  { reports: List Report
  , startDay: Date
  , adding: Maybe ReportInput
  }

type Msg
    = StartAdd
    | CancelAdd
    | ConfirmAdd

startReportInput model =
  { model | adding = Just emptyInput }

cancelReportInput model = 
  { model | adding = Nothing}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartAdd -> (startReportInput model, Cmd.none)
    CancelAdd -> (cancelReportInput model, Cmd.none)
    ConfirmAdd -> (cancelReportInput model, Cmd.none)

-- Use a real implementation...
viewDate : Date -> String
viewDate date =
  [Date.year date, Date.month date, Date.day date]
  |> List.map toString
  |> List.intersperse "-"
  |> String.concat

viewReport : Report -> Html Msg
viewReport report = Html.tr []
  [ Html.td [] [Html.text <| viewDate <| report.date]
  , Html.td [] [Html.text <| toString <| Date.weekday report.date]
  , Html.td [] []
  , Html.td [] []
  , Html.td [] []
  ]


reportHeaders =
  [ Html.th [] [Html.text "Date"]
  , Html.th [] [Html.text "Day"]
  , Html.th [] [Html.text "Start"]
  , Html.th [] [Html.text "Stop"]
  , Html.th [] [Html.text "Diff"]
  ]

viewReportInput input = Html.tr []
  [ Html.td [] [Html.input [] []]
  , Html.td [] []
  , Html.td [] [Html.input [] []]
  , Html.td [] [Html.input [] []]
  , Html.td [] [viewOkButton, viewCancelButton]
  ]

viewAddButton = Html.button [Events.onClick StartAdd] [Html.text "Add"]

viewOkButton = Html.button [Events.onClick ConfirmAdd] [Html.text "Ok"]
viewCancelButton = Html.button [Events.onClick CancelAdd] [Html.text "Cancel"]

viewAddOrInput maybeInput =
  case maybeInput of
    Nothing -> viewAddButton
    Just input -> viewReportInput input

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.table []
      (reportHeaders
      ++ (List.map viewReport model.reports)
      ++ [viewAddOrInput model.adding]
      )
    ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


initReports =
  [Report (date 2018 03 24) (TimeOfDay 8 0)]


init : (Model, Cmd Msg)
init = (Model initReports (date 2017 03 01) Nothing, Cmd.none)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
