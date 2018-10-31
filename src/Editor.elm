module Editor exposing (..)

import Html exposing (Html)
import Html.Attributes as Att
import Html.Events as Events
import Browser
import Time
import Date exposing (Date)
import Http
import Task
import Browser.Dom as Dom
import Basics.Extra exposing (uncurry)
import Array

import Report exposing (..)
import Dict
import Storage exposing (saveReports, loadReports)

type alias Model =
  { reports: ReportDict
  , editing: String
  , url: String
  , token: String
  }

type Msg
    = LoadResult (Result Http.Error ReportDict)
    | SaveEdit
    | Edit String
    | SaveResult (Result Http.Error ())

viewEditor value = Html.textarea [Att.cols 180, Att.rows 40, Events.onInput Edit] [Html.text value]

cell = Html.td []

emptyCell = cell []

cellWithOne one = cell [one]

textCell value = Html.td [] [Html.text <| value]

timeCell : Minutes -> Html Msg
timeCell = textCell << Report.showAsHoursAndMinutes

timeCellOrEmpty : Maybe Minutes -> Html Msg
timeCellOrEmpty value =
  Maybe.withDefault emptyCell (Maybe.map timeCell value)

viewReportRow : ReportDict -> (Date, Report) -> Html Msg
viewReportRow reports (day, report) =
  Html.tr []
    [ Html.td [] [Html.text <| Date.toIsoString day]
    , textCell <| Date.format "EE" day
    , timeCellOrEmpty <| Report.getStart report
    , timeCellOrEmpty <| Report.getPause report
    , timeCellOrEmpty <| Report.getEnd report
    , timeCellOrEmpty <| Report.getExpected report
    , timeCellOrEmpty <| Report.getWorkedMinutes report
    , timeCellOrEmpty <| Report.getDiff report
    , timeCell <| runningTotal reports day
    ]
  
viewReportRows : ReportDict -> List (Html Msg)
viewReportRows reports =
  reports
  |> Report.reportsToList
  |> List.map (viewReportRow reports)

viewReportTable : ReportDict -> Html Msg
viewReportTable = Html.table [] << viewReportRows

view : Model -> Browser.Document Msg
view model =
    Browser.Document "Sello Editor"
      [ viewEditor model.editing
      , Html.button [Events.onClick SaveEdit] [Html.text "Apply"]
      , viewReportTable model.reports
      ]

reportInputs : ReportDict -> List ReportInput
reportInputs reports =
  reports
  |> Report.reportsToList
  |> List.map (uncurry Report.reportToInput)

reportToString : ReportInput -> String
reportToString report =
  [Date.format "YYYY-MM-dd" report.date, report.start, report.pause, report.stop, report.expected]
  |> List.intersperse " "
  |> String.concat

reportsToString : ReportDict -> String
reportsToString reports =
  reports
  |> reportInputs
  |> List.map reportToString
  |> List.intersperse "\n"
  |> String.concat

updateReports : ReportDict -> Model -> Model
updateReports newReports model =
  { model | reports = newReports, editing = reportsToString newReports }

listToReportInput : List String -> Maybe ReportInput
listToReportInput inputs =
  case inputs of
    [dateStr, start, pause, stop, expected] ->
      case Date.fromIsoString dateStr of
        Ok date
          -> Just <| ReportInput date start stop pause expected
        Err _ -> Nothing
    _ -> Nothing

stringToReportInput : String -> Maybe ReportInput
stringToReportInput str =
  str
  |> String.split " "
  |> listToReportInput 

stringToReportInputs : String -> List ReportInput
stringToReportInputs edit =
  edit
  |> String.lines
  |> List.filterMap stringToReportInput

parseReports : List ReportInput -> ReportDict
parseReports = List.foldr Report.saveReport Report.noReports

applyEdit : Model -> Model
applyEdit model =
  { model | reports = stringToReportInputs model.editing |> parseReports }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadResult (Err _) -> ({ model | editing = "Failed to load"}, Cmd.none)
    LoadResult (Ok reports) -> (updateReports reports model, Cmd.none)
    SaveResult _ -> (model, Cmd.none)
    Edit value -> ({ model | editing = value }, Cmd.none)
    SaveEdit ->
      let newModel = applyEdit model
      in (newModel, saveReports model.url model.token newModel.reports SaveResult)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- init : (String, String) -> (Model, Cmd Msg)
init : () -> (Model, Cmd Msg)
init _ =
  let url = "http://localhost:8001/reports"
      token = "demo"
  in
  ( Model
    Report.noReports
    ""
    url
    token
  , Cmd.batch [loadReports url token LoadResult]
  )


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
