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
import Task
import Browser.Dom as Dom

import Report exposing (..)
import Storage exposing (saveReports, loadReports)

type AppStatus = Loading | Saving | Ready | Failure String


type alias Model =
  { reports: ReportDict
  , editing: Maybe ReportInput
  , status: AppStatus
  , showDate: Date
  , showInterval: Date.Unit
  , today: Date
  , url: String
  , token: String
  }


type Msg
    = StartEdit Date
    | CancelEdit
    | SaveEdit
    | InputStart String
    | InputStop String
    | InputPause String
    | InputExpected String
    | SaveResult (Result Http.Error ())
    | LoadResult (Result Http.Error ReportDict)
    | SetInterval Date.Unit
    | PreviousInterval
    | NextInterval
    | SetToday Date
    | GotoToday


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

moveInterval : Int -> Model -> Model
moveInterval offset model =
  let newDate = Date.add model.showInterval offset model.showDate
  in
    { model | showDate = newDate }

previousInterval = moveInterval -1
nextInterval = moveInterval 1


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartEdit date -> (startReportInput model date, Task.attempt (\_ -> GotoToday) (Dom.focus "start"))
    CancelEdit -> (cancelReportInput model, Cmd.none)
    SaveEdit -> 
      let newModel = saveReportInput model
      in ({ newModel | status = Saving }, saveReports newModel.url newModel.token newModel.reports SaveResult)
    InputStart start -> (saveField (saveStart start) model, Cmd.none)
    InputStop stop -> (saveField (saveStop stop) model, Cmd.none)
    InputPause pause -> (saveField (savePause pause) model, Cmd.none)
    InputExpected expected -> (saveField (saveExpected expected) model, Cmd.none)
    SaveResult (Ok _) -> ({ model | status = Ready }, Cmd.none)
    SaveResult (Err error) -> failWith error model
    LoadResult (Ok reports) -> ({ model | reports = reports, status = Ready }, Cmd.none)
    LoadResult (Err (Http.BadStatus description)) ->
      if description.status.code == 404 then
        (model, saveReports model.url model.token model.reports SaveResult)
      else
        failWith description.status.message model
    LoadResult (Err error) -> failWith error model
    SetInterval interval -> ({ model | showInterval = interval }, Cmd.none)
    PreviousInterval -> (previousInterval model, Cmd.none)
    NextInterval -> (nextInterval model, Cmd.none)
    SetToday date -> ({ model | showDate = date, today = date }, Cmd.none)
    GotoToday -> (model, getTodaysDate)


editCell readOnly day =
    Html.td
      []
      [ Html.button
        [ Events.onClick (StartEdit day)
        , Att.class "icon-button"
        , Att.disabled readOnly
        ]
        [ icon "fas fa-edit fa-lg" ]
      ]

cell = Html.td []

emptyCell = cell []

cellWithOne one = cell [one]

textCell value = Html.td [] [Html.text <| value]

timeCell : Minutes -> Html Msg
timeCell = textCell << Report.showAsHoursAndMinutes

timeCellOrEmpty : Maybe Minutes -> Html Msg
timeCellOrEmpty value =
  Maybe.withDefault emptyCell (Maybe.map timeCell value)

makeBold v = Html.b [] [v]

viewDay : Bool -> Bool -> Date -> ReportDict -> Report -> Html Msg
viewDay today readOnly day reports report =
  let dateFormat = if today then makeBold else identity
  in
  Html.tr []
    [ Html.td [] [dateFormat (Html.text <| Date.toIsoString day)]
    , textCell <| Date.format "EE" day
    , timeCellOrEmpty <| Report.getStart report
    , timeCellOrEmpty <| Report.getPause report
    , timeCellOrEmpty <| Report.getEnd report
    , timeCellOrEmpty <| Report.getExpected report
    , timeCellOrEmpty <| Report.getWorkedMinutes report
    , timeCellOrEmpty <| Report.getDiff report
    , timeCell <| runningTotal reports day
    , editCell readOnly day
    ]

icon class =
  Html.i [Att.class class] []

viewOkButton input =
  let disabledIfErr =
        Report.inputErrors input |> List.isEmpty |> not
      tooltip = Report.inputErrors input |> String.join ", "
  in
    Html.button
      [ Events.onClick SaveEdit
      , Att.disabled disabledIfErr
      , Att.title tooltip
      , Att.class "icon-button"
      ]
      [ icon "fas fa-check-circle fa-lg" ]

viewCancelButton =
  Html.button
    [ Events.onClick CancelEdit
    , Att.class "icon-button"
    ]
    [ icon "fas fa-times-circle fa-lg" ]

viewTimeInputField event value id =
  Html.input
    [ Events.onInput event
    , Att.id id
    , Att.value value
    , Att.size 1
    , Att.maxlength 5
    , Att.class "form-control"
    ]
    []

editDay : ReportInput -> ReportDict -> Html Msg
editDay input reports =
  let
    inputOk = inputErrors input |> List.isEmpty
    valueOrEmpty value = if inputOk then value else ""
    potentialReport = parseReportInput input
    workedTime = potentialReport |> getWorkedMinutes
    diff = potentialReport |> getDiff
    potentialReports = saveReport input reports
    totalValue = runningTotal potentialReports input.date |> Report.showAsHoursAndMinutes
  in
    Html.tr []
      [ textCell <| Date.toIsoString input.date
      , textCell <| Date.format "EE" input.date
      , cell [viewTimeInputField InputStart input.start "start"]
      , cell [viewTimeInputField InputPause input.pause "pause"]
      , cell [viewTimeInputField InputStop input.stop "stop"]
      , cell [viewTimeInputField InputExpected input.expected "expected"]
      , timeCellOrEmpty workedTime
      , timeCellOrEmpty diff
      , textCell totalValue
      , cell [viewOkButton input, viewCancelButton]
      ]

viewOrEditDay : Model -> Date -> Html Msg
viewOrEditDay model day =
  let report = getReportOrEmpty model.reports day
      isToday = model.today == day
      viewDayOrEmpty readOnly =
        viewDay isToday readOnly day model.reports report
  in
    case model.editing of
      Nothing -> viewDayOrEmpty (model.status == Saving)
      Just input -> if day == input.date then editDay input model.reports else viewDayOrEmpty True

timeColumnHeader caption =
  Html.th
    [ Att.style "width" "10%"
    ]
    [ Html.text caption ]

headerWithWidth caption width =
  Html.th
    [Att.style "width" width]
    [Html.text caption]

reportHeaders =
  [ headerWithWidth "Date" "10em"
  ] ++
  List.map timeColumnHeader
    [ "Day"
    , "Start"
    , "Pause"
    , "Stop"
    , "Expected"
    , "Worked"
    , "Diff"
    , "Total"
    ] ++ [Html.th [] []]

fixedWidthLabel width caption =
  Html.span
    [ Att.style "width" width
    , Att.style "display" "inline-block"
    , Att.style "text-align" "center"
    ]
    [Html.text caption]

viewInterval model =
  let
    format = case model.showInterval of
      Date.Years -> "y"
      Date.Months -> "MMMM, y"
      Date.Weeks -> "'Week' w, y"
      Date.Days -> "EEEE, d MMMM y"
  in
    Date.format format model.showDate
    |> \caption ->
      Html.button
        [ Att.class "btn btn-dark"
        , Att.title "Show interval with todays date"
        , Att.style "width" "10em"
        , Events.onClick GotoToday
        ]
        [ Html.text caption ]

intervalButtons =
  Html.div
    [ Att.class "btn-group"
    ]
    [ intervalButton Date.Years "Year"
    , intervalButton Date.Months "Month"
    , intervalButton Date.Weeks "Week"
    ]

intervalButton : Date.Unit -> String -> Html Msg
intervalButton interval caption =
  Html.button
    [ Events.onClick <| SetInterval interval
    , Att.class "btn btn-dark"
    ]
    [ Html.text caption ]

previousButton =
  Html.button
    [ Events.onClick PreviousInterval, Att.class "btn btn-dark"]
    [Html.text "<<"]

nextButton =
  Html.button
    [ Events.onClick NextInterval, Att.class "btn btn-dark"]
    [Html.text ">>"]
 
moveButtons model =
  Html.div
    [ Att.class "btn-group"
    ]
    [ previousButton
    , viewInterval model
    , nextButton
    ]

separator = Html.text " "

viewNavigation model =
  Html.div [ Att.class "toolbar" ]
    [ moveButtons model
    , separator
    , intervalButtons
    ]

dateUnitToInterval : Date.Unit -> Date.Interval
dateUnitToInterval unit =
  case unit of
    Date.Years -> Date.Year
    Date.Months -> Date.Month
    Date.Weeks -> Date.Week
    Date.Days -> Date.Day

datesInInterval : Model -> List Date
datesInInterval model =
  let dayInInterval = model.showDate
      interval = dateUnitToInterval model.showInterval
      start = Date.floor interval dayInInterval
      -- add one day to stop, because Date.ceiling and Date.floor returns
      -- the same date if it's the first day in the interval
      stop = Date.add Date.Days 1 dayInInterval |> Date.ceiling interval
  in
    Date.range Date.Day 1 start stop

viewReports : Model -> List (Html Msg)
viewReports model =
  [ viewNavigation model
  , Html.table
    [ Att.class "table"
    , Att.style "max-width" "1200px"
    ]
    ( reportHeaders
    ++ (List.map (viewOrEditDay model) (datesInInterval model))
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

getTodaysDate = Date.today |> Task.perform SetToday

fakeDate = Date.fromCalendarDate 2000 Time.Jan 1

init : (String, String) -> (Model, Cmd Msg)
init (url, token) =
  ( Model
    Dict.empty
    Nothing
    Loading
    fakeDate
    Date.Weeks
    fakeDate
    url
    token
  , Cmd.batch [loadReports url token LoadResult, getTodaysDate]
  )


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
