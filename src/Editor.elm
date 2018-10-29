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

import Report exposing (..)
import Storage exposing (saveReports, loadReports)

type alias Model =
  { reports: ReportDict
  , editing: String
  , today: Date
  , url: String
  , token: String
  }


type Msg
    = LoadResult (Result Http.Error ReportDict)
    -- | SaveEdit
    | SaveResult (Result Http.Error ())
    | SetToday Date

viewEditor = [Html.p [] [Html.text "Editor"]]

view : Model -> Browser.Document Msg
view model =
    Browser.Document "Sello-editor" viewEditor

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetToday date -> ({ model | today = date}, Cmd.none)
    LoadResult (Err _) -> ({ model | editing = "Failed to load"}, Cmd.none)
    LoadResult (Ok reports) -> ({ model | reports = reports }, Cmd.none)
    SaveResult _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

getTodaysDate = Date.today |> Task.perform SetToday

fakeDate = Date.fromCalendarDate 2000 Time.Jan 1

init : (String, String) -> (Model, Cmd Msg)
init (url, token) =
  ( Model
    Report.noReports
    ""
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
