import Html exposing (Html)
import Html.Attributes as Att

import TimeSheet exposing (..)
import Time.Date exposing (..)
import Dict exposing (Dict)


type alias Model =
  { reports: Dict Date Report
  , startDay: Date
  }


type Msg =
    None

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None -> (model, Cmd.none)


viewReport : (Date, Report) -> Html Msg
viewReport (day, report) = Html.tr []
  [ Html.td [] [Html.text <| toString <| weekday day]
  , Html.td [] []
  , Html.td [] []
  , Html.td [] []
  ]


reportHeaders =
  [ Html.th [] [Html.text "Dag"]
  , Html.th [] [Html.text "Start"]
  , Html.th [] [Html.text "Stop"]
  , Html.th [] [Html.text "Minus"]
  ]

view : Model -> Html Msg
view model =
  Html.div [] [
    Html.table []
      (reportHeaders ++ (List.map viewReport model.reports))
  ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


initReports =
  Dict.empty


init : (Model, Cmd Msg)
init = (Model initReports (date 2017 03 01), Cmd.none)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
