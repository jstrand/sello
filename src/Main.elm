module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes as Att
import Html.Events as Events
import Http
import List.Extra exposing (scanl, zip)
import Preferences exposing (Preferences)
import Report exposing (Minutes, Report, Reports)
import Storage
import Task
import Time


type AppStatus
    = Loading
    | Saving
    | Ready
    | Failure String


type alias Model =
    { reports : Reports
    , editing : Maybe Report.Input
    , status : AppStatus
    , showDate : Date
    , showInterval : Date.Unit
    , preferences : Preferences
    , today : Date
    , url : String
    , token : String
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
    | LoadReports (Result Http.Error Reports)
    | LoadPreferences (Result Http.Error Preferences)
    | SetInterval Date.Unit
    | PreviousInterval
    | NextInterval
    | SetToday Date
    | ToggleWeekday Time.Weekday
    | GotoToday
    | NoOp


startInput model date =
    { model | editing = Just (Report.getInput model.reports date) }


cancelInput model =
    { model | editing = Nothing }


saveInput : Model -> Model
saveInput model =
    case model.editing of
        Nothing ->
            model

        Just input ->
            { model
                | editing = Nothing
                , reports = Report.saveInput input model.reports
            }


saveField updateF model =
    case model.editing of
        Nothing ->
            model

        Just input ->
            { model
                | editing = Just <| updateF input
            }


describeError : Http.Error -> String
describeError error =
    case error of
        Http.BadUrl url ->
            "Cannot read from " ++ url

        Http.Timeout ->
            "Something took too long!"

        Http.NetworkError ->
            "Somethings up with the internet!"

        Http.BadStatus response ->
            "Somethings up with the server!"

        Http.BadPayload message response ->
            "Somethings really up with the server! " ++ message


failWith error model =
    ( { model | status = Failure error }, Cmd.none )


moveInterval : Int -> Model -> Model
moveInterval offset model =
    let
        newDate =
            Date.add model.showInterval offset model.showDate
    in
    { model | showDate = newDate }


previousInterval =
    moveInterval -1


nextInterval =
    moveInterval 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartEdit date ->
            ( startInput model date, Task.attempt (\_ -> NoOp) (Dom.focus "start") )

        CancelEdit ->
            ( cancelInput model, Cmd.none )

        SaveEdit ->
            let
                newModel =
                    saveInput model
            in
            ( { newModel | status = Saving }, Storage.saveReports newModel.url newModel.token newModel.reports SaveResult )

        InputStart start ->
            ( saveField (Report.saveStart start) model, Cmd.none )

        InputStop stop ->
            ( saveField (Report.saveStop stop) model, Cmd.none )

        InputPause pause ->
            ( saveField (Report.savePause pause) model, Cmd.none )

        InputExpected expected ->
            ( saveField (Report.saveExpected expected) model, Cmd.none )

        SaveResult (Ok _) ->
            ( { model | status = Ready }, Cmd.none )

        SaveResult (Err error) ->
            failWith (describeError error) model

        LoadReports (Ok reports) ->
            ( { model | reports = reports, status = Ready }, Cmd.none )

        LoadReports (Err (Http.BadStatus description)) ->
            if description.status.code == 404 then
                ( model, Storage.saveReports model.url model.token model.reports SaveResult )

            else
                failWith description.status.message model

        LoadReports (Err error) ->
            failWith (describeError error) model

        LoadPreferences (Ok preferences) ->
            ( { model | preferences = preferences }, Cmd.none )

        -- Ingore errors for preferences, it's non-essential
        LoadPreferences (Err error) ->
            ( model, Cmd.none )

        SetInterval interval ->
            ( { model | showInterval = interval }, Cmd.none )

        PreviousInterval ->
            ( previousInterval model, Cmd.none )

        NextInterval ->
            ( nextInterval model, Cmd.none )

        SetToday date ->
            ( { model | showDate = date, today = date }, Cmd.none )

        GotoToday ->
            ( model, getTodaysDate )

        ToggleWeekday day ->
            let
                newModel =
                    toggleWeekday day model
            in
            ( newModel, Storage.savePreferences model.url model.token newModel.preferences SaveResult )


toggleWeekday : Time.Weekday -> Model -> Model
toggleWeekday day model =
    { model | preferences = Preferences.toggleDay day model.preferences }


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


cell =
    Html.td []


emptyCell =
    cell []


cellWithOne one =
    cell [ one ]


textCell value =
    Html.td [] [ Html.text <| value ]


timeCell : Minutes -> Html Msg
timeCell =
    textCell << Report.showAsHoursAndMinutes


timeCellOrEmpty : Maybe Minutes -> Html Msg
timeCellOrEmpty value =
    Maybe.withDefault emptyCell (Maybe.map timeCell value)


makeBold v =
    Html.b [] [ v ]


viewDiff : Report -> Html Msg
viewDiff report =
    case Report.getDiffResult report of
        Ok maybeDiff ->
            timeCellOrEmpty maybeDiff

        Err warning ->
            Html.td [ Att.title warning ] [ warningIcon ]


viewDay : Bool -> Bool -> Date -> String -> Report -> Html Msg
viewDay today readOnly day total report =
    let
        dateFormat =
            if today then
                makeBold

            else
                identity
    in
    Html.tr []
        [ Html.td [] [ dateFormat (Html.text <| Date.toIsoString day) ]
        , textCell <| Date.format "EE" day
        , timeCellOrEmpty <| Report.getStart report
        , timeCellOrEmpty <| Report.getPause report
        , timeCellOrEmpty <| Report.getEnd report
        , timeCellOrEmpty <| Report.getExpected report
        , timeCellOrEmpty <| Report.getWorkedMinutes report
        , viewDiff report
        , textCell total
        , editCell readOnly day
        ]


icon class =
    Html.i [ Att.class class ] []


warningIcon =
    icon "fas fa-exclamation-triangle fa-lg"


viewOkButton input =
    let
        disabledIfErr =
            Report.inputErrors input |> List.isEmpty |> not

        tooltip =
            Report.inputErrors input |> String.join ", "
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


editDay : Report.Input -> String -> Html Msg
editDay input total =
    let
        inputOk =
            Report.inputErrors input |> List.isEmpty

        potentialReport =
            Report.parseInput input

        workedTime =
            potentialReport |> Report.getWorkedMinutes
    in
    Html.tr []
        [ textCell <| Date.toIsoString input.date
        , textCell <| Date.format "EE" input.date
        , cell [ viewTimeInputField InputStart input.start "start" ]
        , cell [ viewTimeInputField InputPause input.pause "pause" ]
        , cell [ viewTimeInputField InputStop input.stop "stop" ]
        , cell [ viewTimeInputField InputExpected input.expected "expected" ]
        , timeCellOrEmpty workedTime
        , viewDiff potentialReport
        , textCell total
        , cell [ viewOkButton input, viewCancelButton ]
        ]


viewOrEditDay : Model -> ( Date, Int ) -> Html Msg
viewOrEditDay model ( day, total ) =
    let
        report =
            Report.getReportOrEmpty model.reports day

        isToday =
            model.today == day

        totalStr =
            Report.showAsHoursAndMinutes total

        viewDayOrEmpty readOnly =
            viewDay isToday readOnly day totalStr report
    in
    case model.editing of
        Nothing ->
            viewDayOrEmpty (model.status == Saving)

        Just input ->
            let
                reportsWithEdit =
                    Report.saveInput input model.reports
            in
            if day == input.date then
                editDay input totalStr

            else
                viewDay isToday True day totalStr report


timeColumnHeader caption =
    Html.th
        [ Att.style "width" "10%"
        ]
        [ Html.text caption ]


headerWithWidth caption width =
    Html.th
        [ Att.style "width" width ]
        [ Html.text caption ]


reportHeaders =
    [ headerWithWidth "Date" "10em"
    ]
        ++ List.map timeColumnHeader
            [ "Day"
            , "Start"
            , "Pause"
            , "Stop"
            , "Expected"
            , "Worked"
            , "Diff"
            , "Total"
            ]
        ++ [ Html.th [] [] ]


fixedWidthLabel width caption =
    Html.span
        [ Att.style "width" width
        , Att.style "display" "inline-block"
        , Att.style "text-align" "center"
        ]
        [ Html.text caption ]


viewInterval model =
    let
        format =
            case model.showInterval of
                Date.Years ->
                    "y"

                Date.Months ->
                    "MMMM, y"

                Date.Weeks ->
                    "'Week' w, y"

                Date.Days ->
                    "EEEE, d MMMM y"
    in
    Date.format format model.showDate
        |> (\caption ->
                Html.button
                    [ Att.class "btn btn-dark"
                    , Att.title "Show interval with todays date"
                    , Att.style "width" "10em"
                    , Events.onClick GotoToday
                    ]
                    [ Html.text caption ]
           )


intervalButtons activeInterval =
    Html.div
        [ Att.class "btn-group"
        ]
        [ intervalButton Date.Years (activeInterval == Date.Years) "Year"
        , intervalButton Date.Months (activeInterval == Date.Months) "Month"
        , intervalButton Date.Weeks (activeInterval == Date.Weeks) "Week"
        ]


intervalButton : Date.Unit -> Bool -> String -> Html Msg
intervalButton interval active caption =
    Html.button
        [ Events.onClick <| SetInterval interval
        , Att.class "btn btn-dark"
        , Att.disabled active
        ]
        [ Html.text caption ]


previousButton =
    Html.button
        [ Events.onClick PreviousInterval, Att.class "btn btn-dark" ]
        [ Html.text "<<" ]


nextButton =
    Html.button
        [ Events.onClick NextInterval, Att.class "btn btn-dark" ]
        [ Html.text ">>" ]


moveButtons model =
    Html.div
        [ Att.class "btn-group"
        ]
        [ previousButton
        , viewInterval model
        , nextButton
        ]


separator =
    Html.text " "


weekdayButton preferences weekday =
    let
        class =
            if Preferences.isDaySelected weekday preferences then
                "btn-dark"

            else
                "btn-outline-dark"
    in
    Html.button
        [ Att.class "btn"
        , Att.class class
        , Events.onClick (ToggleWeekday weekday)
        ]
        [ Html.text (Preferences.weekdayName weekday) ]


weekdayButtons : Preferences -> Html Msg
weekdayButtons preferences =
    Html.div
        [ Att.class "btn-group" ]
        (List.map (weekdayButton preferences) Preferences.allDays)


viewNavigation : Model -> Html Msg
viewNavigation model =
    Html.div [ Att.class "toolbar" ]
        [ moveButtons model
        , separator
        , intervalButtons model.showInterval
        , separator
        , weekdayButtons model.preferences
        ]


dateUnitToInterval : Date.Unit -> Date.Interval
dateUnitToInterval unit =
    case unit of
        Date.Years ->
            Date.Year

        Date.Months ->
            Date.Month

        Date.Weeks ->
            Date.Week

        Date.Days ->
            Date.Day


datesInInterval : Model -> List Date
datesInInterval model =
    let
        dayInInterval =
            model.showDate

        interval =
            dateUnitToInterval model.showInterval

        start =
            Date.floor interval dayInInterval

        -- add one day to stop, because Date.ceiling and Date.floor returns
        -- the same date if it's the first day in the interval
        stop =
            Date.add Date.Days 1 dayInInterval |> Date.ceiling interval
    in
    Date.range Date.Day 1 start stop


viewReports : Model -> List (Html Msg)
viewReports model =
    let
        dates =
            datesInInterval model

        previousDay =
            Date.add Date.Days -1

        dateBeforeInterval =
            Maybe.map previousDay (List.head dates)

        totalBefore =
            Maybe.map (Report.runningTotal model.reports) dateBeforeInterval |> Maybe.withDefault 0

        getDiff date =
            Report.getReportOrEmpty model.reports date
                |> Report.getDiff
                |> Maybe.withDefault 0

        diffs =
            List.map getDiff dates

        accumulatedDiffs =
            scanl (+) totalBefore diffs |> List.tail |> Maybe.withDefault []

        datesWithSum =
            zip dates accumulatedDiffs

        shouldShowDate ( date, _ ) =
            Preferences.shouldShowDate model.preferences date

        filteredDays =
            List.filter shouldShowDate datesWithSum
    in
    [ viewNavigation model
    , Html.table
        [ Att.class "table"
        , Att.style "max-width" "1200px"
        ]
        (reportHeaders ++ List.map (viewOrEditDay model) filteredDays)
    ]


viewBusy =
    [ Html.text "Please wait..." ]


viewError error =
    [ Html.text error ]


view : Model -> Browser.Document Msg
view model =
    let
        currentContents =
            case model.status of
                Loading ->
                    viewBusy

                Saving ->
                    viewReports model

                Ready ->
                    viewReports model

                Failure error ->
                    viewError error
    in
    Browser.Document "Sello" currentContents


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getTodaysDate =
    Date.today |> Task.perform SetToday


fakeDate =
    Date.fromCalendarDate 2000 Time.Jan 1


init : ( String, String ) -> ( Model, Cmd Msg )
init ( url, token ) =
    ( Model
        Report.noReports
        Nothing
        Loading
        fakeDate
        Date.Weeks
        Preferences.default
        fakeDate
        url
        token
    , Cmd.batch
        [ Storage.loadReports url token LoadReports
        , Storage.loadPreferences url token LoadPreferences
        , getTodaysDate
        ]
    )


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
