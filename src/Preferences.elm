module Preferences exposing
    ( Preferences
    , allDays
    , default
    , isDaySelected
    , shouldShowDate
    , toggleDay
    , weekdayName
    )

import Date exposing (Date)
import Time


type alias WeekdaySelection =
    { mon : Bool
    , tue : Bool
    , wed : Bool
    , thu : Bool
    , fri : Bool
    , sat : Bool
    , sun : Bool
    }


isSelected : Time.Weekday -> WeekdaySelection -> Bool
isSelected weekday selection =
    case weekday of
        Time.Mon ->
            selection.mon

        Time.Tue ->
            selection.tue

        Time.Wed ->
            selection.wed

        Time.Thu ->
            selection.thu

        Time.Fri ->
            selection.fri

        Time.Sat ->
            selection.sat

        Time.Sun ->
            selection.sun


toggleSelected : Time.Weekday -> WeekdaySelection -> WeekdaySelection
toggleSelected weekday selection =
    case weekday of
        Time.Mon ->
            { selection | mon = not selection.mon }

        Time.Tue ->
            { selection | tue = not selection.tue }

        Time.Wed ->
            { selection | wed = not selection.wed }

        Time.Thu ->
            { selection | thu = not selection.thu }

        Time.Fri ->
            { selection | fri = not selection.fri }

        Time.Sat ->
            { selection | sat = not selection.sat }

        Time.Sun ->
            { selection | sun = not selection.sun }


type alias Preferences =
    { weekdays : WeekdaySelection
    }


default : Preferences
default =
    Preferences
        { mon = True
        , tue = True
        , wed = True
        , thu = True
        , fri = True
        , sat = False
        , sun = False
        }


shouldShowDate : Preferences -> Date -> Bool
shouldShowDate preferences date =
    isSelected (Date.weekday date) preferences.weekdays


isDaySelected : Time.Weekday -> Preferences -> Bool
isDaySelected day preferences =
    isSelected day preferences.weekdays


toggleDay : Time.Weekday -> Preferences -> Preferences
toggleDay day preferences =
    { preferences | weekdays = toggleSelected day preferences.weekdays }


allDays =
    [ Time.Mon
    , Time.Tue
    , Time.Wed
    , Time.Thu
    , Time.Fri
    , Time.Sat
    , Time.Sun
    ]


weekdayName weekday =
    case weekday of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"
