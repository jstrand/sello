module TimeSheetTests exposing (all)

import Test exposing (..)
import Expect
import Fuzz exposing (..)

import TimeSheet exposing (..)
import Time.Date exposing (date)


testWeekDates _ =
  date 2017 03 01
  |> weekDates
  |> Expect.equal
    [ date 2017 03 01
    , date 2017 03 02
    , date 2017 03 03
    , date 2017 03 04
    , date 2017 03 05
    , date 2017 03 06
    , date 2017 03 07
    ]


testMondayOfWeek _ =
  date 2017 02 09
  |> mondayOfWeek
  |> Expect.equal (date 2017 02 06)


all =
  [ test "Test generating a week" testWeekDates
  , test "Test finding start of a week" testMondayOfWeek
  ]

