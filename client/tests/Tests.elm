module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

import TimeSheetTests


all : Test
all =
  describe "Test Suite"
    [
      describe "Time Sheet" TimeSheetTests.all
    ]
