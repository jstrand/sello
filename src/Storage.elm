module Storage exposing
    ( loadPreferences
    , loadReports
    , savePreferences
    , saveReports
    )

import Http
import Preferences exposing (Preferences)
import Report exposing (Reports)


authHeader : String -> Http.Header
authHeader token =
    Http.header "Authorization" token


saveReports : String -> String -> Reports -> (Result Http.Error () -> msg) -> Cmd msg
saveReports url token reports message =
    Http.request
        { method = "POST"
        , url = url ++ "/reports"
        , headers = [ authHeader token ]
        , body = Http.jsonBody (Report.encodeReports reports)
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send message


loadReports : String -> String -> (Result Http.Error Reports -> msg) -> Cmd msg
loadReports url token message =
    Http.request
        { method = "GET"
        , url = url ++ "/reports"
        , headers = [ authHeader token ]
        , body = Http.emptyBody
        , expect = Http.expectJson Report.decodeReports
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send message


savePreferences : String -> String -> Preferences -> (Result Http.Error () -> msg) -> Cmd msg
savePreferences url token preferences message =
    Http.request
        { method = "POST"
        , url = url ++ "/preferences"
        , headers = [ authHeader token ]
        , body = Http.jsonBody (Preferences.encode preferences)
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send message


loadPreferences : String -> String -> (Result Http.Error Preferences -> msg) -> Cmd msg
loadPreferences url token message =
    Http.request
        { method = "GET"
        , url = url ++ "/preferences"
        , headers = [ authHeader token ]
        , body = Http.emptyBody
        , expect = Http.expectJson Preferences.decode
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send message
