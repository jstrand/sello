module Storage exposing
    ( loadReports
    , saveReports
    )

import Http
import Report exposing (Reports)


authHeader : String -> Http.Header
authHeader token =
    Http.header "Authorization" token


saveReports : String -> String -> Reports -> (Result Http.Error () -> msg) -> Cmd msg
saveReports url token reports message =
    Http.request
        { method = "POST"
        , url = url
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
        , url = url
        , headers = [ authHeader token ]
        , body = Http.emptyBody
        , expect = Http.expectJson Report.decodeReports
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send message
