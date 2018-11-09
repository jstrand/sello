module Storage exposing
    ( loadPreferences
    , loadReports
    , savePreferences
    , saveReports
    )

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Preferences exposing (Preferences)
import Report exposing (Reports)


authHeader : String -> Http.Header
authHeader token =
    Http.header "Authorization" token


load : String -> String -> Decode.Decoder result -> (Result Http.Error result -> msg) -> Cmd msg
load url token decoder message =
    Http.request
        { method = "GET"
        , url = url
        , headers = [ authHeader token ]
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send message


save : String -> String -> payload -> (payload -> Encode.Value) -> (Result Http.Error () -> msg) -> Cmd msg
save url token payload encode message =
    Http.request
        { method = "POST"
        , url = url
        , headers = [ authHeader token ]
        , body = Http.jsonBody (encode payload)
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send message


reportsUrl : String -> String
reportsUrl url =
    url ++ "/reports"


saveReports : String -> String -> Reports -> (Result Http.Error () -> msg) -> Cmd msg
saveReports url token reports message =
    save (reportsUrl url) token reports Report.encodeReports message


loadReports : String -> String -> (Result Http.Error Reports -> msg) -> Cmd msg
loadReports url token message =
    load (reportsUrl url) token Report.decodeReports message


preferencesUrl : String -> String
preferencesUrl url =
    url ++ "/preferences"


savePreferences : String -> String -> Preferences -> (Result Http.Error () -> msg) -> Cmd msg
savePreferences url token preferences message =
    save (preferencesUrl url) token preferences Preferences.encode message


loadPreferences : String -> String -> (Result Http.Error Preferences -> msg) -> Cmd msg
loadPreferences url token message =
    load (preferencesUrl url) token Preferences.decode message
