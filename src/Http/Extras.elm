module Http.Extras exposing (isStatus, is401, errorToString)

{-| Convenience functions for working with elm/http

# Asserting status

@docs isStatus, is401

# Rendering Http.Error

@docs errorToString

-}

import Http exposing (Error(..))
import Json.Decode as Json

{-| Given an expected status code, return True if that's the given Http error.
-}
isStatus : Int -> Http.Error -> Bool
isStatus code error =
    case error of
        Http.BadStatus { status } ->
            status.code == code

        _ ->
            False

{-| Do we have a HTTP 401 response?
-}
is401 : Http.Error -> Bool
is401 =
    isStatus 401

parseError : String -> Maybe String
parseError =
    Json.decodeString (Json.field "error" Json.string) >> Result.toMaybe

{-| Render HTTP error to a string. Useful for showing the user what happened.
-}
errorToString : Http.Error -> String
errorToString err =
    case err of
        Timeout ->
            "Timeout exceeded"

        NetworkError ->
          "Network error"

        BadStatus resp ->
            parseError resp.body
                |> Maybe.withDefault (String.fromInt resp.status.code ++ " " ++ resp.status.message)

        BadPayload text resp ->
            -- OK status, unexpected payload
            "Unexpected response from api: " ++ text

        BadUrl url ->
            "Malformed url: " ++ url
