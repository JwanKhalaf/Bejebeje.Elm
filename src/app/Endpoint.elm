module Endpoint exposing (Endpoint, request, searchArtists)

import Http exposing (Body, Expect, Header, request)
import Url.Builder exposing (QueryParameter, string)


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


request :
    { method : String
    , headers : List Header
    , url : Endpoint
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
request config =
    Http.request
        { method = config.method
        , headers = config.headers
        , url = unwrap config.url
        , body = config.body
        , expect = config.expect
        , timeout = config.timeout
        , tracker = config.tracker
        }


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin "http://localhost:5010"
        paths
        queryParams
        |> Endpoint



-- endpoints


searchArtists : String -> Endpoint
searchArtists searchTerm =
    url [ "artists" ] [ string "name" searchTerm ]
