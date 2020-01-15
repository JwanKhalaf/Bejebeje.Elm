module Endpoint exposing (Endpoint, artistLyricsEndpoint, lyricEndpoint, request, searchArtistsEndpoint)

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


url : String -> List String -> List QueryParameter -> Endpoint
url root paths queryParams =
    Url.Builder.crossOrigin (String.dropRight 1 root)
        paths
        queryParams
        |> Endpoint



-- endpoints


searchArtistsEndpoint : String -> String -> Endpoint
searchArtistsEndpoint root searchTerm =
    url root [ "artists" ] [ string "name" searchTerm ]


artistLyricsEndpoint : String -> String -> Endpoint
artistLyricsEndpoint root artistSlug =
    url root [ "artists", artistSlug, "lyrics" ] []


lyricEndpoint : String -> String -> String -> Endpoint
lyricEndpoint root artistSlug lyricSlug =
    url root [ "artists", artistSlug, "lyrics", lyricSlug ] []
