module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Endpoint exposing (artistDetailsEndpoint, artistLyricsEndpoint, lyricEndpoint, request, searchArtistsEndpoint, task)
import Html exposing (Html, a, div, footer, h1, header, img, input, main_, p, span, text)
import Html.Attributes exposing (alt, class, href, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, andThen, bool, fail, field, list, map2, map3, string, succeed)
import Task exposing (Task)
import Url exposing (Url, fromString, toString)
import Url.Parser as Parser exposing ((</>), Parser)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- model


type AppState
    = ShowingLyric (WebData Lyric)
    | ShowingArtistLyrics (WebData (List LyricListItem))
    | SearchingArtists (WebData (List Artist))
    | Home


type alias Slug =
    String


type alias LyricSlug =
    String


type alias RootUrl =
    String


type alias Artist =
    { firstName : String
    , lastName : String
    , slug : Slug
    }


type alias LyricListItem =
    { title : String
    , slug : String
    }


type alias Lyric =
    { title : String
    , body : String
    }


type alias ArtistSlug =
    { name : String
    , isPrimary : Bool
    }


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type alias WebData a =
    RemoteData Http.Error a


type alias Flags =
    { apiRootUrl : String
    }


type alias Model =
    { key : Nav.Key
    , url : Url
    , apiRootUrl : Maybe Url
    , searchTerm : String
    , state : AppState
    , activeArtist : Maybe Artist
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        apiRootUrl =
            Url.fromString flags.apiRootUrl

        parsedUrl =
            Maybe.withDefault NotFound (Parser.parse routeParser url)

        command =
            case parsedUrl of
                ArtistRoute artist ->
                    case apiRootUrl of
                        Just rootUrl ->
                            getLyricsForArtist (Url.toString rootUrl) artist

                        Nothing ->
                            Cmd.none

                NotFound ->
                    Cmd.none

        _ =
            Debug.log "parsed Url" parsedUrl

        _ =
            case apiRootUrl of
                Just rootUrl ->
                    Debug.log "apiRootUrl" (Url.toString rootUrl)

                Nothing ->
                    ""
    in
    ( Model key url apiRootUrl "" Home Nothing, command )


type Route
    = ArtistRoute String
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map ArtistRoute (Parser.s "artists" </> Parser.string </> Parser.s "lyrics")
        ]



-- update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | SearchQueryChanged String
    | ArtistsRetrieved (Result Http.Error (List Artist))
    | ArtistClicked Artist
    | LyricsRetrieved (Result Http.Error ( Artist, List LyricListItem ))
    | LyricClicked Artist String
    | LyricRetrieved (Result Http.Error Lyric)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        SearchQueryChanged searchTerm ->
            if String.isEmpty searchTerm then
                ( { model | searchTerm = searchTerm, state = Home }, Cmd.none )

            else
                case model.apiRootUrl of
                    Nothing ->
                        ( model, Cmd.none )

                    Just rootUrl ->
                        ( { model | searchTerm = searchTerm }, searchArtists (toString rootUrl) searchTerm )

        ArtistsRetrieved result ->
            case result of
                Ok artists ->
                    ( { model | state = SearchingArtists (Success artists) }, Cmd.none )

                Err error ->
                    ( { model | state = SearchingArtists (Failure error) }, Cmd.none )

        ArtistClicked artist ->
            case model.apiRootUrl of
                Nothing ->
                    ( model, Cmd.none )

                Just rootUrl ->
                    ( { model | activeArtist = Just artist }, getLyricsForArtist (toString rootUrl) artist.slug )

        LyricsRetrieved result ->
            case result of
                Ok artistLyrics ->
                    ( { model | state = ShowingArtistLyrics (Success artistLyrics) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        LyricClicked artist lyricSlug ->
            case model.apiRootUrl of
                Nothing ->
                    ( model, Cmd.none )

                Just rootUrl ->
                    ( model, getLyric (toString rootUrl) artist lyricSlug )

        LyricRetrieved result ->
            case result of
                Ok lyric ->
                    ( { model | state = ShowingLyric (Success lyric) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Bêjebêje"
    , body =
        [ header []
            [ div
                [ class "logo" ]
                [ h1 [ class "logo__text" ] [ span [ class "logo__letter" ] [ text "B" ], text "êjebêje" ] ]
            ]
        , main_ []
            [ case model.state of
                Home ->
                    showQuote

                SearchingArtists artists ->
                    case model.apiRootUrl of
                        Nothing ->
                            text ""

                        Just rootUrl ->
                            showArtists (toString rootUrl) artists

                ShowingArtistLyrics artistLyrics ->
                    case model.apiRootUrl of
                        Nothing ->
                            text ""

                        Just rootUrl ->
                            case model.activeArtist of
                                Nothing ->
                                    text "because active artist is empty"

                                Just artist ->
                                    showArtistLyricsList (toString rootUrl) artist artistLyrics

                ShowingLyric lyric ->
                    viewLyric lyric
            ]
        , footer []
            [ div [ class "search" ]
                [ input
                    [ class "search__input", placeholder "Search for artist or lyric", value model.searchTerm, onInput SearchQueryChanged ]
                    []
                ]
            ]
        ]
    }


showQuote : Html Msg
showQuote =
    div [ class "quote" ]
        [ p [ class "quote__text" ]
            [ text "Those who wish to sing always find a song." ]
        , p [ class "quote__author" ]
            [ text "Swedish proverb" ]
        ]


showLoader : Html Msg
showLoader =
    img [ src "loader.svg", alt "an animated loader graphic" ] []


showError : Html Msg
showError =
    text "Oops, something went wrong!"


showArtists : RootUrl -> WebData (List Artist) -> Html Msg
showArtists rootUrl artistData =
    case artistData of
        NotAsked ->
            showQuote

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success artists ->
            div
                [ class "artist__list" ]
                (List.map (viewArtist rootUrl) artists)


viewArtist : RootUrl -> Artist -> Html Msg
viewArtist rootUrl artist =
    a
        [ class "artist__result", href ("/artists/" ++ artist.slug ++ "/lyrics"), onClick (ArtistClicked artist) ]
        [ img [ class "artist__image", src (rootUrl ++ "artists/" ++ artist.slug ++ "/image") ] []
        , p
            [ class "artist__name" ]
            [ text (artist.firstName ++ " " ++ artist.lastName) ]
        ]


showArtistLyricsList : RootUrl -> Artist -> WebData (List LyricListItem) -> Html Msg
showArtistLyricsList rootUrl artist artistLyrics =
    case artistLyrics of
        NotAsked ->
            text ""

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success lyrics ->
            div
                [ class "lyric__list" ]
                (List.map (viewLyricListItem rootUrl artist) lyrics)


viewLyricListItem : RootUrl -> Artist -> LyricListItem -> Html Msg
viewLyricListItem rootUrl artist lyricListItem =
    a
        [ class "lyric-item", onClick (LyricClicked artist lyricListItem.slug) ]
        [ p
            [ class "lyric-item__title" ]
            [ text lyricListItem.title ]
        ]


viewLyric : WebData Lyric -> Html Msg
viewLyric lyric =
    case lyric of
        NotAsked ->
            text ""

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success lyricData ->
            p [ class "lyric__body" ] [ text lyricData.body ]



-- http


searchArtists : String -> String -> Cmd Msg
searchArtists apiRootUrl searchTerm =
    let
        endpoint =
            searchArtistsEndpoint apiRootUrl searchTerm
    in
    request
        { method = "GET"
        , headers = []
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson ArtistsRetrieved artistListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- fetchArtistDetails : String -> Slug -> Cmd Msg
-- fetchArtistDetails apiRootUrl artistSlug =
--     let
--         endpoint =
--             artistDetailsEndpoint apiRootUrl artistSlug
--     in
--     request
--         { method = "GET"
--         , headers = []
--         , url = endpoint
--         , body = Http.emptyBody
--         , expect = Http.expectJson ArtistDetailsRetrieved artistDecoder
--         , timeout = Nothing
--         , tracker = Nothing
--         }


getArtist : String -> Slug -> Task Http.Error Artist
getArtist apiRootUrl artistSlug =
    let
        endpoint =
            artistDetailsEndpoint apiRootUrl artistSlug
    in
    task
        { method = "GET"
        , headers = []
        , url = endpoint
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| artistDecoder
        , timeout = Nothing
        }



-- task :
--     { method : String
--     , headers : List Header
--     , url : String
--     , body : Body
--     , resolver : Resolver x a
--     , timeout : Maybe Float
--     }
--     -> Task x a


getLyricsForArtist : String -> Slug -> Cmd Msg
getLyricsForArtist apiRootUrl artistSlug =
    let
        endpoint =
            artistLyricsEndpoint apiRootUrl artistSlug
    in
    request
        { method = "GET"
        , headers = []
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson LyricsRetrieved lyricListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getLyric : RootUrl -> Artist -> LyricSlug -> Cmd Msg
getLyric apiRootUrl artist lyricSlug =
    let
        endpoint =
            lyricEndpoint apiRootUrl artist.slug lyricSlug
    in
    request
        { method = "GET"
        , headers = []
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson LyricRetrieved lyricDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- decoders


artistSlugListDecoder : Decoder Slug
artistSlugListDecoder =
    list artistSlugDecoder
        |> andThen
            (\slugs ->
                case getPrimarySlug slugs of
                    Nothing ->
                        fail "no primary slug"

                    Just slug ->
                        succeed slug.name
            )


artistSlugDecoder : Decoder ArtistSlug
artistSlugDecoder =
    map2 ArtistSlug
        (field "name" string)
        (field "isPrimary" bool)


artistDecoder : Decoder Artist
artistDecoder =
    map3 Artist
        (field "firstName" string)
        (field "lastName" string)
        (field "slugs" artistSlugListDecoder)


artistListDecoder : Decoder (List Artist)
artistListDecoder =
    field "artists" (list artistDecoder)


lyricListDecoder : Decoder (List LyricListItem)
lyricListDecoder =
    list lyricListItemDecoder


lyricListItemDecoder : Decoder LyricListItem
lyricListItemDecoder =
    map2 LyricListItem
        (field "title" string)
        (field "slug" string)


lyricDecoder : Decoder Lyric
lyricDecoder =
    map2 Lyric
        (field "title" string)
        (field "body" string)



-- tasks
-- getArtistLyrics : Task Error Artist (List LyricListItem)
-- getArtistLyrics
-- helpers


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


getPrimarySlug : List ArtistSlug -> Maybe ArtistSlug
getPrimarySlug artistSlugs =
    List.head (List.filter .isPrimary artistSlugs)
