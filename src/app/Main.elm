module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Endpoint exposing (artistDetailsEndpoint, artistLyricsEndpoint, lyricEndpoint, request, searchArtistsEndpoint, searchLyricsEndpoint)
import Html exposing (Html, a, div, h1, h2, header, hr, i, img, input, main_, p, span, text)
import Html.Attributes exposing (alt, attribute, class, href, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, andThen, bool, fail, field, int, list, map, map2, map3, map4, string, succeed)
import Route exposing (Route)
import Url exposing (Url, fromString, toString)
import Url.Parser as Parser exposing ((</>), Parser)


main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = topView view
        , update = topUpdate update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- model


type AppState
    = ShowingLyric (WebData Lyric)
    | ShowingArtistLyrics ArtistRestult
    | Searching SearchResult
    | Home


type alias Slug =
    String


type alias LyricSlug =
    String


type alias RootUrl =
    String


type alias Artist =
    { fullName : String
    , hasImage : Bool
    , primarySlug : Slug
    }


type alias LyricListItem =
    { title : String
    , slug : Slug
    }


type alias LyricSearchArtistResult =
    { fullName : String
    , primarySlug : Slug
    , hasImage : Bool
    }


type alias LyricSearchResult =
    { title : String
    , primarySlug : Slug
    , artist : LyricSearchArtistResult
    }


type alias Lyric =
    { title : String
    , body : String
    }


type alias ArtistSlug =
    { name : String
    , isPrimary : Bool
    }


type alias ArtistRestult =
    { artist : WebData Artist
    , lyrics : WebData (List LyricListItem)
    }


type alias SearchResult =
    { artists : WebData (List Artist)
    , lyrics : WebData (List LyricSearchResult)
    }


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type alias WebData a =
    RemoteData Http.Error a


type alias Flags =
    { apiRootUrl : Url
    }


type alias AppModel =
    { key : Nav.Key
    , url : Url
    , apiRootUrl : Url
    , searchTerm : String
    , state : AppState
    , activeArtistSlug : Maybe Slug
    }


type Model
    = AllOk AppModel
    | FatalError String


init : Json.Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case Json.Decode.decodeValue flagsDecoder flags of
        Err err ->
            ( FatalError (Json.Decode.errorToString err), Cmd.none )

        Ok f ->
            let
                parsedUrl =
                    Maybe.withDefault Route.NotFoundRoute (Parser.parse Route.parser url)

                temp =
                    case parsedUrl of
                        Route.HomeRoute ->
                            { commands = Cmd.none, state = Home, artistSlug = Nothing }

                        Route.ArtistRoute artist ->
                            { commands =
                                Cmd.batch
                                    [ getLyricsForArtist (Url.toString f.apiRootUrl) artist
                                    , getArtist (toString f.apiRootUrl) artist
                                    ]
                            , state = ShowingArtistLyrics { artist = Loading, lyrics = Loading }
                            , artistSlug = Just artist
                            }

                        Route.LyricRoute artist lyric ->
                            { commands = getLyric (toString f.apiRootUrl) artist lyric
                            , state = ShowingLyric Loading
                            , artistSlug = Just artist
                            }

                        Route.NotFoundRoute ->
                            { commands = Cmd.none, state = Home, artistSlug = Nothing }
            in
            ( AllOk (AppModel key url f.apiRootUrl "" temp.state temp.artistSlug), temp.commands )



-- update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | SearchQueryChanged String
    | RetrievedArtistSearchResults (Result Http.Error (List Artist))
    | RetrievedLyricSearchResults (Result Http.Error (List LyricSearchResult))
    | ArtistClicked Artist
    | LyricsRetrieved (Result Http.Error (List LyricListItem))
    | ArtistDetailsRetrieved (Result Http.Error Artist)
    | LyricClicked Slug String
    | LyricRetrieved (Result Http.Error Lyric)
    | WantToGoHome


topUpdate : (Msg -> AppModel -> ( AppModel, Cmd Msg )) -> Msg -> Model -> ( Model, Cmd Msg )
topUpdate fn msg model =
    case model of
        AllOk m ->
            fn msg m |> Tuple.mapFirst AllOk

        _ ->
            ( model, Cmd.none )


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                parsedUrl =
                    Maybe.withDefault Route.NotFoundRoute (Parser.parse Route.parser url)
            in
            case parsedUrl of
                Route.HomeRoute ->
                    ( { model | url = url, state = Home }, Cmd.none )

                Route.ArtistRoute slug ->
                    ( { model | url = url, state = ShowingArtistLyrics { artist = Loading, lyrics = Loading } }, Cmd.batch [ getLyricsForArtist (toString model.apiRootUrl) slug, getArtist (toString model.apiRootUrl) slug ] )

                _ ->
                    ( { model | url = url }, Cmd.none )

        SearchQueryChanged searchTerm ->
            if String.isEmpty searchTerm then
                ( { model | searchTerm = searchTerm, state = Home }, Cmd.none )

            else
                ( { model | searchTerm = searchTerm, state = Searching { artists = Loading, lyrics = Loading } }, Cmd.batch [ searchArtists (toString model.apiRootUrl) searchTerm, searchLyrics (toString model.apiRootUrl) searchTerm ] )

        RetrievedArtistSearchResults result ->
            let
                temp =
                    case model.state of
                        Searching a ->
                            case result of
                                Ok artists ->
                                    Searching { a | artists = Success artists }

                                Err error ->
                                    Searching { a | artists = Failure error }

                        _ ->
                            model.state
            in
            ( { model | state = temp }, Cmd.none )

        RetrievedLyricSearchResults result ->
            let
                temp =
                    case model.state of
                        Searching a ->
                            case result of
                                Ok lyricSearchResults ->
                                    Searching { a | lyrics = Success lyricSearchResults }

                                Err error ->
                                    Searching { a | lyrics = Failure error }

                        _ ->
                            model.state
            in
            ( { model | state = temp }, Cmd.none )

        ArtistClicked artist ->
            ( { model | searchTerm = "", activeArtistSlug = Just artist.primarySlug, state = ShowingArtistLyrics { artist = Loading, lyrics = Loading } }, Cmd.batch [ getLyricsForArtist (toString model.apiRootUrl) artist.primarySlug, getArtist (toString model.apiRootUrl) artist.primarySlug ] )

        LyricsRetrieved result ->
            let
                temp =
                    case model.state of
                        ShowingArtistLyrics a ->
                            case result of
                                Ok artistLyrics ->
                                    ShowingArtistLyrics { a | lyrics = Success artistLyrics }

                                Err e ->
                                    ShowingArtistLyrics { a | lyrics = Failure e }

                        _ ->
                            model.state
            in
            ( { model | state = temp }, Cmd.none )

        ArtistDetailsRetrieved result ->
            let
                temp =
                    case model.state of
                        ShowingArtistLyrics a ->
                            case result of
                                Ok art ->
                                    ShowingArtistLyrics { a | artist = Success art }

                                Err e ->
                                    ShowingArtistLyrics { a | artist = Failure e }

                        _ ->
                            model.state
            in
            ( { model | state = temp }, Cmd.none )

        LyricClicked artistSlug lyricSlug ->
            ( { model | state = ShowingLyric Loading, activeArtistSlug = Just artistSlug }, getLyric (toString model.apiRootUrl) artistSlug lyricSlug )

        LyricRetrieved result ->
            case result of
                Ok lyric ->
                    ( { model | state = ShowingLyric (Success lyric) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        WantToGoHome ->
            ( { model | state = Home, searchTerm = "" }, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- view


topView : (AppModel -> Browser.Document Msg) -> Model -> Browser.Document Msg
topView viewFunction model =
    case model of
        AllOk m ->
            viewFunction m

        FatalError _ ->
            { title = "Fatal Error on Bêjebêje"
            , body = [ text "sorry, something went wrong" ]
            }


view : AppModel -> Browser.Document Msg
view model =
    { title = "Bêjebêje"
    , body =
        [ div
            [ class "app" ]
            [ header [] <| showHeader model.state model.activeArtistSlug
            , main_ [ class (getClass model.state) ] <| showState model.apiRootUrl model.state model.activeArtistSlug
            , showSearch model.apiRootUrl model.searchTerm model.state
            ]
        ]
    }


showHeader : AppState -> Maybe Slug -> List (Html Msg)
showHeader state artistSlug =
    case state of
        ShowingArtistLyrics _ ->
            [ a [ href "/", attribute "role" "button", attribute "aria-label" "Back" ] [ i [ class "far fa-long-arrow-left artist__back-icon" ] [] ] ]

        ShowingLyric _ ->
            case artistSlug of
                Just slug ->
                    [ a [ href ("/artists/" ++ slug ++ "/lyrics") ] [ i [ class "far fa-long-arrow-left artist__back-icon" ] [] ] ]

                Nothing ->
                    [ text "" ]

        _ ->
            [ showLogo ]


getClass : AppState -> String
getClass state =
    case state of
        Searching _ ->
            "search-results"

        ShowingArtistLyrics _ ->
            "artist"

        ShowingLyric _ ->
            "lyric"

        _ ->
            ""


showState : Url -> AppState -> Maybe Slug -> List (Html Msg)
showState rootUrl state activeArtistSlug =
    case state of
        Home ->
            [ showQuote ]

        ShowingArtistLyrics artistResult ->
            case ( rootUrl, activeArtistSlug ) of
                ( a, Just artist ) ->
                    [ showArtistDetails (toString a)
                        artistResult.artist
                        artistResult.lyrics
                    , showArtistLyricsList
                        (toString a)
                        artist
                        artistResult.lyrics
                    ]

                _ ->
                    [ text "" ]

        ShowingLyric lyric ->
            [ viewLyric lyric ]

        _ ->
            [ text "" ]


showLogo : Html Msg
showLogo =
    div
        [ class "logo" ]
        [ a
            [ href "/", onClick WantToGoHome ]
            [ h1 [ class "logo__text" ] [ span [ class "logo__letter" ] [ text "B" ], text "êjebêje" ] ]
        ]


showSearch : Url -> String -> AppState -> Html Msg
showSearch rootUrl searchTerm state =
    div [ class "search", attribute "role" "search" ]
        [ i [ class "far fa-long-arrow-left search__icon" ] []
        , input
            [ class "search__input", placeholder "Li hunermend bigere", value searchTerm, onInput SearchQueryChanged, attribute "aria-label" "search" ]
            []
        , case state of
            Searching result ->
                showArtists (toString rootUrl) result.artists

            _ ->
                text ""
        , case state of
            Searching result ->
                showLyricSearchResults (toString rootUrl) result.lyrics

            _ ->
                text ""
        ]


showQuote : Html Msg
showQuote =
    div [ class "quote" ]
        [ p [ class "quote__text" ]
            [ text "Kurd ji hev cuda dilopên baranê ne, cihê cihê têne daqurtandin. Ko gihane hev debin lehî, lehîke boş. Tu kes, tu tişt li ber wan nikare bisekine. Felata welatê me di rabûna vê lehiyê de ye." ]
        , p [ class "quote__author" ]
            [ text "Celadet Alî Bedirxan" ]
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
            if List.length artists > 0 then
                div
                    [ class "search__artists-wrap" ]
                    [ h2 [ class "search__sub-heading" ] [ text "Hunermend" ]
                    , hr [ class "search__ruler" ] []
                    , div [ class "search__artists-results" ] (List.map (viewArtist rootUrl) artists)
                    ]

            else
                text ""


showLyricSearchResults : RootUrl -> WebData (List LyricSearchResult) -> Html Msg
showLyricSearchResults rootUrl lyricSearchResults =
    case lyricSearchResults of
        NotAsked ->
            showQuote

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success lyrics ->
            if List.length lyrics > 0 then
                div
                    [ class "search__lyrics-wrap" ]
                    [ h2 [ class "search__sub-heading" ] [ text "Stran" ]
                    , hr [ class "search__ruler" ] []
                    , div [ class "search__lyrics-results" ] (List.map (viewLyricSearchResult rootUrl) lyrics)
                    ]

            else
                text ""


viewLyricSearchResult : RootUrl -> LyricSearchResult -> Html Msg
viewLyricSearchResult rootUrl lyricSearchResult =
    a
        [ class "lyric__search-result"
        , href ("/artists/" ++ lyricSearchResult.artist.primarySlug ++ "/lyrics/" ++ lyricSearchResult.primarySlug)
        , onClick (LyricClicked lyricSearchResult.artist.primarySlug lyricSearchResult.primarySlug)
        ]
        [ img
            [ class "search__artist-image"
            , src (getImagePath rootUrl True lyricSearchResult.artist.primarySlug)
            , alt lyricSearchResult.artist.fullName
            ]
            []
        , div
            [ class "search__lyric-info" ]
            [ p
                [ class "search__lyric-title" ]
                [ text lyricSearchResult.title ]
            , p [ class "search__lyric-artist-name" ]
                [ text lyricSearchResult.artist.fullName ]
            ]
        ]


viewArtist : RootUrl -> Artist -> Html Msg
viewArtist rootUrl artist =
    a
        [ class "artist__result", href ("/artists/" ++ artist.primarySlug ++ "/lyrics"), onClick (ArtistClicked artist) ]
        [ img [ class "search__artist-image", src (getImagePath rootUrl True artist.primarySlug), alt artist.fullName ] []
        , p
            [ class "artist__name" ]
            [ text artist.fullName ]
        ]


showArtistDetails : RootUrl -> WebData Artist -> WebData (List LyricListItem) -> Html Msg
showArtistDetails rootUrl artist lyricsData =
    case artist of
        NotAsked ->
            text ""

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success a ->
            viewArtistCardOnLyricsList rootUrl a lyricsData


showArtistLyricsList : RootUrl -> Slug -> WebData (List LyricListItem) -> Html Msg
showArtistLyricsList rootUrl artistSlug artistLyrics =
    case artistLyrics of
        NotAsked ->
            text ""

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success lyrics ->
            if List.length lyrics > 0 then
                div
                    [ class "lyric__list" ]
                    (List.map (viewLyricListItem artistSlug) lyrics)

            else
                div [ class "lyric__empty-list" ] [ i [ class "fad fa-pennant lyric__empty-icon" ] [], p [ class "lyric__empty-text" ] [ text "Bibure, vê demê stran tune ne!" ] ]


viewLyricListItem : Slug -> LyricListItem -> Html Msg
viewLyricListItem artistSlug lyricListItem =
    a
        [ class "lyric-item", href ("/artists/" ++ artistSlug ++ "/lyrics/" ++ lyricListItem.slug), onClick (LyricClicked artistSlug lyricListItem.slug) ]
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


viewArtistCardOnLyricsList : RootUrl -> Artist -> WebData (List LyricListItem) -> Html Msg
viewArtistCardOnLyricsList rootUrl artist lyricsData =
    div
        [ class "card artist-card" ]
        [ img
            [ class "artist-card__image", src (getImagePath rootUrl True artist.primarySlug), alt artist.fullName ]
            []
        , div [ class "artist-card__meta" ]
            [ h1
                [ class "artist-card__name" ]
                [ text artist.fullName ]
            , h2
                [ class "artist-card__lyric-count" ]
                [ showLyricCount lyricsData ]
            ]
        ]


getImagePath : RootUrl -> Bool -> Slug -> String
getImagePath rootUrl hasImage artistSlug =
    if hasImage then
        rootUrl ++ "artists/" ++ artistSlug ++ "/image"

    else
        "/images/no-photo.jpg"


showLyricCount : WebData (List LyricListItem) -> Html Msg
showLyricCount lyricData =
    case lyricData of
        NotAsked ->
            text ""

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success lyrics ->
            text (String.fromInt (List.length lyrics) ++ " Stran")



-- http


searchArtists : String -> String -> Cmd Msg
searchArtists apiRootUrl artistName =
    let
        endpoint =
            searchArtistsEndpoint apiRootUrl artistName
    in
    request
        { method = "GET"
        , headers = []
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson RetrievedArtistSearchResults artistListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


searchLyrics : String -> String -> Cmd Msg
searchLyrics apiRootUrl lyricTitle =
    let
        endpoint =
            searchLyricsEndpoint apiRootUrl lyricTitle
    in
    request
        { method = "GET"
        , headers = []
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson RetrievedLyricSearchResults lyricSearchResultsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


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


getArtist : String -> Slug -> Cmd Msg
getArtist apiRootUrl artistSlug =
    let
        endpoint =
            artistDetailsEndpoint apiRootUrl artistSlug
    in
    request
        { method = "GET"
        , headers = []
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson ArtistDetailsRetrieved artistDetailsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getLyric : RootUrl -> Slug -> LyricSlug -> Cmd Msg
getLyric apiRootUrl artistSlug lyricSlug =
    let
        endpoint =
            lyricEndpoint apiRootUrl artistSlug lyricSlug
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


flagsDecoder : Decoder Flags
flagsDecoder =
    map Flags
        (field "apiRootUrl" urlDecoder)


urlDecoder : Json.Decode.Decoder Url
urlDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\urlString ->
                case Url.fromString urlString of
                    Just url ->
                        Json.Decode.succeed url

                    Nothing ->
                        Json.Decode.fail <| urlString ++ " is not a valid url."
            )


artistSlugDecoder : Decoder ArtistSlug
artistSlugDecoder =
    map2 ArtistSlug
        (field "name" string)
        (field "isPrimary" bool)


artistDecoder : Decoder Artist
artistDecoder =
    map3 Artist
        (field "fullName" string)
        (field "hasImage" bool)
        (field "primarySlug" string)


artistDetailsDecoder : Decoder Artist
artistDetailsDecoder =
    map3 Artist
        (field "fullName" string)
        (field "hasImage" bool)
        (field "primarySlug" string)


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


lyricSearchResultsDecoder : Decoder (List LyricSearchResult)
lyricSearchResultsDecoder =
    field "lyrics" (list lyricSearchResultDecoder)


lyricSearchResultDecoder : Decoder LyricSearchResult
lyricSearchResultDecoder =
    map3 LyricSearchResult
        (field "title" string)
        (field "primarySlug" string)
        (field "artist" lyricSearchArtistResultDecoder)


lyricSearchArtistResultDecoder : Decoder LyricSearchArtistResult
lyricSearchArtistResultDecoder =
    map3 LyricSearchArtistResult
        (field "fullName" string)
        (field "primarySlug" string)
        (field "hasImage" bool)


lyricDecoder : Decoder Lyric
lyricDecoder =
    map2 Lyric
        (field "title" string)
        (field "body" string)


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
