module Main exposing (..)

import Browser exposing (application)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Endpoint exposing (artistDetailsEndpoint, artistLyricsEndpoint, lyricEndpoint, request, searchArtistsEndpoint, searchLyricsEndpoint)
import Html exposing (Html, a, button, div, h1, h2, header, hr, i, img, input, main_, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onFocus, onInput)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, andThen, bool, fail, field, list, map, map2, map3, string, succeed)
import Route exposing (Route)
import Task
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
    , previousState : Maybe AppState
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
            ( AllOk (AppModel key url f.apiRootUrl "" temp.state Nothing temp.artistSlug), temp.commands )



-- update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | WantToSearch
    | SearchQueryChanged String
    | RetrievedArtistSearchResults (Result Http.Error (List Artist))
    | RetrievedLyricSearchResults (Result Http.Error (List LyricSearchResult))
    | ArtistClicked Artist
    | LyricsRetrieved (Result Http.Error (List LyricListItem))
    | ArtistDetailsRetrieved (Result Http.Error Artist)
    | LyricClicked Slug String
    | LyricRetrieved (Result Http.Error Lyric)
    | WantToGoHome
    | NoOp


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
                    ( { model | url = url, previousState = Just model.state, state = Home }, Cmd.none )

                Route.ArtistRoute slug ->
                    ( { model | url = url, previousState = Just model.state, state = ShowingArtistLyrics { artist = Loading, lyrics = Loading } }, Cmd.batch [ getLyricsForArtist (toString model.apiRootUrl) slug, getArtist (toString model.apiRootUrl) slug ] )

                _ ->
                    ( { model | url = url }, Cmd.none )

        WantToSearch ->
            ( { model | previousState = getPreviousStateValue model.previousState model.state, state = Searching { artists = NotAsked, lyrics = NotAsked } }, focusSearchInput )

        SearchQueryChanged searchTerm ->
            if String.isEmpty searchTerm then
                ( { model | searchTerm = searchTerm, previousState = getPreviousStateValue model.previousState model.state, state = Home }, Cmd.none )

            else
                ( { model | searchTerm = searchTerm, previousState = getPreviousStateValue model.previousState model.state, state = Searching { artists = Loading, lyrics = Loading } }, Cmd.batch [ searchArtists (toString model.apiRootUrl) searchTerm, searchLyrics (toString model.apiRootUrl) searchTerm ] )

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
            ( { model | previousState = getPreviousStateValue model.previousState model.state, state = temp }, Cmd.none )

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
            ( { model | previousState = getPreviousStateValue model.previousState model.state, state = temp }, Cmd.none )

        ArtistClicked artist ->
            ( { model | searchTerm = "", activeArtistSlug = Just artist.primarySlug, previousState = getPreviousStateValue model.previousState model.state, state = ShowingArtistLyrics { artist = Loading, lyrics = Loading } }, Cmd.batch [ getLyricsForArtist (toString model.apiRootUrl) artist.primarySlug, getArtist (toString model.apiRootUrl) artist.primarySlug ] )

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
            ( { model | previousState = Just model.state, state = ShowingLyric Loading, activeArtistSlug = Just artistSlug }, getLyric (toString model.apiRootUrl) artistSlug lyricSlug )

        LyricRetrieved result ->
            case result of
                Ok lyric ->
                    ( { model | state = ShowingLyric (Success lyric) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        WantToGoHome ->
            ( { model | state = Home, searchTerm = "" }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



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
<<<<<<< HEAD
            [ Attr.class (getClass model.state) ]
            [ showHeader model.state model.activeArtistSlug
=======
            [ class (getClass model.state) ]
            [ showHeader model.previousState model.state model.activeArtistSlug
>>>>>>> e3082e2... add a previous state into app state
            , main_ [] <| showState model
            ]
        ]
    }


showHeader : Maybe AppState -> AppState -> Maybe Slug -> Html Msg
showHeader previousState state artistSlug =
    case state of
        ShowingArtistLyrics _ ->
            header []
                [ a
<<<<<<< HEAD
                    [ Attr.href "/", Attr.attribute "role" "button", Attr.attribute "aria-label" "Back" ]
                    [ i [ Attr.class "far fa-long-arrow-left artist__back-icon" ] [] ]
=======
                    [ href (getBackLink previousState), attribute "role" "button", attribute "aria-label" "Back" ]
                    [ i [ class "far fa-long-arrow-left artist__back-icon" ] [] ]
>>>>>>> e3082e2... add a previous state into app state
                ]

        ShowingLyric _ ->
            case artistSlug of
                Just slug ->
                    header
                        []
                        [ a [ Attr.href ("/artists/" ++ slug ++ "/lyrics") ] [ i [ Attr.class "far fa-long-arrow-left artist__back-icon" ] [] ] ]

                Nothing ->
                    text ""

        Searching _ ->
            text ""

        _ ->
            header
                []
                [ showLogo
                ]


getBackLink : Maybe AppState -> String
getBackLink previousState =
    case previousState of
        Just state ->
            case state of
                ShowingArtistLyrics result ->
                    case result.artist of
                        Success a ->
                            "/artists/" ++ a.primarySlug ++ "/lyrics"

                        _ ->
                            ""

                Searching _ ->
                    "/searching"

                Home ->
                    "/"

                _ ->
                    ""

        Nothing ->
            ""


getClass : AppState -> String
getClass state =
    case state of
        Home ->
            "app home"

        Searching _ ->
            "app search"

        ShowingArtistLyrics _ ->
            "app artist"

        ShowingLyric _ ->
            "app lyric"


showState : AppModel -> List (Html Msg)
showState model =
    case model.state of
        Home ->
            [ showQuote, showSearch model.apiRootUrl model.searchTerm model.state ]

        Searching result ->
            [ showMainSearch model.searchTerm
            , case ( result.artists, result.lyrics ) of
                ( Success _, Success _ ) ->
                    div [ Attr.class "search__results" ]
                        [ showArtists (toString model.apiRootUrl) result.artists
                        , showLyricSearchResults (toString model.apiRootUrl) result.lyrics
                        ]

                ( Loading, Loading ) ->
                    div [] [ showSearchArtistResultsLoader, showSearchLyricResultsLoader ]

                ( _, Loading ) ->
                    showSearchLyricResultsLoader

                ( Loading, _ ) ->
                    showSearchArtistResultsLoader

                _ ->
                    text ""
            , button [ Attr.class "search__cancel-btn", onClick WantToGoHome ] [ i [ Attr.class "fas fa-times" ] [] ]
            ]

        ShowingArtistLyrics artistResult ->
            case ( model.apiRootUrl, model.activeArtistSlug ) of
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


showSearchArtistResultsLoader : Html Msg
showSearchArtistResultsLoader =
    div
        [ Attr.class "search__artists-wrap" ]
        [ h2 [ Attr.class "search__sub-heading" ] [ text "Hunermend" ]
        , hr [ Attr.class "search__ruler" ] []
        , div [ Attr.class "search__loader" ]
            [ div [ Attr.class "loader__item" ]
                [ div [ Attr.class "loader__image animate" ] []
                , div [ Attr.class "loader__artist animate" ] []
                ]
            , div [ Attr.class "loader__item" ]
                [ div [ Attr.class "loader__image animate" ] []
                , div [ Attr.class "loader__artist animate" ] []
                ]
            ]
        ]


showSearchLyricResultsLoader : Html Msg
showSearchLyricResultsLoader =
    div
        [ Attr.class "search__lyrics-wrap" ]
        [ h2 [ Attr.class "search__sub-heading" ] [ text "Stran" ]
        , hr [ Attr.class "search__ruler" ] []
        , div [ Attr.class "search__loader" ]
            [ div [ Attr.class "loader__item" ]
                [ div [ Attr.class "loader__image animate" ] []
                , div [ Attr.class "loader__info" ]
                    [ div [ Attr.class "loader__lyric animate" ] []
                    , div [ Attr.class "loader__artist animate" ] []
                    ]
                ]
            , div [ Attr.class "loader__item" ]
                [ div [ Attr.class "loader__image animate" ] []
                , div [ Attr.class "loader__info" ]
                    [ div [ Attr.class "loader__lyric animate" ] []
                    , div [ Attr.class "loader__artist animate" ] []
                    ]
                ]
            ]
        ]


focusSearchInput : Cmd Msg
focusSearchInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "search__input-main")


showLogo : Html Msg
showLogo =
    div
        [ Attr.class "logo" ]
        [ a
            [ Attr.href "/", onClick WantToGoHome ]
            [ img [ Attr.src "images/bejebeje-logo.svg", Attr.alt "Bêjebêje's logo", Attr.class "logo__svg", Attr.width 30 ] [] ]
        ]


showMainSearch : String -> Html Msg
showMainSearch searchTerm =
    input
        [ Attr.id "search__input-main"
        , Attr.class "search__input"
        , Attr.placeholder "Li stranê yan jî hunermend bigere"
        , Attr.value searchTerm
        , onInput SearchQueryChanged
        , Attr.attribute "aria-label" "search"
        ]
        []


showSearch : Url -> String -> AppState -> Html Msg
showSearch rootUrl searchTerm state =
    div [ Attr.class "search__wrap", Attr.attribute "role" "search" ]
        [ i [ Attr.class "far fa-long-arrow-left search__icon" ] []
        , input
            [ Attr.id "search__input"
            , Attr.class "search__input"
            , Attr.placeholder "Li stranê yan jî hunermend bigere"
            , Attr.value searchTerm
            , onInput SearchQueryChanged
            , onFocus WantToSearch
            , Attr.attribute "aria-label" "search"
            ]
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
    div [ Attr.class "quote" ]
        [ p [ Attr.class "quote__text" ]
            [ text "Kurd ji hev cuda dilopên baranê ne, cihê cihê têne daqurtandin. Ko gihane hev debin lehî, lehîke boş. Tu kes, tu tişt li ber wan nikare bisekine. Felata welatê me di rabûna vê lehiyê de ye." ]
        , p [ Attr.class "quote__author" ]
            [ text "Celadet Alî Bedirxan" ]
        ]


showLoader : Html Msg
showLoader =
    img [ Attr.src "loader.svg", Attr.alt "an animated loader graphic" ] []


showError : Html Msg
showError =
    text "Oops, something went wrong!"


showArtists : RootUrl -> WebData (List Artist) -> Html Msg
showArtists rootUrl artistData =
    case artistData of
        NotAsked ->
            text ""

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success artists ->
            if List.length artists > 0 then
                div
                    [ Attr.class "search__artists-wrap" ]
                    [ h2 [ Attr.class "search__sub-heading" ] [ text "Hunermend" ]
                    , hr [ Attr.class "search__ruler" ] []
                    , div [ Attr.class "search__artists-results" ] (List.map (viewArtist rootUrl) artists)
                    ]

            else
                text ""


showLyricSearchResults : RootUrl -> WebData (List LyricSearchResult) -> Html Msg
showLyricSearchResults rootUrl lyricSearchResults =
    case lyricSearchResults of
        NotAsked ->
            text ""

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success lyrics ->
            if List.length lyrics > 0 then
                div
                    [ Attr.class "search__lyrics-wrap" ]
                    [ h2 [ Attr.class "search__sub-heading" ] [ text "Stran" ]
                    , hr [ Attr.class "search__ruler" ] []
                    , div [ Attr.class "search__lyrics-results" ] (List.map (viewLyricSearchResult rootUrl) lyrics)
                    ]

            else
                text ""


viewLyricSearchResult : RootUrl -> LyricSearchResult -> Html Msg
viewLyricSearchResult rootUrl lyricSearchResult =
    a
        [ Attr.class "lyric__search-result"
        , Attr.href ("/artists/" ++ lyricSearchResult.artist.primarySlug ++ "/lyrics/" ++ lyricSearchResult.primarySlug)
        , onClick (LyricClicked lyricSearchResult.artist.primarySlug lyricSearchResult.primarySlug)
        ]
        [ img
            [ Attr.class "search__artist-image"
            , Attr.src (getImagePath rootUrl lyricSearchResult.artist.hasImage lyricSearchResult.artist.primarySlug)
            , Attr.alt lyricSearchResult.artist.fullName
            ]
            []
        , div
            [ Attr.class "search__lyric-info" ]
            [ p
                [ Attr.class "search__lyric-title" ]
                [ text lyricSearchResult.title ]
            , p [ Attr.class "search__lyric-artist-name" ]
                [ text lyricSearchResult.artist.fullName ]
            ]
        ]


viewArtist : RootUrl -> Artist -> Html Msg
viewArtist rootUrl artist =
    a
        [ Attr.class "artist__result", Attr.href ("/artists/" ++ artist.primarySlug ++ "/lyrics"), onClick (ArtistClicked artist) ]
        [ img [ Attr.class "search__artist-image", Attr.src (getImagePath rootUrl artist.hasImage artist.primarySlug), Attr.alt artist.fullName ] []
        , p
            [ Attr.class "artist__name" ]
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
                    [ Attr.class "lyric__list" ]
                    (List.map (viewLyricListItem artistSlug) lyrics)

            else
                div [ Attr.class "lyric__empty-list" ] [ i [ Attr.class "fad fa-pennant lyric__empty-icon" ] [], p [ Attr.class "lyric__empty-text" ] [ text "Bibure, vê demê stran tune ne!" ] ]


viewLyricListItem : Slug -> LyricListItem -> Html Msg
viewLyricListItem artistSlug lyricListItem =
    a
        [ Attr.class "lyric-item", Attr.href ("/artists/" ++ artistSlug ++ "/lyrics/" ++ lyricListItem.slug), onClick (LyricClicked artistSlug lyricListItem.slug) ]
        [ p
            [ Attr.class "lyric-item__title" ]
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
            p [ Attr.class "lyric__body" ] [ text lyricData.body ]


viewArtistCardOnLyricsList : RootUrl -> Artist -> WebData (List LyricListItem) -> Html Msg
viewArtistCardOnLyricsList rootUrl artist lyricsData =
    div
        [ Attr.class "card artist-card" ]
        [ img
            [ Attr.class "artist-card__image", Attr.src (getImagePath rootUrl artist.hasImage artist.primarySlug), Attr.alt artist.fullName ]
            []
        , div [ Attr.class "artist-card__meta" ]
            [ h1
                [ Attr.class "artist-card__name" ]
                [ text artist.fullName ]
            , h2
                [ Attr.class "artist-card__lyric-count" ]
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


getPreviousStateValue : Maybe AppState -> AppState -> Maybe AppState
getPreviousStateValue previousState state =
    case previousState of
        Just prevState ->
            let
                temp =
                    Debug.log "prevState" prevState

                tempTwo =
                    Debug.log "state" state
            in
            if isDifferentFromPreviousState prevState state then
                Just state

            else
                Just prevState

        Nothing ->
            Just state


isDifferentFromPreviousState : AppState -> AppState -> Bool
isDifferentFromPreviousState previousState state =
    case ( previousState, state ) of
        ( ShowingLyric _, ShowingLyric _ ) ->
            False

        ( ShowingArtistLyrics _, ShowingArtistLyrics _ ) ->
            False

        ( Searching _, Searching _ ) ->
            False

        _ ->
            True
