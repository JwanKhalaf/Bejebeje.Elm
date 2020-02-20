module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Endpoint exposing (artistDetailsEndpoint, artistLyricsEndpoint, lyricEndpoint, request, searchArtistsEndpoint)
import Html exposing (Html, a, div, h1, header, i, img, input, main_, p, span, text)
import Html.Attributes exposing (alt, class, href, placeholder, src, value, attribute)
import Html.Events exposing (onClick, onInput)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, andThen, bool, fail, field, list, map, map2, map3, string, succeed)
import Url exposing (Url, fromString, toString)
import Url.Parser as Parser exposing ((</>), Parser)
import Route exposing (Route)


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


type alias ArtistRestult =
    { artist : WebData Artist
    , lyrics : WebData (List LyricListItem)
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
    | ArtistsRetrieved (Result Http.Error (List Artist))
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
                ( { model | searchTerm = searchTerm }, searchArtists (toString model.apiRootUrl) searchTerm )

        ArtistsRetrieved result ->
            case result of
                Ok artists ->
                    ( { model | state = SearchingArtists (Success artists) }, Cmd.none )

                Err error ->
                    ( { model | state = SearchingArtists (Failure error) }, Cmd.none )

        ArtistClicked artist ->
            ( { model | searchTerm = "", activeArtistSlug = Just artist.slug, state = ShowingArtistLyrics { artist = Loading, lyrics = Loading } }, Cmd.batch [ getLyricsForArtist (toString model.apiRootUrl) artist.slug, getArtist (toString model.apiRootUrl) artist.slug ] )

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

        LyricClicked artist lyricSlug ->
            ( model, getLyric (toString model.apiRootUrl) artist lyricSlug )

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
        SearchingArtists _ ->
            "search-results"

        ShowingArtistLyrics _ ->
            "artist"

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
            SearchingArtists artists ->
                showArtists (toString rootUrl) artists

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
            div
                [ class "artist__list" ]
                (List.map (viewArtist rootUrl) artists)


viewArtist : RootUrl -> Artist -> Html Msg
viewArtist rootUrl artist =
    a
        [ class "artist__result", href ("/artists/" ++ artist.slug ++ "/lyrics"), onClick (ArtistClicked artist) ]
        [ img [ class "artist__image", src (rootUrl ++ "artists/" ++ artist.slug ++ "/image"), alt (artist.firstName ++ " " ++ artist.lastName) ] []
        , p
            [ class "artist__name" ]
            [ text (artist.firstName ++ " " ++ artist.lastName) ]
        ]


showArtistDetails : RootUrl -> WebData Artist -> Html Msg
showArtistDetails rootUrl artist =
    case artist of
        NotAsked ->
            text ""

        Loading ->
            showLoader

        Failure _ ->
            showError

        Success a ->
            viewArtistCardOnLyricsList rootUrl a


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
                div [ class "lyric__empty-list" ] [ i [ class "fad fa-pennant lyric__empty-icon" ] [], p [ class "lyric__empty-text" ] [ text "Sorry, no lyrics just yet!" ] ]


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


viewArtistCardOnLyricsList : RootUrl -> Artist -> Html Msg
viewArtistCardOnLyricsList rootUrl artist =
    div
        [ class "card artist-card" ]
        [ img
            [ class "artist-card__image", src (rootUrl ++ "artists/" ++ artist.slug ++ "/image"), alt (artist.firstName ++ " " ++ artist.lastName) ]
            []
        , h1
            [ class "artist-card__name" ]
            [ text artist.firstName, text " ", text artist.lastName ]
        ]



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


artistDetailsDecoder : Decoder Artist
artistDetailsDecoder =
    map3 Artist
        (field "firstName" string)
        (field "lastName" string)
        (field "slug" string)


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
