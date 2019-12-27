module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Endpoint exposing (artistLyricsEndpoint, request, searchArtistsEndpoint)
import Html exposing (Html, a, div, footer, h1, header, img, input, main_, p, span, text)
import Html.Attributes exposing (alt, class, href, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, andThen, bool, fail, field, list, map2, map3, string, succeed)
import Url


main : Program () Model Msg
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
    = SearchingArtists (WebData (List Artist))
    | Home


type alias Slug =
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


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , searchTerm : String
    , state : AppState
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url "" Home, Cmd.none )



-- update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SearchQueryChanged String
    | ArtistsRetrieved (Result Http.Error (List Artist))
    | ArtistClicked String
    | LyricsRetrieved (Result Http.Error (List LyricListItem))
    | LyricClicked String


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
                ( { model | searchTerm = searchTerm }, searchArtists searchTerm )

        ArtistsRetrieved result ->
            case result of
                Ok artists ->
                    ( { model | state = SearchingArtists (Success artists) }, Cmd.none )

                Err error ->
                    ( { model | state = SearchingArtists (Failure error) }, Cmd.none )

        ArtistClicked artistSlug ->
            ( model, getLyricsForArtist artistSlug )

        LyricsRetrieved result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        LyricClicked _ ->
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
                    showArtists artists
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


showArtists : WebData (List Artist) -> Html Msg
showArtists artistData =
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
                (List.map viewArtist artists)


viewArtist : Artist -> Html Msg
viewArtist artist =
    a
        [ class "artist__result", href ("/artists/" ++ artist.slug ++ "/lyrics"), onClick (ArtistClicked artist.slug) ]
        [ img [ class "artist__image", src ("http://localhost:5010/artists/" ++ artist.slug ++ "/image") ] []
        , p
            [ class "artist__name" ]
            [ text (artist.firstName ++ " " ++ artist.lastName) ]
        ]


showArtistLyricsList : List LyricListItem -> Html Msg
showArtistLyricsList retrievedLyrics =
    case retrievedLyrics of
        [] ->
            text ""

        lyrics ->
            div
                [ class "lyric__list" ]
                (List.map viewLyricListItem lyrics)


viewLyricListItem : LyricListItem -> Html Msg
viewLyricListItem lyricListItem =
    a
        [ class "lyric-item", href ("/artists/acdc/lyrics/" ++ lyricListItem.slug), onClick (LyricClicked lyricListItem.slug) ]
        [ p
            [ class "lyric-item__title" ]
            [ text lyricListItem.title ]
        ]



-- http


searchArtists : String -> Cmd Msg
searchArtists searchTerm =
    let
        endpoint =
            searchArtistsEndpoint searchTerm
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


getLyricsForArtist : Slug -> Cmd Msg
getLyricsForArtist artistSlug =
    let
        endpoint =
            artistLyricsEndpoint artistSlug
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



-- helpers


getPrimarySlug : List ArtistSlug -> Maybe ArtistSlug
getPrimarySlug artistSlugs =
    List.head (List.filter .isPrimary artistSlugs)
