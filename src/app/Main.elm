module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Endpoint exposing (Endpoint, request, searchArtists)
import Html exposing (Html, div, footer, h1, header, img, input, main_, p, span, text)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (onInput)
import Http exposing (expectJson, get)
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


type alias Slug =
    String


type alias Artist =
    { firstName : String
    , lastName : String
    , slug : Slug
    }


type alias ArtistSlug =
    { name : String
    , isPrimary : Bool
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , searchTerm : String
    , retrievedArtists : List Artist
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url "" [], Cmd.none )



-- update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SearchQueryChanged String
    | ArtistsRetrieved (Result Http.Error (List Artist))


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
                ( { model | searchTerm = searchTerm, retrievedArtists = [] }, Cmd.none )

            else
                ( { model | searchTerm = searchTerm }, searchArtists searchTerm )

        ArtistsRetrieved result ->
            case result of
                Ok artists ->
                    ( { model | retrievedArtists = artists }, Cmd.none )

                Err _ ->
                    ( { model | retrievedArtists = [] }, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ header []
            [ div
                [ class "logo" ]
                [ h1 [ class "logo__text" ] [ span [ class "logo__letter" ] [ text "B" ], text "êjebêje" ] ]
            ]
        , main_ []
            [ viewArtists model.retrievedArtists ]
        , footer []
            [ div [ class "search" ]
                [ input
                    [ class "search__input", placeholder "Search for artist or lyric", value model.searchTerm, onInput SearchQueryChanged ]
                    []
                ]
            ]
        ]
    }


viewArtists : List Artist -> Html Msg
viewArtists retrievedArtists =
    case retrievedArtists of
        [] ->
            div [ class "quote" ]
                [ p [ class "quote__text" ]
                    [ text "Those who wish to sing always find a song." ]
                , p [ class "quote__author" ]
                    [ text "Swedish proverb" ]
                ]

        artists ->
            div
                [ class "artist__list" ]
                (List.map viewArtist artists)


viewArtist : Artist -> Html Msg
viewArtist artist =
    div
        [ class "artist__result" ]
        [ img [ class "artist__image", src ("http://localhost:5010/artists/" ++ artist.slug ++ "/image") ] []
        , p
            [ class "artist__name" ]
            [ text (artist.firstName ++ " " ++ artist.lastName) ]
        ]



-- http


searchArtists : String -> Cmd Msg
searchArtists searchTerm =
    Http.get
        { url = "http://localhost:5010/artists?name=" ++ searchTerm
        , expect = Http.expectJson ArtistsRetrieved artistListDecoder
        }


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


getPrimarySlug : List ArtistSlug -> Maybe ArtistSlug
getPrimarySlug artistSlugs =
    List.head (List.filter .isPrimary artistSlugs)
