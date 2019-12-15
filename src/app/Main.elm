module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Html exposing (Html, div, footer, h1, header, input, main_, p, span, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)
import Http exposing (expectJson, get)
import Json.Decode exposing (Decoder, field, int, list, map3, string)
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


type alias Artist =
    { firstName : String
    , lastName : String
    , imageId : Int
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
            [ div [ class "quote" ]
                [ p [ class "quote__text" ]
                    [ text "Those who wish to sing always find a song." ]
                , p [ class "quote__author" ]
                    [ text "Swedish proverb" ]
                ]
            , viewArtists model.retrievedArtists
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


viewArtists : List Artist -> Html Msg
viewArtists retrievedArtists =
    case retrievedArtists of
        [] ->
            text "no matched artists"

        _ ->
            text "there are artists"



-- http


searchArtists : String -> Cmd Msg
searchArtists searchTerm =
    Http.get
        { url = "https://api.bejebeje.com/artists?name=" ++ searchTerm
        , expect = Http.expectJson ArtistsRetrieved artistListDecoder
        }


artistDecoder : Decoder Artist
artistDecoder =
    map3 Artist
        (field "firstName" string)
        (field "lastName" string)
        (field "imageId" int)


artistListDecoder : Decoder (List Artist)
artistListDecoder =
    list artistDecoder
