module Main exposing (..)

import Html exposing (text)


n =
    "Hello world!"


main =
    let
        _ =
            Debug.log "number" n
    in
    text n
