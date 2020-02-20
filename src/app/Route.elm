module Route exposing (Route(..), parser)

import Url.Parser as Parser exposing ((</>), Parser)

type Route
    = HomeRoute
    | ArtistRoute String
    | LyricRoute String String
    | NotFoundRoute

parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map ArtistRoute (Parser.s "artists" </> Parser.string </> Parser.s "lyrics")
        , Parser.map LyricRoute (Parser.s "artists" </> Parser.string </> Parser.s "lyrics" </> Parser.string)
        ]