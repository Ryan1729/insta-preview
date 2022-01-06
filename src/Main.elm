module Main exposing (main)

import Html exposing (div, img)
import Html.Attributes exposing (src, style)


redPixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWO45+n5HwAF6QJwj+ULYQAAAABJRU5ErkJggg=="


greenPixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWMw2JD3HwAEsAJOci/TgAAAAABJRU5ErkJggg=="


bluePixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWMwDnr4HwAEiAJm7qe+oQAAAABJRU5ErkJggg=="


image source =
    img
        [ src source
        , style "height" "20ex"
        , style "width" "10em"
        ]
        []


main =
    div
        []
        [ image redPixel
        , image greenPixel
        , image bluePixel
        ]
