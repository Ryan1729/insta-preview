module Main exposing (main)

import Html exposing (div, img)
import Html.Attributes exposing (class, id, src, style)


redPixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWO45+n5HwAF6QJwj+ULYQAAAABJRU5ErkJggg=="


greenPixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWMw2JD3HwAEsAJOci/TgAAAAABJRU5ErkJggg=="


bluePixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWMwDnr4HwAEiAJm7qe+oQAAAABJRU5ErkJggg=="


profile source =
    img
        [ src source
        , class "profile"
        , style "width" "30em"
        , style "height" "20ex"
        ]
        []


cell source =
    img
        [ src source
        , style "height" "20ex"
        ]
        []


main =
    div
        [ id "insta-preview-grid"
        ]
        [ profile bluePixel
        , div
            [ class "cells"
            ]
            [ cell redPixel
            , cell greenPixel
            , cell bluePixel
            , cell redPixel
            , cell greenPixel
            , cell bluePixel
            , cell redPixel
            , cell greenPixel
            , cell bluePixel
            , cell redPixel
            , cell greenPixel
            , cell bluePixel
            ]
        ]
