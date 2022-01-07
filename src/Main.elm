module Main exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, id, src, style)
import Html.Events exposing (onClick)
import Task


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


grid =
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



--
-- MAIN
--


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--
-- MODEL
--


type alias Model =
    { image : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing, Cmd.none )



--
-- UPDATE
--


type Msg
    = ImageRequested
    | ImageSelected File
    | ImageUrl String



-- The MIME types for the image types listed at:
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img#supported_image_formats


imageTypes =
    [ "image/apng"
    , "image/avif"
    , "image/gif"
    , "image/jpeg"
    , "image/png"
    , "image/svg+xml"
    , "image/webp"
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageRequested ->
            ( model
            , Select.file imageTypes ImageSelected
            )

        ImageSelected file ->
            ( model
            , Task.perform ImageUrl (File.toUrl file)
            )

        ImageUrl url ->
            ( { model | image = Just url }
            , Cmd.none
            )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    div []
        [ grid
        , case model.image of
            Nothing ->
                button [ onClick ImageRequested ] [ text "upload image" ]

            Just url ->
                div []
                    [ img
                        [ src url
                        , style
                            "height"
                            "20ex"
                        ]
                        []
                    , p [ style "white-space" "pre" ] [ text url ]
                    ]
        ]



--
-- SUBSCRIPTIONS
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
