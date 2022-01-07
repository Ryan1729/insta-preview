module Main exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, id, src, style)
import Html.Events exposing (onClick)
import Task



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


type alias ImageState =
    Maybe ImageSource


imageDefault : ImageState
imageDefault =
    Nothing



-- A URL


type alias ImageSource =
    String


type alias Model =
    { profile : ImageState
    , cell0 : ImageState
    , cell1 : ImageState
    , cell2 : ImageState
    , cell3 : ImageState
    , cell4 : ImageState
    , cell5 : ImageState
    , cell6 : ImageState
    , cell7 : ImageState
    , cell8 : ImageState
    , cell9 : ImageState
    , cell10 : ImageState
    , cell11 : ImageState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { profile = imageDefault
      , cell0 = imageDefault
      , cell1 = imageDefault
      , cell2 = imageDefault
      , cell3 = imageDefault
      , cell4 = imageDefault
      , cell5 = imageDefault
      , cell6 = imageDefault
      , cell7 = imageDefault
      , cell8 = imageDefault
      , cell9 = imageDefault
      , cell10 = imageDefault
      , cell11 = imageDefault
      }
    , Cmd.none
    )



--
-- UPDATE
--


type ImageIndex
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Eleven


type ImageLocation
    = Profile
    | Cell ImageIndex



-- TODO `ShiftDown ImageIndex`


type ImageAction
    = Replace ImageLocation


type Msg
    = ImageRequested ImageAction
    | ImageSelected ImageAction File
    | ImageUrl ImageAction ImageSource



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
        ImageRequested action ->
            ( model
            , Select.file imageTypes (ImageSelected action)
            )

        ImageSelected action file ->
            ( model
            , Task.perform (ImageUrl action) (File.toUrl file)
            )

        ImageUrl action url ->
            ( case action of
                Replace location ->
                    replace location url model
            , Cmd.none
            )


replace : ImageLocation -> ImageSource -> Model -> Model
replace location source model =
    case location of
        Profile ->
            { model | profile = Just source }

        Cell Zero ->
            { model | cell0 = Just source }

        Cell One ->
            { model | cell1 = Just source }

        Cell Two ->
            { model | cell2 = Just source }

        Cell Three ->
            { model | cell3 = Just source }

        Cell Four ->
            { model | cell4 = Just source }

        Cell Five ->
            { model | cell5 = Just source }

        Cell Six ->
            { model | cell6 = Just source }

        Cell Seven ->
            { model | cell7 = Just source }

        Cell Eight ->
            { model | cell8 = Just source }

        Cell Nine ->
            { model | cell9 = Just source }

        Cell Ten ->
            { model | cell10 = Just source }

        Cell Eleven ->
            { model | cell11 = Just source }



--
-- VIEW
--


view : Model -> Html Msg
view model =
    div
        [ id "insta-preview-grid"
        ]
        [ profile model.profile
        , div
            [ class "cells"
            ]
            [ cell Zero model.cell0
            , cell One model.cell1
            , cell Two model.cell2
            , cell Three model.cell3
            , cell Four model.cell4
            , cell Five model.cell5
            , cell Six model.cell6
            , cell Seven model.cell7
            , cell Eight model.cell8
            , cell Nine model.cell9
            , cell Ten model.cell10
            , cell Eleven model.cell11
            ]
        ]


redPixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWO45+n5HwAF6QJwj+ULYQAAAABJRU5ErkJggg=="


greenPixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWMw2JD3HwAEsAJOci/TgAAAAABJRU5ErkJggg=="


bluePixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWMwDnr4HwAEiAJm7qe+oQAAAABJRU5ErkJggg=="


profileWidth =
    style "width" "30rem"


profileHeight =
    cellHeight


cellWidth =
    style "width" "10rem"


cellHeight =
    style "height" "10rem"


profile imageState =
    case imageState of
        Nothing ->
            button
                [ profileWidth
                , profileHeight
                , onClick (ImageRequested (Replace Profile))
                ]
                [ text "upload image" ]

        Just source ->
            img
                [ src source
                , class "profile"
                , profileWidth
                , profileHeight
                ]
                []


cell index imageState =
    case imageState of
        Nothing ->
            button
                [ cellWidth
                , cellHeight
                , onClick (ImageRequested (Replace (Cell index)))
                ]
                [ text "upload image" ]

        Just source ->
            img
                [ src source
                , cellWidth
                , cellHeight
                ]
                []



--
-- SUBSCRIPTIONS
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
