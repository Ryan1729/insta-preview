port module Main exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, id, src, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Task


type alias FlagsValue =
    E.Value


type alias ModelValue =
    E.Value



--
-- PORTS
--


port saveImages : ModelValue -> Cmd msg



--
-- MAIN
--


main : Program FlagsValue Model Msg
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


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue flagsDecoder flags of
        Ok model ->
            model

        Err _ ->
            { profile = imageDefault
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


flagsDecoder : D.Decoder Model
flagsDecoder =
    let
        andMap : D.Decoder a -> D.Decoder (a -> value) -> D.Decoder value
        andMap =
            D.map2 (|>)

        imageStateDecoder : D.Decoder ImageState
        imageStateDecoder =
            -- TODO check that it looks like a URL?
            D.maybe D.string

        fieldFor : ImageLocation -> D.Decoder ImageState
        fieldFor location =
            D.oneOf
                [ D.field (locationKey location) imageStateDecoder
                , D.succeed imageDefault
                ]
    in
    D.succeed Model
        |> andMap (fieldFor Profile)
        |> andMap (fieldFor (Cell Zero))
        |> andMap (fieldFor (Cell One))
        |> andMap (fieldFor (Cell Two))
        |> andMap (fieldFor (Cell Three))
        |> andMap (fieldFor (Cell Four))
        |> andMap (fieldFor (Cell Five))
        |> andMap (fieldFor (Cell Six))
        |> andMap (fieldFor (Cell Seven))
        |> andMap (fieldFor (Cell Eight))
        |> andMap (fieldFor (Cell Nine))
        |> andMap (fieldFor (Cell Ten))
        |> andMap (fieldFor (Cell Eleven))


modelEncoder : Model -> E.Value
modelEncoder model =
    let
        extractPair : ImageLocation -> Maybe ( String, E.Value )
        extractPair location =
            getImage location model
                |> Maybe.map (\url -> ( locationKey location, E.string url ))
    in
    allLocations
        |> List.filterMap extractPair
        |> E.object



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


toInt index =
    case index of
        Zero ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Eleven ->
            11


saturatingInc index =
    case index of
        Zero ->
            One

        One ->
            Two

        Two ->
            Three

        Three ->
            Four

        Four ->
            Five

        Five ->
            Six

        Six ->
            Seven

        Seven ->
            Eight

        Eight ->
            Nine

        Nine ->
            Ten

        Ten ->
            Eleven

        Eleven ->
            Eleven


type ImageLocation
    = Profile
    | Cell ImageIndex


allLocations : List ImageLocation
allLocations =
    [ Profile
    , Cell Zero
    , Cell One
    , Cell Two
    , Cell Three
    , Cell Four
    , Cell Five
    , Cell Six
    , Cell Seven
    , Cell Eight
    , Cell Nine
    , Cell Ten
    , Cell Eleven
    ]


allLocationStrings =
    [ "profile"
    , "cell0"
    , "cell1"
    , "cell2"
    , "cell3"
    , "cell4"
    , "cell5"
    , "cell6"
    , "cell7"
    , "cell8"
    , "cell9"
    , "cell10"
    , "cell11"
    ]


type alias Key =
    String


locationKey : ImageLocation -> Key
locationKey location =
    case location of
        Profile ->
            "profile"

        Cell Zero ->
            "cell0"

        Cell One ->
            "cell1"

        Cell Two ->
            "cell2"

        Cell Three ->
            "cell3"

        Cell Four ->
            "cell4"

        Cell Five ->
            "cell5"

        Cell Six ->
            "cell6"

        Cell Seven ->
            "cell7"

        Cell Eight ->
            "cell8"

        Cell Nine ->
            "cell9"

        Cell Ten ->
            "cell10"

        Cell Eleven ->
            "cell11"


getImage : ImageLocation -> Model -> ImageState
getImage location model =
    case location of
        Profile ->
            model.profile

        Cell Zero ->
            model.cell0

        Cell One ->
            model.cell1

        Cell Two ->
            model.cell2

        Cell Three ->
            model.cell3

        Cell Four ->
            model.cell4

        Cell Five ->
            model.cell5

        Cell Six ->
            model.cell6

        Cell Seven ->
            model.cell7

        Cell Eight ->
            model.cell8

        Cell Nine ->
            model.cell9

        Cell Ten ->
            model.cell10

        Cell Eleven ->
            model.cell11


type ImageAction
    = Replace ImageLocation
    | ShiftDown ImageIndex


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
            let
                newModel =
                    case action of
                        Replace location ->
                            replace location url model

                        ShiftDown index ->
                            shiftDown index url model
            in
            ( newModel
            , modelEncoder newModel |> saveImages
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


copyDown : ImageIndex -> Model -> Model
copyDown index model =
    case index of
        Zero ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell1 = new.cell0 }

        One ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell2 = new.cell1 }

        Two ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell3 = new.cell2 }

        Three ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell4 = new.cell3 }

        Four ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell5 = new.cell4 }

        Five ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell6 = new.cell5 }

        Six ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell7 = new.cell6 }

        Seven ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell8 = new.cell7 }

        Eight ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell9 = new.cell8 }

        Nine ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell10 = new.cell9 }

        Ten ->
            let
                new =
                    copyDown (saturatingInc index) model
            in
            { new | cell11 = new.cell10 }

        Eleven ->
            model


shiftDown : ImageIndex -> ImageSource -> Model -> Model
shiftDown index source model =
    copyDown index model
        |> replace (Cell index) source



--
-- VIEW
--


view : Model -> Html Msg
view model =
    let
        cellView index =
            getImage (Cell index) model
                |> cell index
    in
    div
        [ absoluteContainerClass
        ]
        [ getImage Profile model |> profile
        , cellView Zero
        , cellView One
        , cellView Two
        , cellView Three
        , cellView Four
        , cellView Five
        , cellView Six
        , cellView Seven
        , cellView Eight
        , cellView Nine
        , cellView Ten
        , cellView Eleven
        ]


redPixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWO45+n5HwAF6QJwj+ULYQAAAABJRU5ErkJggg=="


greenPixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWMw2JD3HwAEsAJOci/TgAAAAABJRU5ErkJggg=="


bluePixel =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQImWMwDnr4HwAEiAJm7qe+oQAAAABJRU5ErkJggg=="


profile imageState =
    let
        profileSizeClass =
            class "insta-preview-profile"

        replaceButton =
            button
                [ absoluteClass
                , profileSizeClass
                , onClick (ImageRequested (Replace Profile))
                ]
                [ text "upload image" ]
    in
    div
        [ absoluteClass
        , profileSizeClass
        , overlayContainerClass
        ]
        (case imageState of
            Nothing ->
                [ replaceButton
                ]

            Just source ->
                [ img
                    [ src source
                    , absoluteClass
                    , profileSizeClass
                    ]
                    []
                , div
                    [ absoluteClass
                    , overlayClass
                    ]
                    [ replaceButton ]
                ]
        )


absoluteClass =
    class "absolute"


absoluteContainerClass =
    class "absolute-container"


cellClass =
    class "insta-preview-cell"


cellSizeClass =
    class "insta-preview-cell-size"


cellReplaceOnClick index =
    onClick (ImageRequested (Replace (Cell index)))


cellUploadButton index =
    button
        [ cellSizeClass
        , absoluteClass
        , cellReplaceOnClick index
        ]
        [ text "upload image" ]


overlayClass =
    class "overlay"


overlayContainerClass =
    class "overlay-container"


cellOverlay index =
    let
        cellOverlayButtonSizeClass =
            class "cell-overlay-button-size"
    in
    div
        [ absoluteClass
        , overlayClass
        ]
        [ button
            [ cellOverlayButtonSizeClass
            , absoluteClass
            , cellReplaceOnClick index
            ]
            [ text "replace image" ]
        , button
            [ cellOverlayButtonSizeClass
            , absoluteClass
            , onClick (ImageRequested (ShiftDown index))
            , class "cell-overlay-button1"
            ]
            [ text "shift down" ]
        ]


cell index imageState =
    let
        cellIndexClass =
            class ("insta-preview-cell" ++ String.fromInt (toInt index))
    in
    div
        [ absoluteClass
        , cellSizeClass
        , cellIndexClass
        , overlayContainerClass
        ]
        (case imageState of
            Nothing ->
                [ cellUploadButton index
                ]

            Just source ->
                [ img
                    [ src source
                    , absoluteClass
                    , cellSizeClass
                    ]
                    []
                , cellOverlay index
                ]
        )



--
-- SUBSCRIPTIONS
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
