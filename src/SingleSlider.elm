module SingleSlider exposing (Model, Msg, init, update, subscriptions, view)

{-| A single slider built natively in Elm


# Model

@docs Model


# Update

@docs Msg, update, subscriptions


# Configuring the slider

@docs init


# View

@docs view

-}

import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode exposing (map)
import DOM exposing (boundingClientRect)
import Mouse exposing (Position)
import Slider exposing (DragInfo, ValueChange(..))


type SliderStatus
    = Selected
    | Dragging DragInfo


{-| The base model for the slider
-}
type alias Model =
    { config : SliderConfig
    , value : Float
    , status : SliderStatus
    }


{-| The basic type accepted by the update
-}
type Msg
    = TrackClicked String
    | DragStart Position Float
    | DragAt Position
    | DragEnd Position


type alias SliderConfig =
    { min : Float, max : Float, step : Int, initialValue : Float }


{-| Returns a default range slider
-}
init : SliderConfig -> Model
init config =
    { config = config
    , value = config.initialValue
    , status = Selected
    }


{-| takes a model and a message and applies it to create an updated model
-}
update : Msg -> Model -> ( Model, ValueChange )
update message model =
    case message of
        TrackClicked newValue ->
            let
                convertedValue =
                    String.toFloat newValue
                        |> Result.withDefault 0
                        |> snapValue model.config

                change =
                    if model.value == convertedValue then
                        NoChange
                    else
                        Changed convertedValue
            in
                ( { model | value = convertedValue }, change )

        DragStart position offsetLeft ->
            ( { model
                | status =
                    Dragging
                        { rangeStartValue = model.value
                        , thumbStartingPosition = offsetLeft + 8
                        , dragStartPosition = (toFloat position.x)
                        }
              }
            , NoChange
            )

        DragAt position ->
            case model.status of
                Selected ->
                    ( model, NoChange )

                Dragging dragInfo ->
                    let
                        delta =
                            ((toFloat position.x) - dragInfo.dragStartPosition)

                        ratio =
                            (dragInfo.rangeStartValue / dragInfo.thumbStartingPosition)

                        newValue =
                            ((dragInfo.thumbStartingPosition + delta) * ratio)
                                |> snapValue model.config

                        change =
                            if newValue == model.value then
                                NoChange
                            else
                                Changed newValue
                    in
                        if newValue >= model.config.min && newValue <= model.config.max then
                            ( { model | value = newValue }, change )
                        else
                            ( model, NoChange )

        DragEnd position ->
            ( { model | status = Selected }, NoChange )


snapValue : SliderConfig -> Float -> Float
snapValue { step } value =
    toFloat (((round value) // step) * step)


onOutsideRangeClick : Model -> Json.Decode.Decoder Msg
onOutsideRangeClick model =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    toString (round ((model.config.max / rectangle.width) * mouseX))
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
        Json.Decode.map TrackClicked valueDecoder


onInsideRangeClick : Model -> Json.Decode.Decoder Msg
onInsideRangeClick model =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    toString (round ((model.value / rectangle.width) * mouseX))
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
        Json.Decode.map TrackClicked valueDecoder


onThumbMouseDown : Json.Decode.Decoder Msg
onThumbMouseDown =
    Json.Decode.map2
        DragStart
        Mouse.position
        (Json.Decode.at [ "target", "offsetLeft" ] Json.Decode.float)


{-| Displays the slider
-}
view : Model -> Html Msg
view model =
    let
        progress_ratio =
            100 / model.config.max

        thumbStartingPosition =
            toString (model.value * progress_ratio) ++ "%"

        progress =
            toString ((model.config.max - model.value) * progress_ratio) ++ "%"
    in
        div
            [ Html.Attributes.class "input-range-container" ]
            [ div
                [ Html.Attributes.class "slider-thumb slider-thumb--first"
                , Html.Attributes.style [ ( "left", thumbStartingPosition ) ]
                , Html.Events.onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } onThumbMouseDown
                ]
                []
            , div
                [ Html.Attributes.class "input-range__track"
                , Html.Attributes.style [ ( "z-index", "1" ) ]
                , Html.Events.on "click" (onOutsideRangeClick model)
                ]
                []
            , div
                [ Html.Attributes.class "input-range__progress"
                , Html.Attributes.style [ ( "left", "0" ), ( "right", progress ), ( "z-index", "1" ) ]
                , Html.Events.on "click" (onInsideRangeClick model)
                ]
                []
            ]



-- Subscriptions ---------------------------------------------------------------


{-| Returns the subscriptions necessary to run
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Dragging _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

        Selected ->
            Sub.none
