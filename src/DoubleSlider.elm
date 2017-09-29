module DoubleSlider exposing (Model, Msg, init, update, subscriptions, view, fallbackView, formatCurrentValue)

{-| A single slider built natively in Elm


# Model

@docs Model


# Update

@docs Msg, update, subscriptions


# Configuring the slider

@docs init


# View

@docs view, fallbackView, formatCurrentValue

-}

import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode exposing (map)
import DOM exposing (boundingClientRect)
import Mouse exposing (Position)
import Slider exposing (DragInfo)


type alias SliderConfig =
    { min : Float
    , max : Float
    , step : Int
    , lowValue : Float
    , highValue : Float
    , formatter : Float -> String
    }


{-| The base model for the slider
-}
type alias Model =
    { config : SliderConfig
    , lowValue : Float
    , highValue : Float
    , status : SliderStatus
    }


type SliderStatus
    = Selected
    | Dragging SliderValueType DragInfo


type SliderValueType
    = LowValue
    | HighValue


{-| The basic type accepted by the update
-}
type Msg
    = TrackClicked SliderValueType String
    | DragStart SliderValueType Position Float
    | DragAt Position
    | DragEnd Position
    | RangeChanged SliderValueType String Bool


{-| Returns a default range slider
-}
init : SliderConfig -> Model
init config =
    { config = config
    , lowValue = config.lowValue
    , highValue = config.highValue
    , status = Selected
    }


{-| takes a model and a message and applies it to create an updated model
-}
update : Msg -> Model -> Model
update message model =
    case message of
        RangeChanged valueType newValue shouldFetchModels ->
            let
                convertedValue =
                    String.toFloat newValue |> Result.toMaybe |> Maybe.withDefault 0
            in
                case valueType of
                    LowValue ->
                        { model | lowValue = convertedValue }

                    HighValue ->
                        { model | highValue = convertedValue }

        TrackClicked valueType newValue ->
            let
                convertedValue =
                    String.toFloat newValue |> Result.toMaybe |> Maybe.withDefault 0
            in
                case valueType of
                    LowValue ->
                        { model | lowValue = convertedValue }

                    HighValue ->
                        { model | highValue = convertedValue }

        DragStart valueType position offsetLeft ->
            { model
                | status =
                    Dragging valueType
                        { rangeStartValue =
                            case valueType of
                                LowValue ->
                                    model.lowValue

                                HighValue ->
                                    model.highValue
                        , thumbStartingPosition = offsetLeft + 16
                        , dragStartPosition = (toFloat position.x)
                        }
            }

        DragAt position ->
            case model.status of
                Selected ->
                    model

                Dragging valueType dragInfo ->
                    let
                        delta =
                            ((toFloat position.x) - dragInfo.dragStartPosition)

                        ratio =
                            (dragInfo.rangeStartValue / dragInfo.thumbStartingPosition)

                        newValue =
                            snapValue model.config ((dragInfo.thumbStartingPosition + delta) * ratio)
                    in
                        if newValue >= model.config.min && newValue <= model.config.max then
                            case valueType of
                                LowValue ->
                                    { model | lowValue = newValue }

                                HighValue ->
                                    { model | highValue = newValue }
                        else
                            model

        DragEnd position ->
            { model | status = Selected }


{-| renders the current values using the formatter
-}
formatCurrentValue : Model -> String
formatCurrentValue model =
    if model.lowValue == model.config.min && model.highValue == model.config.max then
        ""
    else
        (model.config.formatter model.lowValue) ++ " - " ++ (model.config.formatter model.highValue)


snapValue : SliderConfig -> Float -> Float
snapValue { step } value =
    toFloat (((round value) // step) * step)


onOutsideRangeClick : Model -> Json.Decode.Decoder Msg
onOutsideRangeClick model =
    let
        valueTypeDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        newValue =
                            snapValue model.config ((model.config.max / rectangle.width) * mouseX)

                        valueType =
                            if newValue < model.lowValue then
                                LowValue
                            else
                                HighValue
                    in
                        valueType
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)

        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    toString (round ((model.config.max / rectangle.width) * mouseX))
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
        Json.Decode.map2 TrackClicked valueTypeDecoder valueDecoder


onInsideRangeClick : Model -> Json.Decode.Decoder Msg
onInsideRangeClick model =
    let
        valueTypeDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        centerThreshold =
                            rectangle.width / 2

                        valueType =
                            if mouseX < centerThreshold then
                                LowValue
                            else
                                HighValue
                    in
                        valueType
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)

        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        centerThreshold =
                            rectangle.width / 2

                        refValue =
                            model.highValue - model.lowValue

                        newValue =
                            snapValue model.config ((((model.highValue - model.lowValue) / rectangle.width) * mouseX) + model.lowValue)
                    in
                        toString (round newValue)
                )
                (Json.Decode.at [ "target" ] boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
        Json.Decode.map2 TrackClicked valueTypeDecoder valueDecoder


onThumbMouseDown : SliderValueType -> Json.Decode.Decoder Msg
onThumbMouseDown valueType =
    Json.Decode.map3
        DragStart
        (Json.Decode.succeed valueType)
        Mouse.position
        (Json.Decode.at [ "target", "offsetLeft" ] Json.Decode.float)


onRangeChange : SliderValueType -> Bool -> Json.Decode.Decoder Msg
onRangeChange valueType shouldFetchModels =
    Json.Decode.map3
        RangeChanged
        (Json.Decode.succeed valueType)
        targetValue
        (Json.Decode.succeed shouldFetchModels)


{-| Displays the slider using two inputs
-}
fallbackView : Model -> Html Msg
fallbackView model =
    let
        lowValue =
            round model.lowValue

        highValue =
            round model.highValue

        progressRatio =
            100 / model.config.max

        progressLow =
            toString (model.lowValue * progressRatio) ++ "%"

        progressHigh =
            toString ((model.config.max - model.highValue) * progressRatio) ++ "%"
    in
        div []
            [ div
                [ Html.Attributes.class "input-range-container" ]
                [ Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min (toString model.config.min)
                    , Html.Attributes.max (toString model.config.max)
                    , Html.Attributes.value <| (toString model.lowValue)
                    , Html.Attributes.step (toString model.config.step)
                    , Html.Attributes.class "input-range input-range--first"
                    , Html.Events.on "change" (onRangeChange LowValue True)
                    , Html.Events.on "input" (onRangeChange LowValue False)
                    ]
                    []
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min (toString model.config.min)
                    , Html.Attributes.max (toString model.config.max)
                    , Html.Attributes.value <| (toString model.highValue)
                    , Html.Attributes.step (toString model.config.step)
                    , Html.Attributes.class "input-range input-range--second"
                    , Html.Events.on "change" (onRangeChange HighValue True)
                    , Html.Events.on "input" (onRangeChange HighValue False)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__track"
                    , Html.Events.on "click" (onOutsideRangeClick model)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__progress"
                    , Html.Attributes.style [ ( "left", progressLow ), ( "right", progressHigh ) ]
                    , Html.Events.on "click" (onInsideRangeClick model)
                    ]
                    []
                ]
            , div
                [ Html.Attributes.class "input-range-labels-container" ]
                [ div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.config.formatter model.config.min) ]
                , div [ Html.Attributes.class "input-range-label input-range-label--current-value" ] [ Html.text (formatCurrentValue model) ]
                , div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.config.formatter model.config.max) ]
                ]
            ]


{-| Displays the slider
-}
view : Model -> Html Msg
view model =
    let
        lowValue =
            round model.lowValue

        highValue =
            round model.highValue

        progressRatio =
            100 / model.config.max

        lowThumbStartingPosition =
            toString (model.lowValue * progressRatio) ++ "%"

        highThumbStartingPosition =
            toString (model.highValue * progressRatio) ++ "%"

        progressLow =
            toString (model.lowValue * progressRatio) ++ "%"

        progressHigh =
            toString ((model.config.max - model.highValue) * progressRatio) ++ "%"
    in
        div []
            [ div
                [ Html.Attributes.class "input-range-container" ]
                [ div
                    [ Html.Attributes.class "slider-thumb slider-thumb--first"
                    , Html.Attributes.style [ ( "left", lowThumbStartingPosition ), ( "float", "left" ) ]
                    , Html.Events.onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (onThumbMouseDown LowValue)
                    ]
                    []
                , div
                    [ Html.Attributes.class "slider-thumb slider-thumb--second"
                    , Html.Attributes.style [ ( "left", highThumbStartingPosition ), ( "float", "left" ) ]
                    , Html.Events.onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (onThumbMouseDown HighValue)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__track"
                    , Html.Events.on "click" (onOutsideRangeClick model)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__progress"
                    , Html.Attributes.style [ ( "left", progressLow ), ( "right", progressHigh ) ]
                    , Html.Events.on "click" (onInsideRangeClick model)
                    ]
                    []
                ]
            , div
                [ Html.Attributes.class "input-range-labels-container" ]
                [ div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.config.formatter model.config.min) ]
                , div [ Html.Attributes.class "input-range-label" ] [ Html.text (model.config.formatter model.config.max) ]
                ]
            ]


{-| Returns the subscriptions necessary to run
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Selected ->
            Sub.none

        Dragging _ _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
