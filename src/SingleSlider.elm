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


{-| The base model for the slider
-}
type alias Model =
    { min : Float
    , max : Float
    , step : Float
    , value : Float
    }


{-| The basic type accepted by the update
-}
type Msg
    = TrackClicked String
    | RangeChanged String Bool


{-| Returns a default range slider
-}
init : { min : Float, max : Float, step : Float, value : Float } -> Model
init config =
    { min = config.min
    , max = config.max
    , step = config.step
    , value = config.value
    }


{-| takes a model and a message and applies it to create an updated model
-}
update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update message model =
    case message of
        RangeChanged newValue shouldFetchModels ->
            let
                convertedValue =
                    String.toFloat newValue |> Result.toMaybe |> Maybe.withDefault 0

                newModel =
                    { model | value = convertedValue }
            in
                ( newModel, Cmd.none, shouldFetchModels )

        TrackClicked newValue ->
            let
                convertedValue =
                    snapValue (String.toFloat newValue |> Result.toMaybe |> Maybe.withDefault 0) model.step

                newModel =
                    { model | value = convertedValue }
            in
                ( newModel, Cmd.none, True )


snapValue : Float -> Float -> Float
snapValue value step =
    let
        roundedStep =
            round step
    in
        toFloat (((round value) // roundedStep) * roundedStep)


onOutsideRangeClick : Model -> Json.Decode.Decoder Msg
onOutsideRangeClick model =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    toString (round ((model.max / rectangle.width) * mouseX))
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


onRangeChange : Bool -> Json.Decode.Decoder Msg
onRangeChange shouldFetchModels =
    Json.Decode.map2
        RangeChanged
        targetValue
        (Json.Decode.succeed shouldFetchModels)


{-| Displays the slider
-}
view : Model -> Html Msg
view model =
    let
        progress_ratio =
            100 / (model.max - model.min)

        progress =
            toString ((model.max - model.value) * progress_ratio) ++ "%"
    in
        div []
            [ div
                [ Html.Attributes.class "input-range-container" ]
                [ Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min (toString model.min)
                    , Html.Attributes.max (toString model.max)
                    , Html.Attributes.value <| (toString model.value)
                    , Html.Attributes.step (toString model.step)
                    , Html.Attributes.class "input-range"
                    , Html.Events.on "change" (onRangeChange True)
                    , Html.Events.on "input" (onRangeChange False)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__track"
                    , Html.Events.on "click" (onOutsideRangeClick model)
                    ]
                    []
                , div
                    [ Html.Attributes.class "input-range__progress"
                    , Html.Attributes.style [ ( "left", "0" ), ( "right", progress ) ]
                    , Html.Events.on "click" (onInsideRangeClick model)
                    ]
                    []
                ]
            , div
                [ Html.Attributes.class "input-range-labels-container" ]
                [ div [ Html.Attributes.class "input-range-label" ] [ Html.text (toString model.min) ]
                , div [ Html.Attributes.class "input-range-label input-range-label--current-value" ] [ Html.text (toString model.value) ]
                , div [ Html.Attributes.class "input-range-label" ] [ Html.text (toString model.max) ]
                ]
            ]



-- Subscriptions ---------------------------------------------------------------


{-| Returns the subscriptions necessary to run
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
