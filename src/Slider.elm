module Slider exposing (DragInfo)


type alias DragInfo =
    { rangeStartValue : Float
    , thumbStartingPosition : Float
    , dragStartPosition : Float
    }
