module Slider exposing (DragInfo, ValueChange(..))


type ValueChange
    = NoChange
    | Changed Float


type alias DragInfo =
    { rangeStartValue : Float
    , thumbStartingPosition : Float
    , dragStartPosition : Float
    }
