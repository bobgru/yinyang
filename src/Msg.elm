module Msg exposing (..)

import Browser.Dom as Dom
import Location exposing (..)


type Msg
    = NoOp
    | WindowSizeChanged Int Int
    | ViewPortChanged (Result Dom.Error Dom.Viewport)
    | CellLeftClicked Location
    | CellRightClicked Location
    | CellHighlighted (Maybe Location)
