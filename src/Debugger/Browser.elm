module Debugger.Browser exposing (sandbox)

import Browser
import Debugger
import Html exposing (Html)


sandbox :
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    }
    -> Debugger.Program () model msg
sandbox { init, update, view } =
    Browser.sandbox
        { init = Debugger.toInit init
        , update = Debugger.toUpdate update
        , view = Debugger.toView view
        }
