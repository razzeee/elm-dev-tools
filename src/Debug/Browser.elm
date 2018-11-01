module Debug.Browser exposing (sandbox)

import Browser
import Debug.Internals
import Html exposing (Html)


sandbox :
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    }
    -> Debug.Internals.Program () model msg
sandbox { init, update, view } =
    Browser.sandbox
        { init = Debug.Internals.toInit init
        , update = Debug.Internals.toUpdate update
        , view = Debug.Internals.toView view
        }
