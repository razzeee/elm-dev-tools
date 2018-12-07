module Develop exposing (main)

import Browser
import Develop.Main exposing (toInit, toMsg, toSubscriptions, toUpdate, toView)
import Main
    exposing
        ( encodeMsg
        , init
        , msgDecoder
        , onUrlChange
        , onUrlRequest
        , subscriptions
        , update
        , view
        )


main =
    Browser.application
        { init = toInit init
        , onUrlChange = toMsg << onUrlChange
        , onUrlRequest = toMsg << onUrlRequest
        , update = toUpdate update
        , view = toView view
        , subscriptions = toSubscriptions subscriptions
        }
