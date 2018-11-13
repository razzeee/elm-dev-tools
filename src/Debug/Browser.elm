module Debug.Browser exposing (application, document, element, sandbox)

import Browser
import Browser.Navigation
import Debug.Internals
import Html exposing (Html)
import Url exposing (Url)


sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , debug : Debug.Internals.Configuration model msg
    }
    -> Debug.Internals.Program () model msg
sandbox { init, view, update, debug } =
    Browser.document
        { init =
            always (Debug.Internals.toInit ( init, Cmd.none ))
        , view =
            Debug.Internals.toDocument
                { modelToString = debug.modelToString
                , msgToString = debug.msgToString
                , labelMsgsPairs = debug.labelMsgsPairs
                , encodeMsg = debug.encodeMsg
                , view = \model -> { title = "Debug", body = view model :: [] }
                }
        , update =
            Debug.Internals.toUpdate
                { msgDecoder = debug.msgDecoder
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , subscriptions =
            Debug.Internals.toSubscriptions (always Sub.none)
        }


element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , debug : Debug.Internals.Configuration model msg
    }
    -> Debug.Internals.Program flags model msg
element { init, view, update, subscriptions, debug } =
    Browser.element
        { init =
            Debug.Internals.toInit << init
        , view =
            Debug.Internals.toHtml
                { modelToString = debug.modelToString
                , msgToString = debug.msgToString
                , labelMsgsPairs = debug.labelMsgsPairs
                , encodeMsg = debug.encodeMsg
                , view = view
                }
        , update =
            Debug.Internals.toUpdate
                { msgDecoder = debug.msgDecoder
                , update = update
                }
        , subscriptions =
            Debug.Internals.toSubscriptions subscriptions
        }


document :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , debug : Debug.Internals.Configuration model msg
    }
    -> Debug.Internals.Program flags model msg
document { init, view, update, subscriptions, debug } =
    Browser.document
        { init =
            Debug.Internals.toInit << init
        , view =
            Debug.Internals.toDocument
                { modelToString = debug.modelToString
                , msgToString = debug.msgToString
                , labelMsgsPairs = debug.labelMsgsPairs
                , encodeMsg = debug.encodeMsg
                , view = view
                }
        , update =
            Debug.Internals.toUpdate
                { msgDecoder = debug.msgDecoder
                , update = update
                }
        , subscriptions =
            Debug.Internals.toSubscriptions subscriptions
        }


application :
    { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    , debug : Debug.Internals.Configuration model msg
    }
    -> Debug.Internals.Program flags model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange, debug } =
    Browser.application
        { init =
            \flags url key -> Debug.Internals.toInit (init flags url key)
        , view =
            Debug.Internals.toDocument
                { modelToString = debug.modelToString
                , msgToString = debug.msgToString
                , labelMsgsPairs = debug.labelMsgsPairs
                , encodeMsg = debug.encodeMsg
                , view = view
                }
        , update =
            Debug.Internals.toUpdate
                { msgDecoder = debug.msgDecoder
                , update = update
                }
        , subscriptions =
            Debug.Internals.toSubscriptions subscriptions
        , onUrlChange =
            Debug.Internals.toMsg << onUrlChange
        , onUrlRequest =
            Debug.Internals.toMsg << onUrlRequest
        }
