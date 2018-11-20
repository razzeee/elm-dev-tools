module Debug.Browser exposing (application, document, element, sandbox)

import Browser
import Browser.Navigation
import Debug.Main
import Html exposing (Html)
import Url exposing (Url)


type alias SandboxConfig model msg =
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , debug : Debug.Main.Config model msg
    }


type alias ElementConfig flags model msg =
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , debug : Debug.Main.Config model msg
    }


type alias DocumentConfig flags model msg =
    { init : flags -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , debug : Debug.Main.Config model msg
    }


type alias ApplicationConfig flags model msg =
    { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    , debug : Debug.Main.Config model msg
    }


sandbox : SandboxConfig model msg -> Debug.Main.Program () model msg
sandbox { init, view, update, debug } =
    Browser.document
        { init =
            always (Debug.Main.toInit ( init, Cmd.none ))
        , view =
            Debug.Main.toDocument
                { printModel = debug.printModel
                , printMessage = debug.printMessage
                , commands = debug.commands
                , view = \model -> { title = "Elm Debug", body = view model :: [] }
                }
        , update =
            Debug.Main.toUpdate
                { messageDecoder = debug.messageDecoder
                , encodeMessage = debug.encodeMessage
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , subscriptions = Debug.Main.toSubscriptions (always Sub.none)
        }


element : ElementConfig flags model msg -> Debug.Main.Program flags model msg
element { init, view, update, subscriptions, debug } =
    Browser.element
        { init = Debug.Main.toInit << init
        , view =
            Debug.Main.toHtml
                { printModel = debug.printModel
                , printMessage = debug.printMessage
                , commands = debug.commands
                , view = view
                }
        , update =
            Debug.Main.toUpdate
                { messageDecoder = debug.messageDecoder
                , encodeMessage = debug.encodeMessage
                , update = update
                }
        , subscriptions = Debug.Main.toSubscriptions subscriptions
        }


document : DocumentConfig flags model msg -> Debug.Main.Program flags model msg
document { init, view, update, subscriptions, debug } =
    Browser.document
        { init = Debug.Main.toInit << init
        , view =
            Debug.Main.toDocument
                { printModel = debug.printModel
                , printMessage = debug.printMessage
                , commands = debug.commands
                , view = view
                }
        , update =
            Debug.Main.toUpdate
                { messageDecoder = debug.messageDecoder
                , encodeMessage = debug.encodeMessage
                , update = update
                }
        , subscriptions = Debug.Main.toSubscriptions subscriptions
        }


application : ApplicationConfig flags model msg -> Debug.Main.Program flags model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange, debug } =
    Browser.application
        { init = \flags url key -> Debug.Main.toInit (init flags url key)
        , view =
            Debug.Main.toDocument
                { printModel = debug.printModel
                , printMessage = debug.printMessage
                , commands = debug.commands
                , view = view
                }
        , update =
            Debug.Main.toUpdate
                { messageDecoder = debug.messageDecoder
                , encodeMessage = debug.encodeMessage
                , update = update
                }
        , subscriptions = Debug.Main.toSubscriptions subscriptions
        , onUrlChange = Debug.Main.toMsg << onUrlChange
        , onUrlRequest = Debug.Main.toMsg << onUrlRequest
        }
