module DevTools.Browser exposing (application, document, element, sandbox)

import Browser
import Browser.Navigation
import DevTools.Main
import Html exposing (Html)
import Json.Decode as Jd
import Json.Encode as Je
import Url exposing (Url)


type alias SandboxConfig model msg =
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , devTools : DevTools.Main.Config model msg
    }


type alias ElementConfig model msg =
    { init : Jd.Value -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : DevTools.Main.Config model msg
    }


type alias DocumentConfig model msg =
    { init : Jd.Value -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , devTools : DevTools.Main.Config model msg
    }


type alias ApplicationConfig model msg =
    { init : Jd.Value -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    , devTools : DevTools.Main.Config model msg
    }


sandbox : SandboxConfig model msg -> DevTools.Main.Program Jd.Value model msg
sandbox { init, view, update, devTools } =
    Browser.document
        { init =
            \flags ->
                DevTools.Main.toInit
                    { update = \msg model -> ( update msg model, Cmd.none )
                    , msgDecoder = devTools.msgDecoder
                    , flags = flags
                    , model = init
                    , cmd = Cmd.none
                    }
        , view =
            DevTools.Main.toDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = \model -> { title = "", body = view model :: [] }
                }
        , update =
            DevTools.Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , output = devTools.output
                , update = \msg model -> ( update msg model, Cmd.none )
                }
        , subscriptions =
            DevTools.Main.toSubscriptions
                { subscriptions = always Sub.none
                , msgDecoder = devTools.msgDecoder
                }
        }


element : ElementConfig model msg -> DevTools.Main.Program Jd.Value model msg
element { init, view, update, subscriptions, devTools } =
    Browser.element
        { init =
            \flags ->
                let
                    ( model, cmd ) =
                        init flags
                in
                DevTools.Main.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , flags = flags
                    , model = model
                    , cmd = cmd
                    }
        , view =
            DevTools.Main.toHtml
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                }
        , update =
            DevTools.Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , output = devTools.output
                , update = update
                }
        , subscriptions =
            DevTools.Main.toSubscriptions
                { msgDecoder = devTools.msgDecoder
                , subscriptions = subscriptions
                }
        }


document : DocumentConfig model msg -> DevTools.Main.Program Jd.Value model msg
document { init, view, update, subscriptions, devTools } =
    Browser.document
        { init =
            \flags ->
                let
                    ( model, cmd ) =
                        init flags
                in
                DevTools.Main.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , flags = flags
                    , model = model
                    , cmd = cmd
                    }
        , view =
            DevTools.Main.toDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                }
        , update =
            DevTools.Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , output = devTools.output
                }
        , subscriptions =
            DevTools.Main.toSubscriptions
                { subscriptions = subscriptions
                , msgDecoder = devTools.msgDecoder
                }
        }


application : ApplicationConfig model msg -> DevTools.Main.Program Jd.Value model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange, devTools } =
    Browser.application
        { init =
            \flags url key ->
                let
                    ( model, cmd ) =
                        init flags url key
                in
                DevTools.Main.toInit
                    { update = update
                    , msgDecoder = devTools.msgDecoder
                    , flags = flags
                    , model = model
                    , cmd = cmd
                    }
        , view =
            DevTools.Main.toDocument
                { printModel = devTools.printModel
                , encodeMsg = devTools.encodeMsg
                , view = view
                }
        , update =
            DevTools.Main.toUpdate
                { msgDecoder = devTools.msgDecoder
                , encodeMsg = devTools.encodeMsg
                , update = update
                , output = devTools.output
                }
        , subscriptions =
            DevTools.Main.toSubscriptions
                { subscriptions = subscriptions
                , msgDecoder = devTools.msgDecoder
                }
        , onUrlChange = DevTools.Main.toMsg << onUrlChange
        , onUrlRequest = DevTools.Main.toMsg << onUrlRequest
        }
