module Browser.Config exposing (Init, Subscriptions, Update, View)

import Browser
import Browser.Navigation
import Url exposing (Url)


type alias Init model msg flags =
    flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )


type alias View model msg =
    model -> Browser.Document msg


type alias Update model msg =
    msg -> model -> ( model, Cmd msg )


type alias Subscriptions model msg =
    model -> Sub msg
