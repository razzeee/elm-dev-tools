module Debugger exposing (Model, Msg, toInit, toUpdate, toView, view)

import Browser
import Html exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import ZipList exposing (ZipList)


type alias Model appModel appMsg =
    { appModels : ZipList appModel
    , appMessages : List appMsg
    , isAppModelVisible : Bool
    , isAppMessagesVisible : Bool
    }


type Msg appMsg
    = UpdateApp appMsg


toInit : appModel -> Model appModel appMsg
toInit appModel =
    { appModels = ZipList.singleton appModel
    , appMessages = []
    , isAppModelVisible = False
    , isAppMessagesVisible = False
    }


toUpdate : (appMsg -> appModel -> appModel) -> Msg appMsg -> Model appModel appMsg -> Model appModel appMsg
toUpdate updateApp msg model =
    case msg of
        UpdateApp appMsg ->
            let
                newAppModel =
                    updateApp appMsg model.appModels.current
            in
            { model | appModels = ZipList.prepend newAppModel model.appModels }


view : Model appModel appMsg -> Html (Msg appMsg)
view { appModels, appMessages, isAppModelVisible, isAppMessagesVisible } =
    Html.div [] []


toView : (appModel -> Html appMsg) -> Model appModel appMsg -> Html (Msg appMsg)
toView viewApp model =
    Html.div []
        [ Html.map UpdateApp (viewApp model.appModels.current)
        , view model
        ]
