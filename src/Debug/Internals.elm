module Debug.Internals exposing (Program, toInit, toUpdate, toView, view)

import Browser
import Html as H exposing (Html)
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
    | TimeTravel Int
    | ToggleAppModelVisible
    | ToggleAppMessagesVisible


type alias Program flags appModel appMsg =
    Platform.Program flags (Model appModel appMsg) (Msg appMsg)


toInit : appModel -> Model appModel appMsg
toInit appModel =
    { appModels = ZipList.singleton appModel
    , appMessages = []
    , isAppModelVisible = True
    , isAppMessagesVisible = True
    }


toUpdate : (appMsg -> appModel -> appModel) -> Msg appMsg -> Model appModel appMsg -> Model appModel appMsg
toUpdate updateApp msg model =
    case msg of
        UpdateApp appMsg ->
            let
                newAppModel =
                    updateApp appMsg model.appModels.current
            in
            { model
                | appModels = ZipList.dropHeads (ZipList.insert newAppModel model.appModels)
                , appMessages = appMsg :: model.appMessages
            }

        TimeTravel index ->
            { model | appModels = ZipList.toIndex index model.appModels }

        ToggleAppModelVisible ->
            { model | isAppModelVisible = not model.isAppModelVisible }

        ToggleAppMessagesVisible ->
            { model | isAppMessagesVisible = not model.isAppMessagesVisible }


viewCurrentAppModel : (appModel -> String) -> Bool -> appModel -> Html (Msg appMsg)
viewCurrentAppModel appModelToString isVisible appModel =
    H.div []
        [ H.text <|
            if isVisible then
                appModelToString appModel

            else
                ""
        ]


viewAppModelSlider : ZipList appModel -> Html (Msg appMsg)
viewAppModelSlider appModels =
    H.input
        [ Ha.type_ "range"
        , Ha.min "0"
        , Ha.max (String.fromInt (ZipList.length appModels - 1))
        , Ha.disabled (ZipList.length appModels == 1)
        , Ha.value (String.fromInt (List.length appModels.tails))
        , He.onInput (String.toInt >> Maybe.withDefault 0 >> TimeTravel)
        ]
        []


viewToggleButton : Msg appMsg -> String -> Bool -> Html (Msg appMsg)
viewToggleButton onClick label isSelected =
    H.button
        [ Ha.style "font-weight"
            (if isSelected then
                "bold"

             else
                "normal"
            )
        , He.onClick onClick
        ]
        [ H.text label ]


viewInitAppMessage appModelIndex =
    H.div
        [ He.onClick (TimeTravel 0)
        , Ha.style "background-color"
            (if appModelIndex == 0 then
                "lightgray"

             else
                "white"
            )
        ]
        [ H.text "Initial State", H.text "0" ]


viewAppMessage : Int -> Int -> appMsg -> Html (Msg appMsg)
viewAppMessage appModelIndex index appMessage =
    H.div
        [ He.onClick (TimeTravel index)
        , Ha.style "background-color"
            (if index == appModelIndex then
                "lightgray"

             else
                "white"
            )
        ]
        [ H.text (Debug.toString appMessage), H.text (String.fromInt index) ]


viewAppMessageList : (appMsg -> String) -> Bool -> Int -> List appMsg -> Html (Msg appMsg)
viewAppMessageList appMsgToString isVisible appModelIndex appMessages =
    if isVisible then
        H.div [] (viewInitAppMessage appModelIndex :: List.indexedMap (\index appMessage -> viewAppMessage appModelIndex (index + 1) appMessage) appMessages)

    else
        H.text ""


view : Model appModel appMsg -> Html (Msg appMsg)
view { appModels, appMessages, isAppModelVisible, isAppMessagesVisible } =
    H.div
        [ Ha.style "position" "fixed"
        , Ha.style "bottom" "0"
        , Ha.style "right" "0"
        ]
        [ viewAppMessageList Debug.toString isAppMessagesVisible (List.length appModels.tails) appMessages
        , viewCurrentAppModel Debug.toString isAppModelVisible appModels.current
        , viewAppModelSlider appModels
        , viewToggleButton ToggleAppModelVisible "model" isAppModelVisible
        , viewToggleButton ToggleAppMessagesVisible "msg" isAppMessagesVisible
        ]


toView : (appModel -> Html appMsg) -> Model appModel appMsg -> Html (Msg appMsg)
toView viewApp model =
    H.div []
        [ H.map UpdateApp (viewApp model.appModels.current)
        , view model
        ]
