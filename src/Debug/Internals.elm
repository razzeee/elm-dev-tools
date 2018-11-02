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
            { model
                | appModels = ZipList.dropHeads (ZipList.insert newAppModel model.appModels)
                , appMessages = appMsg :: List.drop (List.length model.appModels.heads) model.appMessages
            }

        TimeTravel index ->
            { model | appModels = ZipList.toIndex index model.appModels }

        ToggleAppModelVisible ->
            { model | isAppModelVisible = not model.isAppModelVisible }

        ToggleAppMessagesVisible ->
            { model | isAppMessagesVisible = not model.isAppMessagesVisible }


viewCurrentAppModel : (appModel -> String) -> Bool -> appModel -> Html (Msg appMsg)
viewCurrentAppModel appModelToString isVisible appModel =
    if isVisible then
        H.div
            [ Ha.style "padding" "4px"
            , Ha.style "border-bottom" "1px solid #eeeeee"
            ]
            [ H.text (appModelToString appModel) ]

    else
        H.text ""


viewAppModelSlider : ZipList appModel -> Html (Msg appMsg)
viewAppModelSlider appModels =
    H.div
        [ Ha.style "padding" "4px"
        , Ha.style "border-bottom" "1px solid #eeee"
        ]
        [ H.input
            [ Ha.type_ "range"
            , Ha.min "0"
            , Ha.max (String.fromInt (ZipList.length appModels - 1))
            , Ha.disabled (ZipList.length appModels == 1)
            , Ha.value (String.fromInt (List.length appModels.tails))
            , He.onInput (String.toInt >> Maybe.withDefault 0 >> TimeTravel)
            ]
            []
        ]


viewToggleButton : Msg appMsg -> String -> Bool -> Html (Msg appMsg)
viewToggleButton onClick label isSelected =
    H.div
        [ Ha.style "cursor" "pointer"
        , Ha.style "border-right" "1px solid #eeee"
        , Ha.style "padding" "4px"
        , Ha.style "width" "50%"
        , Ha.style "background-color"
            (if isSelected then
                "#60B5CC"

             else
                "white"
            )
        , Ha.style "color"
            (if isSelected then
                "white"

             else
                "black"
            )
        , He.onClick onClick
        ]
        [ H.text label ]


viewInitAppMessage : Int -> Html (Msg appMsg)
viewInitAppMessage appModelIndex =
    H.div
        [ He.onClick (TimeTravel 0)
        , Ha.style "cursor" "pointer"
        , Ha.style "padding" "0px 4px"
        , Ha.style "border-bottom" "1px solid #eeeeee"
        , Ha.style "background-color"
            (if appModelIndex == 0 then
                "#60B5CC"

             else
                "white"
            )
        , Ha.style "color"
            (if appModelIndex == 0 then
                "white"

             else
                "black"
            )
        ]
        [ H.span [] [ H.text "Init" ]
        , H.span [ Ha.style "float" "right" ] [ H.text "0" ]
        ]


viewAppMessage : Int -> Int -> appMsg -> Html (Msg appMsg)
viewAppMessage appModelIndex index appMessage =
    H.div
        [ He.onClick (TimeTravel index)
        , Ha.style "cursor" "pointer"
        , Ha.style "padding" "0px 4px"
        , Ha.style "border-bottom" "1px solid #eeeeee"
        , Ha.style "background-color"
            (if index == appModelIndex then
                "#60B5CC"

             else
                "white"
            )
        , Ha.style "color"
            (if index == appModelIndex then
                "white"

             else
                "black"
            )
        ]
        [ H.span [] [ H.text (Debug.toString appMessage) ]
        , H.span [ Ha.style "float" "right" ] [ H.text (String.fromInt index) ]
        ]


viewAppMessageList : (appMsg -> String) -> Bool -> Int -> List appMsg -> Html (Msg appMsg)
viewAppMessageList appMsgToString isVisible appModelIndex appMessages =
    if isVisible then
        H.div [] (viewInitAppMessage appModelIndex :: List.indexedMap (\index appMessage -> viewAppMessage appModelIndex (index + 1) appMessage) appMessages)

    else
        H.text ""


view : Model appModel appMsg -> Html (Msg appMsg)
view { appModels, appMessages, isAppModelVisible, isAppMessagesVisible } =
    H.div
        [ Ha.style "font-family" "'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif"
        , Ha.style "position" "fixed"
        , Ha.style "bottom" "0"
        , Ha.style "right" "0"
        , Ha.style "border" "1px solid #eeeeee"
        ]
        [ viewAppMessageList Debug.toString isAppMessagesVisible (List.length appModels.tails) appMessages
        , viewCurrentAppModel Debug.toString isAppModelVisible appModels.current
        , viewAppModelSlider appModels
        , H.div
            [ Ha.style "display" "flex"
            , Ha.style "justify-content" "stretch"
            ]
            [ viewToggleButton ToggleAppModelVisible "model" isAppModelVisible
            , viewToggleButton ToggleAppMessagesVisible "msg" isAppMessagesVisible
            ]
        ]


toView : (appModel -> Html appMsg) -> Model appModel appMsg -> Html (Msg appMsg)
toView viewApp model =
    H.div []
        [ H.map UpdateApp (viewApp model.appModels.current)
        , view model
        ]
