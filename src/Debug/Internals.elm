module Debug.Internals exposing (Program, toInit, toUpdate, toView, view)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import ZipList exposing (ZipList)


type alias Model appModel appMsg =
    { appModels : ZipList appModel
    , appMessages : List appMsg
    , isAppModelVisible : Bool
    , isAppMessagesVisible : Bool
    }


type Msg appMsg
    = UpdateApp appMsg


type alias Program flags appModel appMsg =
    Platform.Program flags (Model appModel appMsg) (Msg appMsg)


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


viewSlider : Element (Msg appMsg)
viewSlider =
    Input.slider
        [ Element.height (Element.px 30)

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color grey
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = AdjustValue
        , label = Input.labelAbove [] (Element.text "My Slider Value")
        , min = 0
        , max = 75
        , step = Nothing
        , value = List.length (List.length model.appModels.heads)
        , thumb =
            Input.defaultThumb
        }


view : Model appModel appMsg -> Element (Msg appMsg)
view { appModels, appMessages, isAppModelVisible, isAppMessagesVisible } =
    Element.el []
        (Element.row []
            viewSlider
        )


toView : (appModel -> Html appMsg) -> Model appModel appMsg -> Html (Msg appMsg)
toView viewApp model =
    let
        appElement =
            Element.map UpdateApp (Element.html (viewApp model.appModels.current))
    in
    Element.layout [] <|
        Element.el
            [ Element.below (view model)
            ]
            appElement
