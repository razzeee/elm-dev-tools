module Debug.Main exposing (Config, Program, doNothing, toDocument, toHtml, toInit, toMsg, toSubscriptions, toUpdate)

import Browser
import Browser.Dom as Bd
import Browser.Events as Be
import File as F exposing (File)
import File.Download as Fd
import File.Select as Fs
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Html.Keyed as Hk
import Json.Decode as Jd
import Json.Encode as Je
import Position exposing (Position)
import Process as P
import Size exposing (Size)
import Svg as S exposing (Svg)
import Svg.Attributes as Sa
import Svg.Events as Se
import Task exposing (Task)
import Time
import Tuple
import Utils as U
import ZipList as Zl exposing (ZipList)


type alias Config model msg =
    { msgToString : msg -> String
    , modelToString : model -> String
    , msgDecoder : Jd.Decoder msg
    , encodeMsg : msg -> Je.Value
    , labelMsgsPairs : List ( String, List msg )
    }


type Page
    = LabelMsgsPairs
    | Updates


type DragEvent
    = Start
    | To Position
    | NoDrag


type HoverTarget
    = ModelButton
    | DragButton
    | DismissButton
    | LayoutButton
    | ImportButton
    | ExportButton
    | UpdateSlider
    | NavigationItem Int
    | UpdateItem Int
    | LabelMsgsPairItem Int
    | NoHover


type alias Model model msg =
    { updates : ZipList ( Maybe msg, model )
    , viewportSize : Size
    , position : Position
    , layout : Layout
    , page : Page
    , drag : Bool
    , hover : HoverTarget
    , overlayModel : Bool
    , importError : Maybe Jd.Error
    }


type Msg msg
    = UpdateModel msg
    | SelectUpdate Int
    | SelectPage Page
    | ToggleLayout
    | ToggleModel
    | StartDrag
    | DragTo Position
    | StopDrag
    | HoverElement HoverTarget
    | ViewportResized Size
    | SelectFile
    | FileSelected File
    | ImportUpdates String
    | ExportUpdates
    | BatchMessages (List msg)
    | Dismiss
    | DoNothing


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg msg)


type alias ViewConfig model msg view =
    { modelToString : model -> String
    , msgToString : msg -> String
    , labelMsgsPairs : List ( String, List msg )
    , view : model -> view
    }


type alias UpdateConfig model msg =
    { msgDecoder : Jd.Decoder msg
    , encodeMsg : msg -> Je.Value
    , update : msg -> model -> ( model, Cmd msg )
    }


type Layout
    = Collapsed
    | Expanded


doNothing : Msg msg
doNothing =
    DoNothing


toMsg : msg -> Msg msg
toMsg =
    UpdateModel


toggleLayout : Layout -> Layout
toggleLayout layout =
    case layout of
        Collapsed ->
            Expanded

        Expanded ->
            Collapsed


pageTitle : Page -> String
pageTitle page =
    case page of
        LabelMsgsPairs ->
            "Commands"

        Updates ->
            "Updates"


toCursorStyle : Bool -> HoverTarget -> H.Attribute msg
toCursorStyle drag target =
    Ha.style "cursor" <|
        if drag then
            "grabbing"

        else
            case target of
                NoHover ->
                    "initial"

                DragButton ->
                    "grab"

                _ ->
                    "pointer"


layoutToSize : Layout -> Size
layoutToSize layout =
    case layout of
        Collapsed ->
            { height = 18, width = 180 }

        Expanded ->
            { height = 218, width = 180 }


updatePosition : Size -> Size -> Position -> Position
updatePosition { height, width } viewport { top, left } =
    { top = clamp 0 (viewport.height - height) top
    , left = clamp 0 (viewport.width - width) left
    }


viewDivider : Html msg
viewDivider =
    H.div
        [ Ha.style "border-left" U.border
        , Ha.style "margin" "3px 0 4px 0"
        ]
        []


viewMaterialIconsSvg : List (S.Attribute (Msg msg)) -> List (Svg (Msg msg)) -> Html (Msg msg)
viewMaterialIconsSvg attributes =
    S.svg
        ([ Sa.width "15"
         , Sa.height "15"
         , Sa.viewBox "0 0 24 24"
         ]
            ++ attributes
        )


viewDragButton : HoverTarget -> Bool -> Html (Msg msg)
viewDragButton target drag =
    viewMaterialIconsSvg
        [ Se.onMouseDown StartDrag
        , Se.onMouseOver (HoverElement DragButton)
        , Se.onMouseOut (HoverElement NoHover)
        ]
        [ S.path
            [ Sa.fill (U.toIconColor (target == DragButton) drag)
            , Sa.d "M7,19V17H9V19H7M11,19V17H13V19H11M15,19V17H17V19H15M7,15V13H9V15H7M11,15V13H13V15H11M15,15V13H17V15H15M7,11V9H9V11H7M11,11V9H13V11H11M15,11V9H17V11H15M7,7V5H9V7H7M11,7V5H13V7H11M15,7V5H17V7H15Z"
            ]
            []
        , S.title [] [ S.text "drag to a new position" ]
        ]


viewDismissButton : HoverTarget -> Size -> Position -> Html (Msg msg)
viewDismissButton target { height, width } { top, left } =
    viewMaterialIconsSvg
        [ Se.onClick Dismiss
        , Se.onMouseOver (HoverElement DismissButton)
        , Se.onMouseOut (HoverElement NoHover)
        ]
        [ S.path
            [ Sa.d "M5,17.59L15.59,7H9V5H19V15H17V8.41L6.41,19L5,17.59Z"
            , Sa.fill (U.toIconColor (target == DismissButton) False)
            ]
            []
        , S.title [] [ S.text "dismiss to the upper-right" ]
        ]


viewImportButton : HoverTarget -> Html (Msg msg)
viewImportButton target =
    viewMaterialIconsSvg
        [ Se.onClick SelectFile
        , Se.onMouseOver (HoverElement ImportButton)
        , Se.onMouseOut (HoverElement NoHover)
        ]
        [ S.path
            [ Sa.d "M14,2H6A2,2 0 0,0 4,4V20A2,2 0 0,0 6,22H18A2,2 0 0,0 20,20V8L14,2M13.5,16V19H10.5V16H8L12,12L16,16H13.5M13,9V3.5L18.5,9H13Z"
            , Sa.fill (U.toIconColor (target == ImportButton) False)
            ]
            []
        , S.title [] [ S.text "import another session" ]
        ]


viewExportButton : HoverTarget -> Html (Msg msg)
viewExportButton target =
    viewMaterialIconsSvg
        [ Se.onClick ExportUpdates
        , Se.onMouseOver (HoverElement ExportButton)
        , Se.onMouseOut (HoverElement NoHover)
        ]
        [ S.path
            [ Sa.d "M14,2L20,8V20A2,2 0 0,1 18,22H6A2,2 0 0,1 4,20V4A2,2 0 0,1 6,2H14M18,20V9H13V4H6V20H18M12,19L8,15H10.5V12H13.5V15H16L12,19Z"
            , Sa.fill (U.toIconColor (target == ExportButton) False)
            ]
            []
        , S.title [] [ S.text "export this session" ]
        ]


viewModelButton : HoverTarget -> Bool -> Html (Msg msg)
viewModelButton target modelOverlayed =
    let
        title =
            if modelOverlayed then
                "Hide the model"

            else
                "Inspect the model"
    in
    S.svg
        [ Sa.width "12"
        , Sa.height "12"
        , Sa.style "padding: 1px 1px 2px 2px;"
        , Sa.viewBox "0 0 24 24"
        , Se.onClick ToggleModel
        , Se.onMouseOver (HoverElement ModelButton)
        , Se.onMouseOut (HoverElement NoHover)
        ]
        [ S.path
            [ Sa.d "M5,3H7V5H5V10A2,2 0 0,1 3,12A2,2 0 0,1 5,14V19H7V21H5C3.93,20.73 3,20.1 3,19V15A2,2 0 0,0 1,13H0V11H1A2,2 0 0,0 3,9V5A2,2 0 0,1 5,3M19,3A2,2 0 0,1 21,5V9A2,2 0 0,0 23,11H24V13H23A2,2 0 0,0 21,15V19A2,2 0 0,1 19,21H17V19H19V14A2,2 0 0,1 21,12A2,2 0 0,1 19,10V5H17V3H19M12,15A1,1 0 0,1 13,16A1,1 0 0,1 12,17A1,1 0 0,1 11,16A1,1 0 0,1 12,15M8,15A1,1 0 0,1 9,16A1,1 0 0,1 8,17A1,1 0 0,1 7,16A1,1 0 0,1 8,15M16,15A1,1 0 0,1 17,16A1,1 0 0,1 16,17A1,1 0 0,1 15,16A1,1 0 0,1 16,15Z"
            , Sa.fill (U.toIconColor (target == ModelButton) modelOverlayed)
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewLayoutButton : HoverTarget -> Layout -> Html (Msg msg)
viewLayoutButton target layout =
    let
        ( d, title ) =
            case layout of
                Expanded ->
                    ( "M19,6.41L17.59,5L12,10.59L6.41,5L5,6.41L10.59,12L5,17.59L6.41,19L12,13.41L17.59,19L19,17.59L13.41,12L19,6.41Z"
                    , "collapse the window"
                    )

                Collapsed ->
                    ( "M20 8h-2.81c-.45-.78-1.07-1.45-1.82-1.96L17 4.41 15.59 3l-2.17 2.17C12.96 5.06 12.49 5 12 5c-.49 0-.96.06-1.41.17L8.41 3 7 4.41l1.62 1.63C7.88 6.55 7.26 7.22 6.81 8H4v2h2.09c-.05.33-.09.66-.09 1v1H4v2h2v1c0 .34.04.67.09 1H4v2h2.81c1.04 1.79 2.97 3 5.19 3s4.15-1.21 5.19-3H20v-2h-2.09c.05-.33.09-.66.09-1v-1h2v-2h-2v-1c0-.34-.04-.67-.09-1H20V8zm-6 8h-4v-2h4v2zm0-4h-4v-2h4v2z"
                    , "expand the window"
                    )
    in
    viewMaterialIconsSvg
        [ Se.onClick ToggleLayout
        , Se.onMouseOver (HoverElement LayoutButton)
        , Se.onMouseOut (HoverElement NoHover)
        ]
        [ S.path
            [ Sa.fill (U.toIconColor (target == LayoutButton) False)
            , Sa.d d
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewOverlay : Size -> String -> Html (Msg msg)
viewOverlay viewportSize text =
    H.div
        [ Ha.style "top" "0"
        , Ha.style "left" "0"
        , Ha.style "color" "black"
        , Ha.style "padding" "5vw"
        , Ha.style "position" "fixed"
        , Ha.style "z-index" "2147483646"
        , Ha.style "background-color" "rgba(255,255,255,.95)"
        , Ha.style "height" (U.toPx viewportSize.height)
        , Ha.style "width" (U.toPx viewportSize.width)
        ]
        [ H.div [] [ H.text text ]
        ]


viewSlider : Int -> Int -> Html (Msg msg)
viewSlider length currentIndex =
    H.div
        [ Ha.title "scroll to previous states"
        ]
        [ H.input
            [ Ha.style "margin" "0 5%"
            , Ha.style "width" "90%"
            , Ha.type_ "range"
            , Ha.min (String.fromInt 0)
            , Ha.disabled (length == 1)
            , Ha.max (String.fromInt (length - 1))
            , Ha.value (String.fromInt currentIndex)
            , He.onInput (SelectUpdate << Maybe.withDefault 0 << String.toInt)
            ]
            []
        ]


viewUpdate : HoverTarget -> Int -> ( Int, String ) -> Html (Msg msg)
viewUpdate target currentIndex ( index, label ) =
    let
        isSelected =
            index == currentIndex

        isHovered =
            target == UpdateItem index

        isOdd =
            modBy 2 index == 1
    in
    U.selectable False
        [ Ha.style "height" "18px"
        , Ha.style "line-height" "18px"
        , Ha.style "font-size" "8px"
        , Ha.style "padding" "0 9px"
        , Ha.style "background-color" (U.toListBackgroundColor isOdd isHovered isSelected)
        , Ha.style "color" (U.toListTextColor isSelected)
        , He.onClick (SelectUpdate index)
        , He.onMouseOver (HoverElement (UpdateItem index))
        , He.onMouseOut (HoverElement NoHover)
        ]
        [ H.text (U.trim 24 label)
        , H.span
            [ Ha.style "float" "right"
            ]
            [ H.text (String.fromInt index)
            ]
        ]


viewLabelMsgsPair : HoverTarget -> Int -> ( String, List msg ) -> Html (Msg msg)
viewLabelMsgsPair target index ( label, msgs ) =
    H.button
        [ Ha.style "width" "159px"
        , Ha.style "margin" "4px"
        , Ha.style "line-height" "18px"
        , Ha.style "font-size" "9px"
        , Ha.style "font-weight" "500"
        , Ha.style "background-color" (U.toListBackgroundColor False (target == LabelMsgsPairItem index) False)
        , He.onMouseOver (HoverElement (LabelMsgsPairItem index))
        , He.onMouseOut (HoverElement NoHover)
        , He.onClick (BatchMessages msgs)
        ]
        [ H.text label
        ]


viewPage : Int -> HoverTarget -> (msg -> String) -> Size -> Page -> List ( Int, String ) -> List ( String, List msg ) -> Html (Msg msg)
viewPage currentIndex target msgToString layoutSize page updates labelMsgsPairs =
    H.div
        [ Ha.style "height" (U.toPx (layoutSize.height - 38))
        , Ha.style "border-bottom" U.border
        , Ha.style "overflow"
            (case page of
                LabelMsgsPairs ->
                    "hidden scroll"

                Updates ->
                    "hidden"
            )
        ]
    <|
        case page of
            Updates ->
                List.map (viewUpdate target currentIndex) updates

            LabelMsgsPairs ->
                List.indexedMap (viewLabelMsgsPair target) labelMsgsPairs


viewControls : List (Html (Msg msg)) -> Html (Msg msg)
viewControls =
    H.div
        [ Ha.style "display" "flex"
        , Ha.style "height" "18px"
        , Ha.style "background-color" U.darkGray
        , Ha.style "border-bottom" U.border
        ]


viewButtons : List (Html (Msg msg)) -> Html (Msg msg)
viewButtons =
    H.div
        [ Ha.style "display" "flex"
        , Ha.style "margin" "1px"
        ]


viewNavigationPage : Int -> Page -> Page -> HoverTarget -> Html (Msg msg)
viewNavigationPage index page modelPage target =
    let
        isSelected =
            page == modelPage

        isHovered =
            target == NavigationItem index
    in
    U.selectable False
        [ Ha.style "height" "18px"
        , Ha.style "line-height" "18px"
        , Ha.style "font-size" "9px"
        , Ha.style "padding" "0 9px"
        , Ha.style "color" (U.toTextColor isHovered isSelected)
        , Ha.style "background-color" (U.toBackgroundColor isHovered isSelected)
        , He.onMouseOver (HoverElement (NavigationItem index))
        , He.onMouseOut (HoverElement NoHover)
        , He.onClick (SelectPage page)
        ]
        [ H.text (pageTitle page)
        ]


viewDebug : (msg -> String) -> List ( String, List msg ) -> Model model msg -> Html (Msg msg)
viewDebug msgToString labelMsgsPairs model =
    let
        isExpanded =
            model.layout == Expanded

        layoutSize =
            layoutToSize model.layout
    in
    H.div
        [ Ha.style "position" "fixed"
        , Ha.style "z-index" "2147483647"
        , Ha.style "font-family" "system-ui"
        , Ha.style "background-color" "white"
        , Ha.style "border" U.border
        , Ha.style "top" (U.toPx model.position.top)
        , Ha.style "left" (U.toPx model.position.left)
        , Ha.style "width" (U.toPx (.width layoutSize))
        , Ha.style "height" (U.toPx (.height layoutSize))
        , U.onRightClick DoNothing
        ]
        [ U.viewIf isExpanded <|
            viewControls <|
                [ viewButtons
                    [ viewModelButton model.hover model.overlayModel
                    , viewExportButton model.hover
                    , viewImportButton model.hover
                    ]
                , viewDivider
                , viewNavigationPage 0 Updates model.page model.hover
                , viewNavigationPage 1 LabelMsgsPairs model.page model.hover
                ]
        , U.viewIf isExpanded <|
            viewPage
                (List.length model.updates.tails)
                model.hover
                msgToString
                layoutSize
                model.page
                (Zl.toList (Zl.trim 10 (Zl.indexedMap (\index ( msg, _ ) -> ( index, Maybe.withDefault "Init" (Maybe.map msgToString msg) )) model.updates)))
                labelMsgsPairs
        , viewControls
            [ viewSlider (Zl.length model.updates) (List.length model.updates.tails)
            , viewDivider
            , viewButtons
                [ viewDragButton model.hover model.drag
                , viewDismissButton model.hover layoutSize model.position
                , viewLayoutButton model.hover model.layout
                ]
            ]
        ]


toDocument : ViewConfig model msg (Browser.Document msg) -> Model model msg -> Browser.Document (Msg msg)
toDocument { modelToString, msgToString, labelMsgsPairs, view } model =
    { title = "Debug"
    , body =
        [ U.selectable (not model.drag)
            [ toCursorStyle model.drag model.hover
            ]
            (viewDebug msgToString labelMsgsPairs model
                :: U.viewIf model.overlayModel (viewOverlay model.viewportSize (modelToString (Tuple.second model.updates.current)))
                :: List.map (H.map UpdateModel) (.body (view (Tuple.second model.updates.current)))
            )
        ]
    }


toHtml : ViewConfig model msg (Html msg) -> Model model msg -> Html (Msg msg)
toHtml { modelToString, msgToString, labelMsgsPairs, view } model =
    U.selectable (not model.drag)
        [ toCursorStyle model.drag model.hover
        ]
        [ viewDebug msgToString labelMsgsPairs model
        , U.viewIf model.overlayModel (viewOverlay model.viewportSize (modelToString (Tuple.second model.updates.current)))
        , H.map UpdateModel (view (Tuple.second model.updates.current))
        ]


toInit : ( model, Cmd msg ) -> ( Model model msg, Cmd (Msg msg) )
toInit ( model, cmd ) =
    ( { updates = Zl.singleton ( Nothing, model )
      , position = { left = 10000, top = 10000 }
      , viewportSize = { width = 0, height = 0 }
      , layout = Collapsed
      , overlayModel = False
      , page = Updates
      , drag = False
      , hover = NoHover
      , importError = Nothing
      }
    , Cmd.batch
        [ Cmd.map UpdateModel cmd
        , Task.perform ViewportResized (Task.map Size.fromViewport Bd.getViewport)
        ]
    )


toSubscriptions : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
toSubscriptions subscriptions { updates, position, drag } =
    Sub.batch
        [ Sub.map UpdateModel (subscriptions (Tuple.second updates.current))
        , Be.onResize (Size.map2 ViewportResized)
        , U.subscribeIf drag
            (Sub.batch
                [ Be.onMouseMove (Jd.map DragTo (Position.mouseMoveDecoder position))
                , Be.onMouseUp (Jd.succeed StopDrag)
                ]
            )
        ]


toUpdate : UpdateConfig model msg -> Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
toUpdate { msgDecoder, encodeMsg, update } msg model =
    case msg of
        UpdateModel updateMsg ->
            let
                ( newModel, cmd ) =
                    update updateMsg (Tuple.second model.updates.current)

                updates =
                    Zl.dropHeads (Zl.insert ( Just updateMsg, newModel ) model.updates)
            in
            ( { model | updates = updates }
            , Cmd.map UpdateModel cmd
            )

        SelectUpdate index ->
            ( { model | updates = Zl.toIndex index model.updates }, Cmd.none )

        SelectPage page ->
            ( { model | page = page }, Cmd.none )

        FileSelected file ->
            ( model, Task.perform ImportUpdates (F.toString file) )

        ToggleModel ->
            ( { model
                | overlayModel = not model.overlayModel
              }
            , Cmd.none
            )

        ToggleLayout ->
            ( { model
                | layout = toggleLayout model.layout
                , overlayModel = model.overlayModel && model.layout == Collapsed
                , position = updatePosition (layoutToSize (toggleLayout model.layout)) model.viewportSize model.position
              }
            , Cmd.none
            )

        SelectFile ->
            ( model, Fs.file [ "application/json" ] FileSelected )

        ViewportResized viewportSize ->
            let
                layoutSize =
                    layoutToSize model.layout
            in
            ( { model
                | viewportSize = viewportSize
                , position = updatePosition layoutSize { viewportSize | height = layoutSize.height } model.position
              }
            , Cmd.none
            )

        HoverElement hover ->
            ( { model | hover = hover }, Cmd.none )

        Dismiss ->
            ( { model
                | position =
                    updatePosition
                        (layoutToSize model.layout)
                        model.viewportSize
                        { top = 0
                        , left = model.viewportSize.width
                        }
              }
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )

        ExportUpdates ->
            ( model
            , (Fd.string "elm-debug" "application/json"
                << Je.encode 0
                << Je.list encodeMsg
              )
                (List.reverse (List.filterMap Tuple.first (Zl.toList model.updates)))
            )

        StartDrag ->
            ( { model | drag = True }, Cmd.none )

        DragTo position ->
            ( { model
                | position =
                    updatePosition (layoutToSize model.layout)
                        model.viewportSize
                        { position
                            | top =
                                position.top
                                    - (if model.layout == Expanded then
                                        207

                                       else
                                        9
                                      )
                            , left = position.left - 137
                        }
              }
            , Cmd.none
            )

        StopDrag ->
            ( { model | drag = False }, Cmd.none )

        ImportUpdates text ->
            case Jd.decodeString (Jd.list msgDecoder) text of
                Ok msgs ->
                    ( { model
                        | updates = Zl.singleton (Zl.toTail model.updates).current
                      }
                    , U.msgToCmd (BatchMessages (List.reverse msgs))
                    )

                Err err ->
                    ( { model | importError = Just err }, Cmd.none )

        BatchMessages msgs ->
            case msgs of
                head :: tails ->
                    ( model
                    , Cmd.batch
                        [ U.msgToCmd (UpdateModel head)
                        , U.msgToCmd (BatchMessages tails)
                        ]
                    )

                [] ->
                    ( model, Cmd.none )
