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
    { printMessage : msg -> String
    , printModel : model -> String
    , messageDecoder : Jd.Decoder msg
    , encodeMessage : msg -> Je.Value
    , commands : List ( String, List msg )
    }


type Page
    = Commands
    | Updates


type DragEvent
    = Start
    | To Position
    | Stop


type EventTarget
    = OverlayButton Bool
    | DragButton
    | DismissButton
    | LayoutButton Layout
    | ImportButton
    | ExportButton
    | UpdateSlider
    | NavigationButtonAt Int Page
    | UpdateButtonAt Int String String
    | CommandButtonAt Int String
    | None


type alias Model model msg =
    { updates : ZipList ( Maybe msg, model )
    , importError : Maybe Jd.Error
    , viewportSize : Size
    , position : Position
    , layout : Layout
    , page : Page
    , hover : EventTarget
    , isDragging : Bool
    , isModelOverlayed : Bool
    }


type Msg msg
    = UpdateWith msg
    | SelectUpdateAt Int
    | SelectPage Page
    | ToggleLayout
    | ToggleModelOverlay
    | Drag DragEvent
    | Hover EventTarget
    | ResizeViewport Size
    | SelectFile
    | FileSelected File
    | ImportUpdates String
    | ExportUpdates
    | BatchMessages (Maybe Int) (List msg)
    | Dismiss
    | DoNothing


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg msg)


type alias ViewConfig model msg view =
    { printModel : model -> String
    , printMessage : msg -> String
    , commands : List ( String, List msg )
    , view : model -> view
    }


type alias UpdateConfig model msg =
    { messageDecoder : Jd.Decoder msg
    , encodeMessage : msg -> Je.Value
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
    UpdateWith


toggleLayout : Layout -> Layout
toggleLayout layout =
    case layout of
        Collapsed ->
            Expanded

        Expanded ->
            Collapsed


toRelativeDragButtonPosition : Position -> Layout -> Position
toRelativeDragButtonPosition { top, left } layout =
    { left = left - 137
    , top =
        case layout of
            Expanded ->
                top - 207

            Collapsed ->
                top - 9
    }


pageToString : Page -> String
pageToString page =
    case page of
        Commands ->
            "Commands"

        Updates ->
            "Updates"


pageToOverflow : Page -> String
pageToOverflow page =
    case page of
        Commands ->
            "hidden scroll"

        Updates ->
            "hidden"


toCursorStyle : Bool -> EventTarget -> H.Attribute msg
toCursorStyle isDragging target =
    let
        cursor =
            if isDragging then
                "grabbing"

            else
                case target of
                    None ->
                        "unset"

                    DragButton ->
                        "grab"

                    UpdateSlider ->
                        "grab"

                    _ ->
                        "pointer"
    in
    Ha.style "cursor" cursor


exportDisabledTitle : String
exportDisabledTitle =
    "You need at least 1 update to export this session"


initStateLabel : String
initStateLabel =
    "Init"


toTitle : EventTarget -> String
toTitle target =
    case target of
        OverlayButton isModelOverlayed ->
            if isModelOverlayed then
                "Hide the model"

            else
                "Inspect the model"

        DragButton ->
            "Drag to a new position"

        DismissButton ->
            "Dismiss to the upper-right"

        LayoutButton layout ->
            case layout of
                Expanded ->
                    "Collapse to hide controls"

                Collapsed ->
                    "Expand to reveal controls"

        ImportButton ->
            "Import from a file"

        ExportButton ->
            "Export to a file"

        UpdateSlider ->
            "Scroll to other states"

        NavigationButtonAt _ page ->
            case page of
                Updates ->
                    "A list of updates"

                Commands ->
                    "A list of commands"

        UpdateButtonAt _ msgString modelString ->
            msgString ++ "\n\n" ++ modelString

        CommandButtonAt _ msgs ->
            msgs

        None ->
            ""


layoutToSize : Layout -> Size
layoutToSize layout =
    case layout of
        Collapsed ->
            { height = 18, width = 180 }

        Expanded ->
            { height = 218, width = 180 }


updatePosition : Size -> Size -> Position -> Position
updatePosition debugSize viewportSize debugPosition =
    { top = clamp 0 (viewportSize.height - debugSize.height) debugPosition.top
    , left = clamp 0 (viewportSize.width - debugSize.width) debugPosition.left
    }


viewDivider : Html msg
viewDivider =
    H.div
        [ Ha.style "border-left" U.border
        , Ha.style "margin" "3px 0 4px 0"
        ]
        []


viewIcon : List (S.Attribute (Msg msg)) -> List (Svg (Msg msg)) -> Html (Msg msg)
viewIcon attributes =
    S.svg
        (Sa.width "15"
            :: Sa.height "15"
            :: Sa.viewBox "0 0 24 24"
            :: attributes
        )


viewDragButton : EventTarget -> Bool -> Html (Msg msg)
viewDragButton target isDragging =
    viewIcon
        [ Se.onMouseDown (Drag Start)
        , Se.onMouseOver (Hover DragButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.fill (U.toIconColor (target == DragButton) isDragging)
            , Sa.d "M7,19V17H9V19H7M11,19V17H13V19H11M15,19V17H17V19H15M7,15V13H9V15H7M11,15V13H13V15H11M15,15V13H17V15H15M7,11V9H9V11H7M11,11V9H13V11H11M15,11V9H17V11H15M7,7V5H9V7H7M11,7V5H13V7H11M15,7V5H17V7H15Z"
            ]
            []
        , S.title [] [ S.text (toTitle DragButton) ]
        ]


viewDismissButton : EventTarget -> Size -> Position -> Html (Msg msg)
viewDismissButton target { height, width } { top, left } =
    viewIcon
        [ Se.onClick Dismiss
        , Se.onMouseOver (Hover DismissButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.d "M5,17.59L15.59,7H9V5H19V15H17V8.41L6.41,19L5,17.59Z"
            , Sa.fill (U.toIconColor (target == DismissButton) False)
            ]
            []
        , S.title [] [ S.text (toTitle DismissButton) ]
        ]


viewImportButton : EventTarget -> Html (Msg msg)
viewImportButton target =
    viewIcon
        [ Se.onClick SelectFile
        , Se.onMouseOver (Hover ImportButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.d "M14,2H6A2,2 0 0,0 4,4V20A2,2 0 0,0 6,22H18A2,2 0 0,0 20,20V8L14,2M13.5,16V19H10.5V16H8L12,12L16,16H13.5M13,9V3.5L18.5,9H13Z"
            , Sa.fill (U.toIconColor (target == ImportButton) False)
            ]
            []
        , S.title [] [ S.text (toTitle ImportButton) ]
        ]


toViewExportButtonAttributes : Bool -> List (S.Attribute (Msg msg))
toViewExportButtonAttributes isEnabled =
    if isEnabled then
        [ Se.onClick ExportUpdates
        , Se.onMouseOver (Hover ExportButton)
        , Se.onMouseOut (Hover None)
        ]

    else
        []


toViewExportButtonText : Bool -> String
toViewExportButtonText isEnabled =
    if isEnabled then
        toTitle ExportButton

    else
        exportDisabledTitle


viewExportButton : EventTarget -> Bool -> Html (Msg msg)
viewExportButton target isEnabled =
    viewIcon
        (toViewExportButtonAttributes isEnabled)
        [ S.path
            [ Sa.d "M14,2L20,8V20A2,2 0 0,1 18,22H6A2,2 0 0,1 4,20V4A2,2 0 0,1 6,2H14M18,20V9H13V4H6V20H18M12,19L8,15H10.5V12H13.5V15H16L12,19Z"
            , Sa.fill (U.toIconColor (target == ExportButton) False)
            ]
            []
        , S.title []
            [ S.text
                (toViewExportButtonText isEnabled)
            ]
        ]


viewOverlayButton : EventTarget -> Bool -> Html (Msg msg)
viewOverlayButton target isModelOverlayed =
    viewIcon
        [ Se.onClick ToggleModelOverlay
        , Se.onMouseOver (Hover (OverlayButton isModelOverlayed))
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.d "M5,3H7V5H5V10A2,2 0 0,1 3,12A2,2 0 0,1 5,14V19H7V21H5C3.93,20.73 3,20.1 3,19V15A2,2 0 0,0 1,13H0V11H1A2,2 0 0,0 3,9V5A2,2 0 0,1 5,3M19,3A2,2 0 0,1 21,5V9A2,2 0 0,0 23,11H24V13H23A2,2 0 0,0 21,15V19A2,2 0 0,1 19,21H17V19H19V14A2,2 0 0,1 21,12A2,2 0 0,1 19,10V5H17V3H19M12,15A1,1 0 0,1 13,16A1,1 0 0,1 12,17A1,1 0 0,1 11,16A1,1 0 0,1 12,15M8,15A1,1 0 0,1 9,16A1,1 0 0,1 8,17A1,1 0 0,1 7,16A1,1 0 0,1 8,15M16,15A1,1 0 0,1 17,16A1,1 0 0,1 16,17A1,1 0 0,1 15,16A1,1 0 0,1 16,15Z"
            , Sa.fill (U.toIconColor (target == OverlayButton isModelOverlayed) isModelOverlayed)
            ]
            []
        , S.title [] [ S.text (toTitle (OverlayButton isModelOverlayed)) ]
        ]


viewLayoutButton : EventTarget -> Layout -> Html (Msg msg)
viewLayoutButton target layout =
    let
        d =
            case layout of
                Expanded ->
                    "M19,6.41L17.59,5L12,10.59L6.41,5L5,6.41L10.59,12L5,17.59L6.41,19L12,13.41L17.59,19L19,17.59L13.41,12L19,6.41Z"

                Collapsed ->
                    "M20 8h-2.81c-.45-.78-1.07-1.45-1.82-1.96L17 4.41 15.59 3l-2.17 2.17C12.96 5.06 12.49 5 12 5c-.49 0-.96.06-1.41.17L8.41 3 7 4.41l1.62 1.63C7.88 6.55 7.26 7.22 6.81 8H4v2h2.09c-.05.33-.09.66-.09 1v1H4v2h2v1c0 .34.04.67.09 1H4v2h2.81c1.04 1.79 2.97 3 5.19 3s4.15-1.21 5.19-3H20v-2h-2.09c.05-.33.09-.66.09-1v-1h2v-2h-2v-1c0-.34-.04-.67-.09-1H20V8zm-6 8h-4v-2h4v2zm0-4h-4v-2h4v2z"
    in
    viewIcon
        [ Se.onClick ToggleLayout
        , Se.onMouseOver (Hover (LayoutButton layout))
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.fill (U.toIconColor (target == LayoutButton layout) False)
            , Sa.d d
            ]
            []
        , S.title [] [ S.text (toTitle (LayoutButton layout)) ]
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
        [ Ha.title (toTitle UpdateSlider)
        ]
        [ H.input
            [ Ha.style "margin" "0 5%"
            , Ha.style "width" "90%"
            , Ha.style "height" "18px"
            , Ha.type_ "range"
            , Ha.min (String.fromInt 0)
            , Ha.disabled (length == 1)
            , Ha.max (String.fromInt (length - 1))
            , Ha.value (String.fromInt currentIndex)
            , He.onInput (SelectUpdateAt << Maybe.withDefault 0 << String.toInt)
            , He.onMouseOver (Hover UpdateSlider)
            , He.onMouseOut (Hover None)
            ]
            []
        ]


viewUpdate : EventTarget -> Int -> ( Int, String, String ) -> Html (Msg msg)
viewUpdate target currentIndex ( index, msgString, modelString ) =
    let
        isSelected =
            index == currentIndex

        isHovered =
            target == UpdateButtonAt index modelString msgString

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
        , Ha.title (toTitle (UpdateButtonAt index msgString modelString))
        , He.onClick (SelectUpdateAt index)
        , He.onMouseOver (Hover (UpdateButtonAt index msgString modelString))
        , He.onMouseOut (Hover None)
        ]
        [ H.text (U.trim 24 msgString)
        , H.span
            [ Ha.style "float" "right"
            ]
            [ H.text (String.fromInt index)
            ]
        ]


viewCommand : (msg -> String) -> EventTarget -> Int -> ( String, List msg ) -> Html (Msg msg)
viewCommand printMsg target index ( label, msgs ) =
    let
        commandTarget =
            CommandButtonAt index (U.join (\a b -> b ++ "\n " ++ a) "" (List.map printMsg msgs))
    in
    U.selectable False
        [ Ha.style "width" "159px"
        , Ha.style "margin" "3px"
        , Ha.style "line-height" "18px"
        , Ha.style "font-size" "9px"
        , Ha.style "font-weight" "500"
        , Ha.style "text-align" "center"
        , Ha.style "border" U.border
        , Ha.style "background-color" (U.toListBackgroundColor False (target == commandTarget) False)
        , Ha.title (toTitle commandTarget)
        , He.onMouseOver (Hover commandTarget)
        , He.onMouseOut (Hover None)
        , He.onClick (BatchMessages Nothing msgs)
        ]
        [ H.text (U.trim 16 label)
        ]


viewPage : Int -> EventTarget -> (msg -> String) -> Size -> Page -> List ( Int, String, String ) -> List ( String, List msg ) -> Html (Msg msg)
viewPage currentIndex target printMessage layoutSize page updates commands =
    let
        body =
            case page of
                Updates ->
                    List.map (viewUpdate target currentIndex) updates

                Commands ->
                    List.indexedMap (viewCommand printMessage target) commands
    in
    H.div
        [ Ha.style "border-bottom" U.border
        , Ha.style "height" (U.toPx (layoutSize.height - 38))
        , Ha.style "overflow" (pageToOverflow page)
        ]
        body


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


viewNavigationPage : Int -> Page -> Page -> EventTarget -> Html (Msg msg)
viewNavigationPage index page modelPage target =
    let
        isSelected =
            page == modelPage

        isHovered =
            target == NavigationButtonAt index page
    in
    U.selectable False
        [ Ha.style "height" "18px"
        , Ha.style "line-height" "18px"
        , Ha.style "font-size" "9px"
        , Ha.style "padding" "0 9px"
        , Ha.style "background-color" (U.toBackgroundColor isHovered isSelected)
        , Ha.style "color" (U.toTextColor isHovered isSelected)
        , Ha.title (toTitle (NavigationButtonAt index page))
        , He.onMouseOver (Hover (NavigationButtonAt index page))
        , He.onMouseOut (Hover None)
        , He.onClick (SelectPage page)
        ]
        [ H.text (pageToString page)
        ]


toNavigationUnderlineWidth : Page -> Int
toNavigationUnderlineWidth page =
    case page of
        Updates ->
            54

        Commands ->
            64


toNavigationUnderlineTransform : Page -> String
toNavigationUnderlineTransform page =
    case page of
        Updates ->
            "translate(0,0)"

        Commands ->
            "translate(55px,0)"


viewNavigationUnderline : Page -> Html msg
viewNavigationUnderline page =
    H.div
        [ Ha.style "position" "absolute"
        , Ha.style "top" "17px"
        , Ha.style "left" "48px"
        , Ha.style "transition" "transform 140ms ease-out, width 140ms ease-out"
        , Ha.style "border-bottom" ("2px solid " ++ U.toIconColor False True)
        , Ha.style "width" (U.toPx (toNavigationUnderlineWidth page))
        , Ha.style "transform" (toNavigationUnderlineTransform page)
        ]
        []


viewDebug : (msg -> String) -> (model -> String) -> List ( String, List msg ) -> Model model msg -> Html (Msg msg)
viewDebug printMessage printModel commands model =
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
                    [ viewOverlayButton model.hover model.isModelOverlayed
                    , viewExportButton model.hover (Zl.length model.updates > 1)
                    , viewImportButton model.hover
                    ]
                , viewDivider
                , viewNavigationPage 0 Updates model.page model.hover
                , viewNavigationPage 1 Commands model.page model.hover
                , viewNavigationUnderline model.page
                ]
        , U.viewIf isExpanded <|
            viewPage
                (Zl.currentIndex model.updates)
                model.hover
                printMessage
                layoutSize
                model.page
                (Zl.toList (Zl.trim 10 (Zl.indexedMap (\index ( msg, mdl ) -> ( index, Maybe.withDefault initStateLabel (Maybe.map printMessage msg), printModel mdl )) model.updates)))
                commands
        , viewControls
            [ viewSlider (Zl.length model.updates) (Zl.currentIndex model.updates)
            , viewDivider
            , viewButtons
                [ viewDragButton model.hover model.isDragging
                , viewDismissButton model.hover layoutSize model.position
                , viewLayoutButton model.hover model.layout
                ]
            ]
        ]


toDocument : ViewConfig model msg (Browser.Document msg) -> Model model msg -> Browser.Document (Msg msg)
toDocument { printModel, printMessage, commands, view } model =
    { title = "Debug"
    , body =
        [ U.selectable (not model.isDragging)
            [ toCursorStyle model.isDragging model.hover
            ]
            (viewDebug printMessage printModel commands model
                :: U.viewIf model.isModelOverlayed (viewOverlay model.viewportSize (printModel (Tuple.second model.updates.current)))
                :: List.map (H.map UpdateWith) (.body (view (Tuple.second model.updates.current)))
            )
        ]
    }


toHtml : ViewConfig model msg (Html msg) -> Model model msg -> Html (Msg msg)
toHtml { printModel, printMessage, commands, view } model =
    U.selectable (not model.isDragging)
        [ toCursorStyle model.isDragging model.hover
        ]
        [ viewDebug printMessage printModel commands model
        , U.viewIf model.isModelOverlayed (viewOverlay model.viewportSize (printModel (Tuple.second model.updates.current)))
        , H.map UpdateWith (view (Tuple.second model.updates.current))
        ]


toInit : ( model, Cmd msg ) -> ( Model model msg, Cmd (Msg msg) )
toInit ( model, cmd ) =
    ( { updates = Zl.singleton ( Nothing, model )

      -- TODO -- remove the invalid state where
      -- { model
      --     | position = { left = 10000, top = 10000 }
      --     , viewportSize = { width = 0, height = 0 }
      -- }
      , position = { left = 10000, top = 10000 }
      , viewportSize = { width = 0, height = 0 }
      , layout = Collapsed
      , isModelOverlayed = False
      , page = Updates
      , isDragging = False
      , hover = None
      , importError = Nothing
      }
    , Cmd.batch
        [ Cmd.map UpdateWith cmd
        , Task.perform ResizeViewport (Task.map Size.fromViewport Bd.getViewport)
        ]
    )


toSubscriptions : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
toSubscriptions subscriptions { updates, position, isDragging } =
    Sub.batch
        [ Sub.map UpdateWith (subscriptions (Tuple.second updates.current))
        , Be.onResize (Size.map2 ResizeViewport)
        , U.subscribeIf isDragging
            (Sub.batch
                [ Be.onMouseMove (Jd.map (Drag << To) (Position.mouseMoveDecoder position))
                , Be.onMouseUp (Jd.succeed (Drag Stop))
                ]
            )
        ]


toUpdate : UpdateConfig model msg -> Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
toUpdate { messageDecoder, encodeMessage, update } msg model =
    case msg of
        UpdateWith updateMsg ->
            let
                ( newModel, cmd ) =
                    update updateMsg (Tuple.second model.updates.current)

                updates =
                    Zl.dropHeads (Zl.insert ( Just updateMsg, newModel ) model.updates)
            in
            ( { model | updates = updates }
            , Cmd.map UpdateWith cmd
            )

        SelectUpdateAt index ->
            ( { model | updates = Zl.toIndex index model.updates }, Cmd.none )

        SelectPage page ->
            ( { model | page = page }, Cmd.none )

        FileSelected file ->
            ( model, Task.perform ImportUpdates (F.toString file) )

        ToggleModelOverlay ->
            ( { model
                | isModelOverlayed = not model.isModelOverlayed
              }
            , Cmd.none
            )

        ToggleLayout ->
            ( { model
                | layout = toggleLayout model.layout
                , isModelOverlayed = model.isModelOverlayed && model.layout == Collapsed
                , position = updatePosition (layoutToSize (toggleLayout model.layout)) model.viewportSize model.position
              }
            , Cmd.none
            )

        SelectFile ->
            ( model, Fs.file [ "application/json" ] FileSelected )

        ResizeViewport viewportSize ->
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

        Hover hover ->
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
            case Zl.filterMap Tuple.first model.updates of
                Nothing ->
                    ( model, Cmd.none )

                Just msgZl ->
                    ( model
                      -- TODO -- generate appropriate name instead of "elm-debug", based on Browser.Document.title, if available
                    , Fd.string "elm-debug" "application/json" (Je.encode 0 (Zl.jsonEncode encodeMessage msgZl))
                    )

        Drag Start ->
            ( { model | isDragging = True }, Cmd.none )

        Drag (To position) ->
            ( { model
                | position =
                    updatePosition
                        (layoutToSize model.layout)
                        model.viewportSize
                        (toRelativeDragButtonPosition position model.layout)
              }
            , Cmd.none
            )

        Drag Stop ->
            ( { model | isDragging = False }, Cmd.none )

        ImportUpdates text ->
            case Jd.decodeString (Zl.jsonDecoder messageDecoder) text of
                Ok updates ->
                    ( { model | updates = Zl.singleton (Zl.toHead model.updates).current }
                    , U.msgToCmd (BatchMessages (Just (Zl.currentIndex updates + 1)) (Zl.toList updates))
                    )

                Err err ->
                    -- TODO -- handle the import error case with some kind of feedback to the user
                    ( { model | importError = Just err }, Cmd.none )

        BatchMessages selectIndex msgs ->
            case msgs of
                head :: tails ->
                    ( model
                    , Cmd.batch
                        [ U.msgToCmd (UpdateWith head)
                        , U.msgToCmd (BatchMessages selectIndex tails)
                        ]
                    )

                [] ->
                    case selectIndex of
                        Just index ->
                            ( model, U.msgToCmd (SelectUpdateAt index) )

                        Nothing ->
                            ( model, Cmd.none )
