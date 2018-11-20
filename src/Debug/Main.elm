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
import Html.Lazy as Hl
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
import ZipList as Zl exposing (ZipList)



-- API


type alias Config model msg =
    { printMessage : msg -> String
    , printModel : model -> String
    , messageDecoder : Jd.Decoder msg
    , encodeMessage : msg -> Je.Value
    , commands : List ( String, List msg )
    }


type alias Program flags model msg =
    Platform.Program flags (Model model msg) (Msg msg)


doNothing : Msg msg
doNothing =
    DoNothing


toDocument : ViewConfig model msg (Browser.Document msg) -> Model model msg -> Browser.Document (Msg msg)
toDocument { printModel, printMessage, commands, view } model =
    { title = "Debug"
    , body =
        [ selectable (not model.isDragging)
            [ toCursorStyle model.isDragging model.hoverable
            ]
            (viewDebug printMessage printModel commands model
                :: viewIf model.isModelOverlayed (viewOverlay model.viewportSize (printModel (Tuple.second model.updates.current)))
                :: List.map (H.map UpdateWith) (.body (view (Tuple.second model.updates.current)))
            )
        ]
    }


toHtml : ViewConfig model msg (Html msg) -> Model model msg -> Html (Msg msg)
toHtml { printModel, printMessage, commands, view } model =
    selectable (not model.isDragging)
        [ toCursorStyle model.isDragging model.hoverable
        ]
        [ viewDebug printMessage printModel commands model
        , viewIf model.isModelOverlayed (viewOverlay model.viewportSize (printModel (Tuple.second model.updates.current)))
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
      , hoverable = None
      , importError = Nothing
      }
    , Cmd.batch
        [ Cmd.map UpdateWith cmd
        , Task.perform ResizeViewport (Task.map Size.fromViewport Bd.getViewport)
        ]
    )


toMsg : msg -> Msg msg
toMsg =
    UpdateWith


toSubscriptions : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
toSubscriptions subscriptions { updates, position, isDragging } =
    Sub.batch
        [ Sub.map UpdateWith (subscriptions (Tuple.second updates.current))
        , Be.onResize (Size.map2 ResizeViewport)
        , subscribeIf isDragging
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
            ( { model | importError = Nothing }
            , Fs.file [ jsonMimeType ] FileSelected
            )

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

        Hover hoverable ->
            ( { model | hoverable = hoverable }, Cmd.none )

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
                    , Fd.string "elm-debug" jsonMimeType (Je.encode 0 (Zl.jsonEncode encodeMessage msgZl))
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
                    ( { model | updates = Zl.singleton (Zl.toTail model.updates).current }
                    , msgToCmd (BatchMessages (Just (Zl.currentIndex updates + 1)) (Zl.toList updates))
                    )

                Err err ->
                    ( { model | importError = Just err }, Cmd.none )

        BatchMessages selectIndex msgs ->
            case msgs of
                head :: tails ->
                    ( model
                    , Cmd.batch
                        [ msgToCmd (UpdateWith head)
                        , msgToCmd (BatchMessages selectIndex tails)
                        ]
                    )

                [] ->
                    case selectIndex of
                        Just index ->
                            ( model, msgToCmd (SelectUpdateAt index) )

                        Nothing ->
                            ( model, Cmd.none )



-- Internals


type Page
    = Commands
    | Updates


type DragEvent
    = Start
    | To Position
    | Stop


type Hoverable
    = OverlayButton
    | DragButton
    | DismissButton
    | LayoutButton
    | ImportButton
    | ExportButton
    | UpdateSlider
    | NavigationButtonFor Page
    | UpdateButtonAt Int
    | CommandButtonAt Int
    | None


type alias Model model msg =
    { updates : ZipList ( Maybe msg, model )
    , importError : Maybe Jd.Error
    , isModelOverlayed : Bool
    , hoverable : Hoverable
    , viewportSize : Size
    , position : Position
    , isDragging : Bool
    , layout : Layout
    , page : Page
    }


type Msg msg
    = UpdateWith msg
    | SelectUpdateAt Int
    | SelectPage Page
    | ToggleLayout
    | ToggleModelOverlay
    | Drag DragEvent
    | Hover Hoverable
    | ResizeViewport Size
    | SelectFile
    | FileSelected File
    | ImportUpdates String
    | ExportUpdates
    | BatchMessages (Maybe Int) (List msg)
    | Dismiss
    | DoNothing


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


fit : Int -> String -> String
fit maxLength text =
    if String.length text > maxLength then
        String.left (maxLength - 3) text ++ "..."

    else
        text


jsonMimeType : String
jsonMimeType =
    "application/json"


msgToCmd : msg -> Cmd msg
msgToCmd =
    Task.perform identity << Task.succeed


toPx : Int -> String
toPx n =
    String.fromInt n ++ "px"


viewIf : Bool -> Html msg -> Html msg
viewIf isVisible html =
    if isVisible then
        html

    else
        H.text ""


subscribeIf : Bool -> Sub msg -> Sub msg
subscribeIf isSubscribing subscribe =
    if isSubscribing then
        subscribe

    else
        Sub.none


listJoin : (a -> a -> a) -> a -> List a -> a
listJoin add unit list =
    case list of
        head :: tails ->
            List.foldl add head tails

        [] ->
            unit


selectable : Bool -> List (H.Attribute msg) -> List (Html msg) -> Html msg
selectable isSelectable attributes =
    H.div
        (if isSelectable then
            attributes

         else
            attributes
                ++ [ Ha.style "-webkit-touch-callout" "none"
                   , Ha.style "-webkit-user-select" "none"
                   , Ha.style "-khtml-user-select" "none"
                   , Ha.style "-moz-user-select" "none"
                   , Ha.style "-ms-user-select" "none"
                   , Ha.style "user-select" "none"
                   ]
        )


onRightClick : msg -> H.Attribute msg
onRightClick msg =
    He.preventDefaultOn "contextmenu" (Jd.succeed ( msg, True ))


isJust : Maybe a -> Bool
isJust maybe =
    maybe /= Nothing


isOdd : Int -> Bool
isOdd n =
    modBy 2 n == 1


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


pageToOverflow : Page -> String
pageToOverflow page =
    case page of
        Commands ->
            "hidden scroll"

        Updates ->
            "hidden"


toCursorStyle : Bool -> Hoverable -> H.Attribute msg
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
        [ Ha.style "border-left" "1px solid #d3d3d3"
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


viewDragButton : Hoverable -> Bool -> Html (Msg msg)
viewDragButton target isDragging =
    let
        color =
            if isDragging then
                "#1cabf1"

            else if DragButton == target then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        [ Se.onMouseDown (Drag Start)
        , Se.onMouseOver (Hover DragButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.fill color
            , Sa.d "M7,19V17H9V19H7M11,19V17H13V19H11M15,19V17H17V19H15M7,15V13H9V15H7M11,15V13H13V15H11M15,15V13H17V15H15M7,11V9H9V11H7M11,11V9H13V11H11M15,11V9H17V11H15M7,7V5H9V7H7M11,7V5H13V7H11M15,7V5H17V7H15Z"
            ]
            []
        , S.title [] [ S.text "Drag to a new position" ]
        ]


viewDismissButton : Hoverable -> Size -> Position -> Html (Msg msg)
viewDismissButton target { height, width } { top, left } =
    let
        color =
            if DismissButton == target then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        [ Se.onClick Dismiss
        , Se.onMouseOver (Hover DismissButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.d "M5,17.59L15.59,7H9V5H19V15H17V8.41L6.41,19L5,17.59Z"
            , Sa.fill color
            ]
            []
        , S.title [] [ S.text "Dismiss to the upper-right" ]
        ]


toImportButtonColor : Bool -> Bool -> String
toImportButtonColor isError isHovered =
    if isHovered then
        "black"

    else if isError then
        "red"

    else
        "#7c7c7c"


viewImportButton : Hoverable -> Maybe Jd.Error -> Html (Msg msg)
viewImportButton target importError =
    let
        title =
            case importError of
                Just decodeError ->
                    Jd.errorToString decodeError

                Nothing ->
                    "Import from a file"
    in
    viewIcon
        [ Se.onClick SelectFile
        , Se.onMouseOver (Hover ImportButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.d "M14,2H6A2,2 0 0,0 4,4V20A2,2 0 0,0 6,22H18A2,2 0 0,0 20,20V8L14,2M13.5,16V19H10.5V16H8L12,12L16,16H13.5M13,9V3.5L18.5,9H13Z"
            , Sa.fill (toImportButtonColor (isJust importError) (ImportButton == target))
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewExportButton : Hoverable -> Bool -> Html (Msg msg)
viewExportButton target isEnabled =
    let
        attributes =
            if isEnabled then
                [ Se.onClick ExportUpdates
                , Se.onMouseOver (Hover ExportButton)
                , Se.onMouseOut (Hover None)
                ]

            else
                []

        title =
            if isEnabled then
                "Export to a file"

            else
                "You need at least 1 update to export this session"

        color =
            if ExportButton == target then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        attributes
        [ S.path
            [ Sa.d "M14,2L20,8V20A2,2 0 0,1 18,22H6A2,2 0 0,1 4,20V4A2,2 0 0,1 6,2H14M18,20V9H13V4H6V20H18M12,19L8,15H10.5V12H13.5V15H16L12,19Z"
            , Sa.fill color
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewOverlayButton : Hoverable -> Bool -> Html (Msg msg)
viewOverlayButton target isModelOverlayed =
    let
        title =
            if isModelOverlayed then
                "Hide the model"

            else
                "Inspect the model"

        color =
            if isModelOverlayed then
                "#1cabf1"

            else if OverlayButton == target then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        [ Se.onClick ToggleModelOverlay
        , Se.onMouseOver (Hover OverlayButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.d "M5,3H7V5H5V10A2,2 0 0,1 3,12A2,2 0 0,1 5,14V19H7V21H5C3.93,20.73 3,20.1 3,19V15A2,2 0 0,0 1,13H0V11H1A2,2 0 0,0 3,9V5A2,2 0 0,1 5,3M19,3A2,2 0 0,1 21,5V9A2,2 0 0,0 23,11H24V13H23A2,2 0 0,0 21,15V19A2,2 0 0,1 19,21H17V19H19V14A2,2 0 0,1 21,12A2,2 0 0,1 19,10V5H17V3H19M12,15A1,1 0 0,1 13,16A1,1 0 0,1 12,17A1,1 0 0,1 11,16A1,1 0 0,1 12,15M8,15A1,1 0 0,1 9,16A1,1 0 0,1 8,17A1,1 0 0,1 7,16A1,1 0 0,1 8,15M16,15A1,1 0 0,1 17,16A1,1 0 0,1 16,17A1,1 0 0,1 15,16A1,1 0 0,1 16,15Z"
            , Sa.fill color
            ]
            []
        , S.title [] [ S.text title ]
        ]


viewLayoutButton : Hoverable -> Layout -> Html (Msg msg)
viewLayoutButton target layout =
    let
        ( title, d ) =
            case layout of
                Expanded ->
                    ( "Collapse to hide controls"
                    , "M19,6.41L17.59,5L12,10.59L6.41,5L5,6.41L10.59,12L5,17.59L6.41,19L12,13.41L17.59,19L19,17.59L13.41,12L19,6.41Z"
                    )

                Collapsed ->
                    ( "Expand to reveal controls"
                    , "M20 8h-2.81c-.45-.78-1.07-1.45-1.82-1.96L17 4.41 15.59 3l-2.17 2.17C12.96 5.06 12.49 5 12 5c-.49 0-.96.06-1.41.17L8.41 3 7 4.41l1.62 1.63C7.88 6.55 7.26 7.22 6.81 8H4v2h2.09c-.05.33-.09.66-.09 1v1H4v2h2v1c0 .34.04.67.09 1H4v2h2.81c1.04 1.79 2.97 3 5.19 3s4.15-1.21 5.19-3H20v-2h-2.09c.05-.33.09-.66.09-1v-1h2v-2h-2v-1c0-.34-.04-.67-.09-1H20V8zm-6 8h-4v-2h4v2zm0-4h-4v-2h4v2z"
                    )

        color =
            if LayoutButton == target then
                "black"

            else
                "#7c7c7c"
    in
    viewIcon
        [ Se.onClick ToggleLayout
        , Se.onMouseOver (Hover LayoutButton)
        , Se.onMouseOut (Hover None)
        ]
        [ S.path
            [ Sa.fill color
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
        , Ha.style "height" (toPx viewportSize.height)
        , Ha.style "width" (toPx viewportSize.width)
        ]
        [ H.div [] [ H.text text ]
        ]


viewSlider : Int -> Int -> Html (Msg msg)
viewSlider length currentIndex =
    H.div
        [ Ha.title "Scroll to other states"
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


viewUpdate : Hoverable -> Int -> ( Int, String, String ) -> Html (Msg msg)
viewUpdate target currentIndex ( index, msgString, modelString ) =
    let
        isSelected =
            index == currentIndex

        color =
            if isSelected then
                "white"

            else
                "black"

        backgroundColor =
            if isSelected then
                "#1cabf1"

            else if UpdateButtonAt index == target then
                "#f1f6fd"

            else if isOdd index then
                "#f5f5f5"

            else
                "white"
    in
    selectable False
        [ Ha.style "height" "18px"
        , Ha.style "line-height" "18px"
        , Ha.style "font-size" "8px"
        , Ha.style "padding" "0 9px"
        , Ha.style "color" color
        , Ha.style "background-color" backgroundColor
        , Ha.title (msgString ++ "\n\n" ++ modelString)
        , He.onClick (SelectUpdateAt index)
        , He.onMouseOver (Hover (UpdateButtonAt index))
        , He.onMouseOut (Hover None)
        ]
        [ H.text (fit 24 msgString)
        , H.span
            [ Ha.style "float" "right"
            ]
            [ H.text (String.fromInt index)
            ]
        ]


viewCommand : (msg -> String) -> Hoverable -> Int -> ( String, List msg ) -> Html (Msg msg)
viewCommand printMsg target index ( label, msgs ) =
    let
        this =
            CommandButtonAt index

        backgroundColor =
            if CommandButtonAt index == target then
                "#f5f5f5"

            else
                "white"
    in
    selectable False
        [ Ha.style "width" "159px"
        , Ha.style "margin" "3px"
        , Ha.style "line-height" "18px"
        , Ha.style "font-size" "9px"
        , Ha.style "font-weight" "500"
        , Ha.style "text-align" "center"
        , Ha.style "border" "1px solid #d3d3d3"
        , Ha.style "background-color" backgroundColor
        , Ha.title (listJoin (\a b -> b ++ "\n " ++ a) "" (List.map printMsg msgs))
        , He.onMouseOver (Hover this)
        , He.onMouseOut (Hover None)
        , He.onClick (BatchMessages Nothing msgs)
        ]
        [ H.text (fit 16 label)
        ]


viewPage : Int -> Hoverable -> (msg -> String) -> Size -> Page -> List ( Int, String, String ) -> List ( String, List msg ) -> Html (Msg msg)
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
        [ Ha.style "border-bottom" "1px solid #d3d3d3"
        , Ha.style "height" (toPx (layoutSize.height - 38))
        , Ha.style "overflow" (pageToOverflow page)
        ]
        body


viewControls : List (Html (Msg msg)) -> Html (Msg msg)
viewControls =
    H.div
        [ Ha.style "display" "flex"
        , Ha.style "height" "18px"
        , Ha.style "background-color" "#f3f3f3"
        , Ha.style "border-bottom" "1px solid #d3d3d3"
        ]


viewButtons : List (Html (Msg msg)) -> Html (Msg msg)
viewButtons =
    H.div
        [ Ha.style "display" "flex"
        , Ha.style "margin" "1px"
        ]


viewNavigationPage : Int -> Page -> Page -> Hoverable -> Html (Msg msg)
viewNavigationPage index page modelPage target =
    let
        this =
            NavigationButtonFor page

        isSelected =
            page == modelPage

        isHovered =
            this == target

        color =
            if isSelected || isHovered then
                "black"

            else
                "#555555"

        backgroundColor =
            if isSelected then
                "rgba(0,0,0,0)"

            else if isHovered then
                "rgba(0,0,0,.03)"

            else
                "rgba(0,0,0,0)"

        title =
            case page of
                Updates ->
                    "A list of updates"

                Commands ->
                    "A list of commands"

        label =
            case page of
                Commands ->
                    "Commands"

                Updates ->
                    "Updates"
    in
    selectable False
        [ Ha.style "height" "18px"
        , Ha.style "line-height" "18px"
        , Ha.style "font-size" "9px"
        , Ha.style "padding" "0 9px"
        , Ha.style "background-color" backgroundColor
        , Ha.style "color" color
        , Ha.title title
        , He.onMouseOver (Hover this)
        , He.onMouseOut (Hover None)
        , He.onClick (SelectPage page)
        ]
        [ H.text label
        ]


viewNavigationUnderline : Page -> Html msg
viewNavigationUnderline page =
    let
        ( width, transform ) =
            case page of
                Updates ->
                    ( 54, "translate(0,0)" )

                Commands ->
                    ( 64, "translate(55px,0)" )
    in
    H.div
        [ Ha.style "position" "absolute"
        , Ha.style "top" "17px"
        , Ha.style "left" "48px"
        , Ha.style "transition" "transform 140ms ease-out, width 140ms ease-out"
        , Ha.style "border-bottom" "2px solid #1cabf1"
        , Ha.style "width" (toPx width)
        , Ha.style "transform" transform
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
        , Ha.style "border" "1px solid #d3d3d3"
        , Ha.style "top" (toPx model.position.top)
        , Ha.style "left" (toPx model.position.left)
        , Ha.style "width" (toPx (.width layoutSize))
        , Ha.style "height" (toPx (.height layoutSize))
        , onRightClick DoNothing
        ]
        [ viewIf isExpanded <|
            viewControls <|
                [ viewButtons
                    [ viewOverlayButton model.hoverable model.isModelOverlayed
                    , viewExportButton model.hoverable (Zl.length model.updates > 1)
                    , viewImportButton model.hoverable model.importError
                    ]
                , viewDivider
                , viewNavigationPage 0 Updates model.page model.hoverable
                , viewNavigationPage 1 Commands model.page model.hoverable
                , viewNavigationUnderline model.page
                ]
        , viewIf isExpanded <|
            viewPage
                (Zl.currentIndex model.updates)
                model.hoverable
                printMessage
                layoutSize
                model.page
                (Zl.toList (Zl.trim 10 (Zl.indexedMap (\index ( msg, mdl ) -> ( index, Maybe.withDefault "Init" (Maybe.map printMessage msg), printModel mdl )) model.updates)))
                commands
        , viewControls
            [ viewSlider (Zl.length model.updates) (Zl.currentIndex model.updates)
            , viewDivider
            , viewButtons
                [ viewDragButton model.hoverable model.isDragging
                , viewDismissButton model.hoverable layoutSize model.position
                , viewLayoutButton model.hoverable model.layout
                ]
            ]
        ]
