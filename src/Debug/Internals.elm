module Debug.Internals exposing (Configuration, Program, doNothing, toDocument, toHtml, toInit, toMsg, toSubscriptions, toUpdate)

import Browser
import Browser.Dom as Bd
import Browser.Events as Be
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd
import Json.Encode as Je
import Process as P
import Task exposing (Task)
import Time
import Tuple
import ZipList as Zl exposing (ZipList)


type alias Configuration model msg =
    { printModel : model -> String
    , printMsg : msg -> String
    }


type alias Position =
    { left : Int
    , top : Int
    }


type alias Size =
    { width : Int
    , height : Int
    }


type alias Model model =
    { updates : ZipList ( String, model )
    , viewportSize : Size
    , position : Position
    , isExpanded : Bool
    , isDragging : Bool
    , isHovering : Bool
    , isModelOverlayed : Bool
    }


type Msg msg
    = Update msg
    | ToUpdateAt Int
    | ToggleExpanded
    | ToggleModelOverlay
    | SetDragging Bool
    | SetHovering Bool
    | DragTo Position
    | ResizeViewport Size
    | Dismiss
    | DoNothing


type alias Program flags model msg =
    Platform.Program flags (Model model) (Msg msg)


doNothing : Msg msg
doNothing =
    DoNothing


toMsg : msg -> Msg msg
toMsg =
    Update


viewportToSize : Bd.Viewport -> Size
viewportToSize { viewport } =
    { width = round viewport.width
    , height = round viewport.height
    }


toInit : ( model, Cmd msg ) -> ( Model model, Cmd (Msg msg) )
toInit ( model, cmd ) =
    ( { updates = Zl.singleton ( "Init", model )
      , position = { left = 10000, top = 10000 }
      , viewportSize = { width = 0, height = 0 }
      , isExpanded = False
      , isDragging = False
      , isHovering = False
      , isModelOverlayed = False
      }
    , Cmd.batch
        [ Cmd.map Update cmd
        , Task.perform ResizeViewport (Task.map viewportToSize Bd.getViewport)
        ]
    )


viewModelOverlay : (model -> String) -> Bool -> Size -> model -> Html (Msg msg)
viewModelOverlay viewModel isModelOverlayed size model =
    if isModelOverlayed then
        H.div
            [ Ha.style "position" "fixed"
            , Ha.style "height" "100vh"
            , Ha.style "width" "100vw"
            , Ha.style "background-color" "rgba(255,255,255,.8)"
            , Ha.style "color" "black"
            , Ha.style "z-index" "2147483646"
            , Ha.style "top" "0"
            , Ha.style "left" "0"
            , Ha.style "height" (toPx size.height)
            , Ha.style "width" (toPx size.width)
            , Ha.style "padding" "5vw"
            ]
            [ H.div
                []
                [ H.text (viewModel model)
                ]
            ]

    else
        H.text ""


toHtml : (model -> String) -> (model -> Html msg) -> Model model -> Html (Msg msg)
toHtml printModel view model =
    H.div []
        [ viewDebugger model
        , viewModelOverlay printModel model.isModelOverlayed model.viewportSize (Tuple.second model.updates.current)
        , H.map Update (view (Tuple.second model.updates.current))
        ]


toDocument : (model -> String) -> (model -> Browser.Document msg) -> Model model -> Browser.Document (Msg msg)
toDocument printModel view model =
    { title = "Debug"
    , body =
        viewDebugger model
            :: viewModelOverlay printModel model.isModelOverlayed model.viewportSize (Tuple.second model.updates.current)
            :: List.map (H.map Update) (.body (view (Tuple.second model.updates.current)))
    }


mouseMoveDecoder : Bool -> Position -> Jd.Decoder Position
mouseMoveDecoder isExpanded { left, top } =
    Jd.map2
        Position
        (Jd.field "clientX" Jd.int)
        (Jd.field "clientY" Jd.int)


updatePosition : Bool -> Size -> Position -> Position
updatePosition isExpanded { height, width } { top, left } =
    { top = clamp 0 (height - toWidth isExpanded) (top - 10)
    , left = clamp 0 (width - toWidth isExpanded) (left - toWidth isExpanded // 2)
    }


toUpdate : (msg -> String) -> (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
toUpdate printMsg update msg model =
    case msg of
        Update updateMsg ->
            let
                ( newModel, cmd ) =
                    update updateMsg (Tuple.second model.updates.current)

                updates =
                    Zl.dropHeads (Zl.insert ( printMsg updateMsg, newModel ) model.updates)
            in
            ( { model | updates = updates }
            , Cmd.map Update cmd
            )

        ToUpdateAt index ->
            ( { model | updates = Zl.toIndex index model.updates }, Cmd.none )

        ToggleModelOverlay ->
            ( { model
                | isModelOverlayed = not model.isModelOverlayed
                , isExpanded = True
              }
            , if model.isModelOverlayed == False then
                Task.perform identity (Task.succeed Dismiss)

              else
                Cmd.none
            )

        ToggleExpanded ->
            ( { model
                | isExpanded = not model.isExpanded
                , isModelOverlayed = model.isModelOverlayed && not model.isExpanded
                , position = updatePosition (not model.isExpanded) model.viewportSize model.position
              }
            , Cmd.none
            )

        SetHovering isHovering ->
            ( { model | isHovering = isHovering }, Cmd.none )

        SetDragging isDragging ->
            ( { model | isDragging = isDragging }, Cmd.none )

        ResizeViewport viewportSize ->
            ( { model
                | viewportSize = viewportSize
              }
            , Task.perform identity (Task.andThen (\_ -> Task.succeed Dismiss) (P.sleep 100))
            )

        DragTo position ->
            ( { model
                | position =
                    updatePosition
                        model.isExpanded
                        model.viewportSize
                        position
              }
            , Cmd.none
            )

        Dismiss ->
            ( { model
                | position =
                    updatePosition
                        model.isExpanded
                        model.viewportSize
                        { top = model.viewportSize.height
                        , left = model.viewportSize.width
                        }
              }
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )


toSubscriptions : (model -> Sub msg) -> Model model -> Sub (Msg msg)
toSubscriptions subscriptions model =
    Sub.batch
        [ Sub.map Update (subscriptions (Tuple.second model.updates.current))
        , Be.onResize (\width height -> ResizeViewport (Size width height))
        , if model.isDragging then
            Sub.batch
                [ Be.onMouseMove (Jd.map DragTo (mouseMoveDecoder model.isExpanded model.position))
                , Be.onMouseUp (Jd.succeed (SetDragging False))
                ]

          else
            Sub.none
        ]


blue : String
blue =
    "#60B5CC"


gray : String
gray =
    "#eeeeee"


unselectable : List (H.Attribute msg) -> List (Html msg) -> Html msg
unselectable attributes =
    H.div
        (attributes
            ++ [ Ha.style "-webkit-touch-callout" "none"
               , Ha.style "-webkit-user-select" "none"
               , Ha.style "-khtml-user-select" "none"
               , Ha.style "-moz-user-select" "none"
               , Ha.style "-ms-user-select" "none"
               , Ha.style "user-select" "none"
               ]
        )


toPx : Int -> String
toPx n =
    String.fromInt n ++ "px"


toBoxShadow : Bool -> String
toBoxShadow isHovering =
    if isHovering then
        gray ++ " 0px 0px 8px 2px"

    else
        gray ++ " 0px 0px 8px 1px"


toBorderRadius : Bool -> String
toBorderRadius isExpanded =
    if isExpanded then
        "0%"

    else
        "100%"


joinStrings : String -> List String -> String
joinStrings separator strings =
    case strings of
        head :: tails ->
            List.foldl (\curr prev -> prev ++ separator ++ curr) head tails

        [] ->
            ""


toHeight : Int -> Bool -> Int
toHeight messageCount isExpanded =
    if isExpanded then
        round (toFloat (messageCount + 3) * 19.09)

    else
        76


toWidth : Bool -> Int
toWidth isExpanded =
    if isExpanded then
        256

    else
        76


onRightClick : msg -> H.Attribute msg
onRightClick msg =
    He.preventDefaultOn "contextmenu" (Jd.succeed ( msg, True ))


viewSlider : Int -> Int -> Html (Msg msg)
viewSlider length currentIndex =
    H.div []
        [ H.input
            [ Ha.style "width" "92%"
            , Ha.style "margin" "0 4%"
            , Ha.type_ "range"
            , Ha.max (String.fromInt (length - 1))
            , Ha.min (String.fromInt 0)
            , Ha.value (String.fromInt currentIndex)
            , Ha.disabled (length == 1)
            , He.onInput (ToUpdateAt << Maybe.withDefault 0 << String.toInt)
            ]
            []
        ]


viewMessages : List ( Int, String ) -> Int -> Html (Msg msg)
viewMessages messages currentIndex =
    unselectable
        []
        (List.indexedMap (viewMessage currentIndex) messages)


fitText : Int -> String -> String
fitText maxLength text =
    if String.length text > maxLength then
        String.left (maxLength - 3) text ++ "..."

    else
        text


viewMessage : Int -> Int -> ( Int, String ) -> Html (Msg msg)
viewMessage currentIndex viewIndex ( index, label ) =
    H.div
        ([ useIf (viewIndex /= 0) (Ha.style "border-top" ("1px solid " ++ gray))
         , Ha.style "padding" "0px 4px"
         ]
            ++ (if currentIndex == index then
                    [ Ha.style "background-color" blue
                    , Ha.style "color" "white"
                    ]

                else
                    [ Ha.style "cursor" "pointer"
                    , He.onClick (ToUpdateAt index)
                    ]
               )
        )
        [ H.text (fitText 22 label)
        , H.span
            [ Ha.style "float" "right"
            , Ha.style "margin-left" "10px"
            ]
            [ H.text (fitText 3 (String.fromInt index)) ]
        ]


drag : List (H.Attribute (Msg msg)) -> List (Html (Msg msg)) -> Html (Msg msg)
drag attributes =
    H.div
        (attributes
            ++ [ Ha.style "cursor" "grab"
               , He.onMouseDown (SetDragging True)
               , He.onMouseUp (SetDragging False)
               ]
        )


toSelectedColor : Bool -> String
toSelectedColor isSelected =
    if isSelected then
        "white"

    else
        "black"


useIf : Bool -> H.Attribute msg -> H.Attribute msg
useIf predicate attribute =
    if predicate then
        attribute

    else
        Ha.style "" ""


useMaybe : Maybe value -> (value -> H.Attribute msg) -> H.Attribute msg
useMaybe maybe toAttribute =
    case maybe of
        Just value ->
            toAttribute value

        Nothing ->
            Ha.style "" ""


toSelectedBackgroundColor : Bool -> String
toSelectedBackgroundColor isSelected =
    if isSelected then
        blue

    else
        "initial"


viewModelOverlayToggleButton : String -> Bool -> Html (Msg msg)
viewModelOverlayToggleButton label isSelected =
    unselectable
        [ Ha.style "display" "inline-block"
        , Ha.style "text-align" "center"
        , Ha.style "width" "50%"
        , Ha.style "color" (toSelectedColor isSelected)
        , Ha.style "background-color" (toSelectedBackgroundColor isSelected)
        , useIf (not isSelected) (He.onClick ToggleModelOverlay)
        , useIf (not isSelected) (Ha.style "cursor" "pointer")
        ]
        [ H.text label
        ]


viewModelOverlayToggle : Bool -> Html (Msg msg)
viewModelOverlayToggle isModelOverlayed =
    H.div
        [ Ha.style "width" "100%"
        ]
        [ viewModelOverlayToggleButton "App" (not isModelOverlayed)
        , viewModelOverlayToggleButton "Model" isModelOverlayed
        ]


viewDebugger : Model model -> Html (Msg msg)
viewDebugger { updates, position, isExpanded, isDragging, isHovering, viewportSize, isModelOverlayed } =
    H.div
        ([ Ha.style "position" "fixed"
         , Ha.style "overflow" "hidden"
         , Ha.style "font-family" "system-ui"
         , Ha.style "background-color" "white"
         , Ha.style "z-index" "2147483647"
         , Ha.style "transition" <|
            joinStrings ", "
                [ "box-shadow 70ms linear"
                , "border-radius 70ms linear"
                , "width 70ms linear"
                , "height 140ms linear"
                , "top 40ms linear"
                , "left 40ms linear"
                , "background-color 0 linear 70ms"
                ]
         , Ha.style "width" (toPx (toWidth isExpanded))
         , Ha.style "border-radius" (toBorderRadius isExpanded)
         , Ha.style "box-shadow" (toBoxShadow isHovering)
         , Ha.style "left" (toPx position.left)
         , Ha.style "top" (toPx position.top)
         , He.onMouseEnter (SetHovering True)
         , He.onMouseLeave (SetHovering False)
         , onRightClick ToggleExpanded
         ]
            ++ (if isExpanded then
                    []

                else
                    [ He.onDoubleClick Dismiss
                    , He.onMouseDown (SetDragging True)
                    ]
               )
        )
        (if isExpanded then
            [ drag
                [ Ha.style "text-align" "center"
                , He.onDoubleClick Dismiss
                ]
                [ unselectable []
                    [ H.text "Debug"
                    ]
                ]
            , viewSlider (Zl.length updates) (List.length updates.tails)
            , viewModelOverlayToggle isModelOverlayed
            , viewMessages (Zl.toList (Zl.trim 10 (Zl.indexedMap (\index ( label, _ ) -> ( index, label )) updates))) (List.length updates.tails)
            ]

         else
            [ drag
                []
                [ unselectable
                    [ Ha.style "padding" "28.5px 0"
                    , Ha.style "text-align" "center"
                    , Ha.style "background-color" blue
                    , Ha.style "color" "white"
                    ]
                    [ H.text "Debug" ]
                ]
            ]
        )
