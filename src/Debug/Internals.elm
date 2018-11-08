module Debug.Internals exposing (Program, doNothing, toDocument, toHtml, toInit, toMsg, toSubscriptions, toUpdate)

import Browser
import Browser.Dom as Bd
import Browser.Events as Be
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd
import Json.Encode as Je
import Task exposing (Task)
import ZipList as Zl exposing (ZipList)


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
    , isHovered : Bool
    }


type Msg msg
    = Update msg
    | ToUpdateAt Int
    | ToggleExpanded
    | ToggleDragging Bool
    | ToggleHovered Bool
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
      , position = { left = 300, top = 300 }
      , viewportSize = { width = 0, height = 0 }
      , isExpanded = False
      , isDragging = False
      , isHovered = False
      }
    , Cmd.batch
        [ Cmd.map Update cmd
        , Task.perform ResizeViewport (Task.map viewportToSize Bd.getViewport)
        ]
    )


toHtml : (model -> Html msg) -> Model model -> Html (Msg msg)
toHtml view model =
    H.div []
        [ H.map Update (view (Tuple.second model.updates.current))
        , viewDebugger model
        ]


toDocument : (model -> Browser.Document msg) -> Model model -> Browser.Document (Msg msg)
toDocument view model =
    { title = "Debug"
    , body = viewDebugger model :: List.map (H.map Update) (.body (view (Tuple.second model.updates.current)))
    }


mouseMoveDecoder : Bool -> Position -> Jd.Decoder Position
mouseMoveDecoder isExpanded { left, top } =
    Jd.map2
        Position
        (Jd.field "clientX" Jd.int)
        (Jd.field "clientY" Jd.int)


updatePosition : Bool -> Size -> Position -> Position
updatePosition isExpanded { height, width } { top, left } =
    { top = clamp 0 (height - toHeight isExpanded) top
    , left = clamp 0 (width - toWidth isExpanded) left
    }


toUpdate : (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
toUpdate update msg model =
    case msg of
        Update updateMsg ->
            let
                ( newModel, cmd ) =
                    update updateMsg (Tuple.second model.updates.current)

                updates =
                    Zl.dropHeads (Zl.insert ( Debug.toString updateMsg, newModel ) model.updates)
            in
            ( { model | updates = updates }
            , Cmd.map Update cmd
            )

        ToUpdateAt index ->
            ( { model | updates = Zl.toIndex index model.updates }, Cmd.none )

        ToggleExpanded ->
            ( { model
                | isExpanded = not model.isExpanded
                , position = updatePosition (not model.isExpanded) model.viewportSize model.position
              }
            , Cmd.none
            )

        ToggleHovered isHovered ->
            ( { model | isHovered = isHovered }, Cmd.none )

        ToggleDragging isDragging ->
            ( { model | isDragging = isDragging }, Cmd.none )

        ResizeViewport viewportSize ->
            ( { model
                | viewportSize = viewportSize
                , position = updatePosition model.isExpanded viewportSize model.position
              }
            , Cmd.none
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
                , Be.onMouseUp (Jd.succeed (ToggleDragging False))
                ]

          else
            Sub.none
        ]


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
toBoxShadow isHovered =
    if isHovered then
        "rgba(0,0,0,.3) 0px 0px 6px 1px"

    else
        "rgba(0,0,0,.2) 0px 0px 4px 1px"


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


toHeight : Bool -> Int
toHeight isExpanded =
    if isExpanded then
        256

    else
        76


toWidth : Bool -> Int
toWidth =
    toHeight


onRightClick : msg -> H.Attribute msg
onRightClick msg =
    He.preventDefaultOn "contextmenu" (Jd.succeed ( msg, True ))


viewSlider : Int -> Int -> Html (Msg msg)
viewSlider length currentIndex =
    H.div []
        [ H.input
            [ Ha.style "width" "95%"
            , Ha.type_ "range"
            , Ha.max (String.fromInt (length - 1))
            , Ha.min (String.fromInt 0)
            , Ha.value (String.fromInt currentIndex)
            , Ha.disabled (length == 1)
            , He.onInput (ToUpdateAt << Maybe.withDefault 0 << String.toInt)
            ]
            []
        ]


viewModel : model -> Html (Msg msg)
viewModel model =
    H.div
        [ Ha.style "padding" "4px"
        ]
        [ H.text (Debug.toString model)
        ]


viewMessages : List String -> Int -> Html (Msg msg)
viewMessages messages currentIndex =
    H.div
        []
        (List.indexedMap (viewMessage currentIndex) messages)


viewMessage : Int -> Int -> String -> Html (Msg msg)
viewMessage currentIndex index label =
    H.div
        ([ Ha.style "border-top" "1px solid #eeeeee"
         , Ha.style "padding" "0px 4px"
         ]
            ++ (if currentIndex == index then
                    [ Ha.style "background-color" "#60B5CC"
                    , Ha.style "color" "white"
                    ]

                else
                    [ Ha.style "cursor" "pointer"
                    , He.onClick (ToUpdateAt index)
                    ]
               )
        )
        [ H.text label
        , H.span
            [ Ha.style "float" "right"
            , Ha.style "margin-left" "10px"
            ]
            [ H.text (String.fromInt index) ]
        ]


viewDebugger : Model model -> Html (Msg msg)
viewDebugger { updates, position, isExpanded, isDragging, isHovered, viewportSize } =
    H.div
        ([ Ha.style "position" "fixed"
         , Ha.style "background-color" "white"
         , Ha.style "cursor" "pointer"
         , Ha.style "width" (toPx (toWidth isExpanded))
         , Ha.style "height" (toPx (toHeight isExpanded))
         , Ha.style "border-radius" (toBorderRadius isExpanded)
         , Ha.style "box-shadow" (toBoxShadow isHovered)
         , Ha.style "left" (toPx position.left)
         , Ha.style "top" (toPx position.top)
         , Ha.style "transition" <|
            joinStrings ", "
                [ "box-shadow 70ms linear"
                , "border-radius 70ms linear"
                , "width 140ms linear 70ms"
                , "height 140ms linear 70ms"
                , "transform 140ms linear 70ms"
                ]
         , onRightClick ToggleExpanded
         , He.onMouseEnter (ToggleHovered True)
         , He.onMouseLeave (ToggleHovered False)
         ]
            ++ (if isExpanded then
                    []

                else
                    [ He.onDoubleClick Dismiss
                    , He.onMouseDown (ToggleDragging True)
                    ]
               )
        )
        (if isExpanded then
            [ viewSlider (Zl.length updates) (List.length updates.tails)
            , viewModel (Tuple.second updates.current)
            , viewMessages (List.map Tuple.first (Zl.toList updates)) (List.length updates.tails)
            ]

         else
            [ unselectable
                [ Ha.style "padding" "28.5px 0"
                , Ha.style "text-align" "center"
                ]
                [ H.text "Debug" ]
            ]
        )
