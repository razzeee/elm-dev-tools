module Debug.Internals exposing (Program, toInit, toUpdate, toView)

import Browser
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import ZipList exposing (ZipList)


type alias Model model =
    { updates : ZipList ( String, model )
    }


type Msg msg
    = Update msg
    | ToUpdateAt Int


type alias Program flags model msg =
    Platform.Program flags (Model model) (Msg msg)


toInit : model -> Model model
toInit model =
    { updates = ZipList.singleton ( "Init", model )
    }


toUpdate : (msg -> model -> model) -> Msg msg -> Model model -> Model model
toUpdate update msg model =
    case msg of
        Update updateMsg ->
            let
                newUpdate =
                    ( Debug.toString updateMsg, update updateMsg (Tuple.second model.updates.current) )
            in
            { model | updates = ZipList.dropHeads (ZipList.insert newUpdate model.updates) }

        ToUpdateAt index ->
            { model | updates = ZipList.toIndex index model.updates }


toView : (model -> Html msg) -> Model model -> Html (Msg msg)
toView viewApp model =
    H.div []
        [ H.map Update (viewApp (Tuple.second model.updates.current))
        , viewDebugger model
        ]


viewDebugger : Model model -> Html (Msg msg)
viewDebugger model =
    let
        currentUpdateIndex =
            List.length model.updates.tails
    in
    H.div
        [ Ha.style "font-family"
            "'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif"
        , Ha.style "position" "fixed"
        , Ha.style "bottom" "0"
        , Ha.style "right" "0"
        , Ha.style "border" "1px solid #eeeeee"
        ]
        [ viewMessages (List.map Tuple.first (ZipList.toList model.updates)) currentUpdateIndex
        , viewModel (Tuple.second model.updates.current)
        , viewSlider (ZipList.length model.updates) currentUpdateIndex
        ]


viewSlider : Int -> Int -> Html (Msg msg)
viewSlider length currentIndex =
    H.div []
        [ H.input
            [ Ha.type_ "range"
            , Ha.max (String.fromInt (length - 1))
            , Ha.min (String.fromInt 0)
            , Ha.value (String.fromInt currentIndex)
            , He.onInput (ToUpdateAt << Maybe.withDefault 0 << String.toInt)
            ]
            []
        ]


viewMessage : Int -> Int -> String -> Html (Msg msg)
viewMessage currentIndex index msgText =
    H.div
        ([ Ha.style "border-bottom" "1px solid #eeeeee"
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
        [ H.text msgText
        , H.span
            [ Ha.style "float" "right"
            ]
            [ H.text (String.fromInt index) ]
        ]


viewMessages : List String -> Int -> Html (Msg msg)
viewMessages messages currentIndex =
    H.div [] (List.indexedMap (viewMessage currentIndex) messages)


viewModel : model -> Html (Msg msg)
viewModel model =
    H.div
        [ Ha.style "padding" "4px"
        ]
        [ H.text (Debug.toString model)
        ]
