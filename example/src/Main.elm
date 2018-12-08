port module Main exposing (main)

import Browser
import DevTools.Browser
import Html as H exposing (Html)
import Html.Attributes as Ha
import Html.Events as He
import Json.Decode as Jd
import Json.Encode as Je
import Time


port output : Je.Value -> Cmd msg


type Msg
    = NameInput String
    | PassInput String
    | CountInput Int
    | LogIn
    | LogOut


type Page
    = Authentication { name : String, pass : String }
    | Counter { name : String, count : Int }


init _ =
    ( { page = Authentication { name = "", pass = "" }
      }
    , Cmd.none
    )


update msg model =
    case ( msg, model.page ) of
        ( NameInput name, Authentication state ) ->
            ( { model | page = Authentication { state | name = name } }, Cmd.none )

        ( PassInput pass, Authentication state ) ->
            ( { model | page = Authentication { state | pass = pass } }, Cmd.none )

        ( CountInput count, Counter state ) ->
            ( { model | page = Counter { state | count = count } }, Cmd.none )

        ( LogIn, Authentication state ) ->
            ( { model | page = Counter { name = state.name, count = 0 } }, Cmd.none )

        ( LogOut, Counter state ) ->
            ( { model | page = Authentication { name = "", pass = "" } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions model =
    case model.page of
        Counter state ->
            Time.every 50 (always (CountInput (state.count + 1)))

        _ ->
            Sub.none


view model =
    { title = "Example"
    , body =
        H.div
            [ Ha.style "display" "flex"
            , Ha.style "flex-direction" "column"
            , Ha.style "justify-content" "center"
            , Ha.style "align-items" "center"
            , Ha.style "height" "95vh"
            , Ha.style "width" "95vw"
            , Ha.style "overflow" "hidden"
            ]
            (case model.page of
                Counter state ->
                    [ H.div [] [ H.text ("Hello " ++ state.name) ]
                    , H.button [ He.onClick LogOut ] [ H.text "Log Out" ]
                    , H.div []
                        [ H.button [ He.onClick (CountInput (state.count + 1)) ] [ H.text "+" ]
                        , H.div [] [ H.text (String.fromInt state.count) ]
                        , H.button [ He.onClick (CountInput (state.count - 1)) ] [ H.text "-" ]
                        ]
                    ]

                Authentication state ->
                    [ H.input
                        [ Ha.type_ "text"
                        , Ha.value state.name
                        , He.onInput NameInput
                        ]
                        []
                    , H.input
                        [ Ha.type_ "password"
                        , Ha.value state.pass
                        , He.onInput PassInput
                        ]
                        []
                    , H.button
                        [ Ha.disabled (String.length state.pass < 1 || String.length state.name < 1)
                        , He.onClick LogIn
                        ]
                        [ H.text "Log In" ]
                    ]
            )
            :: []
    }


msgDecoder =
    Jd.oneOf
        [ Jd.field "NameInput" (Jd.map NameInput Jd.string)
        , Jd.field "PassInput" (Jd.map PassInput Jd.string)
        , Jd.field "CountInput" (Jd.map CountInput Jd.int)
        , Jd.field "LogIn" (Jd.null LogIn)
        , Jd.field "LogOut" (Jd.null LogOut)
        ]


encodeMsg msg =
    case msg of
        NameInput text ->
            Je.object [ ( "NameInput", Je.string text ) ]

        PassInput text ->
            Je.object [ ( "PassInput", Je.string text ) ]

        CountInput count ->
            Je.object [ ( "CountInput", Je.int count ) ]

        LogIn ->
            Je.object [ ( "LogIn", Je.null ) ]

        LogOut ->
            Je.object [ ( "LogOut", Je.null ) ]


debug =
    { printModel = Debug.toString
    , output = output
    , msgDecoder = msgDecoder
    , encodeMsg = encodeMsg
    }


main =
    DevTools.Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , debug = debug
        }
