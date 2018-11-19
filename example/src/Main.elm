module Main exposing (main)

import Browser exposing (..)
import Debug.Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (..)


type Msg
    = NameInput String
    | PassInput String
    | CountInput Int
    | LogIn
    | LogOut


type Page
    = Authentication { name : String, pass : String }
    | Counter { name : String, count : Int }


init =
    { page = Authentication { name = "", pass = "" }
    }


update msg model =
    case msg of
        NameInput name ->
            case model.page of
                Authentication state ->
                    { model | page = Authentication { state | name = name } }

                _ ->
                    model

        PassInput pass ->
            case model.page of
                Authentication state ->
                    { model | page = Authentication { state | pass = pass } }

                _ ->
                    model

        CountInput count ->
            case model.page of
                Counter state ->
                    { model | page = Counter { state | count = count } }

                _ ->
                    model

        LogIn ->
            case model.page of
                Authentication state ->
                    { model | page = Counter { name = state.name, count = 0 } }

                _ ->
                    model

        LogOut ->
            case model.page of
                Counter _ ->
                    { model | page = Authentication { name = "", pass = "" } }

                _ ->
                    model


view model =
    case model.page of
        Counter state ->
            div []
                [ text ("Hello " ++ state.name)
                , button [ onClick LogOut ] [ text "Log Out" ]
                , div []
                    [ button [ onClick (CountInput (state.count + 1)) ] [ text "+" ]
                    , div [] [ text (fromInt state.count) ]
                    , button [ onClick (CountInput (state.count - 1)) ] [ text "-" ]
                    ]
                ]

        Authentication state ->
            div []
                [ input
                    [ type_ "text"
                    , value state.name
                    , onInput NameInput
                    ]
                    []
                , input
                    [ type_ "password"
                    , value state.pass
                    , onInput PassInput
                    ]
                    []
                , button
                    [ disabled (String.length state.pass < 1 || String.length state.name < 1)
                    , onClick LogIn
                    ]
                    [ text "Log In" ]
                ]


debug =
    { printMessage = Debug.toString
    , printModel = Debug.toString
    , messageDecoder =
        Decode.oneOf
            [ Decode.field "NameInput" (Decode.map NameInput Decode.string)
            , Decode.field "PassInput" (Decode.map PassInput Decode.string)
            , Decode.field "CountInput" (Decode.map CountInput Decode.int)
            , Decode.field "LogIn" (Decode.null LogIn)
            , Decode.field "LogOut" (Decode.null LogOut)
            ]
    , encodeMessage =
        \msg ->
            case msg of
                NameInput text ->
                    Encode.object [ ( "NameInput", Encode.string text ) ]

                PassInput text ->
                    Encode.object [ ( "PassInput", Encode.string text ) ]

                CountInput count ->
                    Encode.object [ ( "CountInput", Encode.int count ) ]

                LogIn ->
                    Encode.object [ ( "LogIn", Encode.null ) ]

                LogOut ->
                    Encode.object [ ( "LogOut", Encode.null ) ]
    , commands =
        [ ( "To Count", [ NameInput "someone", LogIn, CountInput 1337 ] )
        ]
    }


main =
    Debug.Browser.sandbox
        { init = init
        , update = update
        , view = view
        , debug = debug
        }
