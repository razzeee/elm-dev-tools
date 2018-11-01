module Main exposing (main)

import Browser exposing (..)
import Debug.Browser
import Html exposing (..)
import Html.Events exposing (..)
import String exposing (..)


type Msg
    = Increment
    | Decrement


init =
    { count = 0
    }


update msg model =
    { model
        | count =
            (+) model.count <|
                case msg of
                    Increment ->
                        1

                    Decrement ->
                        -1
    }


view model =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text (fromInt model.count) ]
        , button [ onClick Decrement ] [ text "-" ]
        ]


main =
    Debug.Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
