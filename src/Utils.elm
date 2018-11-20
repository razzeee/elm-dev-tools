module Utils exposing
    ( attributeIf
    , border
    , darkGray
    , isOdd
    , join
    , msgToCmd
    , noHtml
    , onRightClick
    , selectable
    , subscribeIf
    , toBackgroundColor
    , toIconColor
    , toListBackgroundColor
    , toListTextColor
    , toPx
    , toTextColor
    , trim
    , viewIf
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Task


trim : Int -> String -> String
trim maxLength text =
    if String.length text > maxLength then
        String.left (maxLength - 3) text ++ "..."

    else
        text


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    Html.Events.preventDefaultOn "contextmenu" (Json.Decode.succeed ( msg, True ))


selectable : Bool -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
selectable isSelectable attributes =
    Html.div
        (if isSelectable then
            attributes

         else
            attributes
                ++ [ Html.Attributes.style "-webkit-touch-callout" "none"
                   , Html.Attributes.style "-webkit-user-select" "none"
                   , Html.Attributes.style "-khtml-user-select" "none"
                   , Html.Attributes.style "-moz-user-select" "none"
                   , Html.Attributes.style "-ms-user-select" "none"
                   , Html.Attributes.style "user-select" "none"
                   ]
        )


border : String
border =
    "1px solid #d3d3d3"


darkGray : String
darkGray =
    "#f3f3f3"


toIconColor : Bool -> Bool -> String
toIconColor isHovered isSelected =
    if isSelected then
        "#1cabf1"

    else if isHovered then
        "black"

    else
        "#7c7c7c"


toTextColor : Bool -> Bool -> String
toTextColor isHovered isSelected =
    if isSelected || isHovered then
        "black"

    else
        "#555555"


toListTextColor : Bool -> String
toListTextColor isSelected =
    if isSelected then
        "white"

    else
        "black"


toListBackgroundColor : Bool -> Bool -> Bool -> String
toListBackgroundColor isOdd isHovered isSelected =
    if isSelected then
        "#1cabf1"

    else if isOdd || isHovered then
        "#f5f5f5"

    else
        "white"


toBackgroundColor : Bool -> Bool -> String
toBackgroundColor isHovered isSelected =
    if isSelected then
        "rgba(0,0,0,0)"

    else if isHovered then
        "rgba(0,0,0,.03)"

    else
        "rgba(0,0,0,0)"


toPx : Int -> String
toPx n =
    String.fromInt n ++ "px"


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity (Task.succeed msg)


noHtml : Html msg
noHtml =
    Html.text ""


noAttribute : Html.Attribute msg
noAttribute =
    Html.Attributes.style "" ""


attributeIf : Bool -> Html.Attribute msg -> Html.Attribute msg
attributeIf predicate attribute =
    if predicate then
        attribute

    else
        noAttribute


viewIf : Bool -> Html msg -> Html msg
viewIf isVisible html =
    if isVisible then
        html

    else
        noHtml


subscribeIf : Bool -> Sub msg -> Sub msg
subscribeIf isSubscribing subscribe =
    if isSubscribing then
        subscribe

    else
        Sub.none


join : (a -> a -> a) -> a -> List a -> a
join add unit list =
    case list of
        head :: tails ->
            List.foldl add head tails

        [] ->
            unit


isOdd : Int -> Bool
isOdd n =
    modBy 2 n == 1
