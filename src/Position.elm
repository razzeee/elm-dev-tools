module Position exposing (Position, add, clamp, fromSize, init, jsonDecoder, jsonEncode, mouseMoveDecoder, sub)

import Json.Decode as Jd
import Json.Encode as Je
import Size exposing (Size)


type alias Position =
    { left : Int
    , top : Int
    }


init : Position
init =
    Position 0 0


add : Position -> Position -> Position
add addition { left, top } =
    { left = left + addition.left
    , top = top + addition.top
    }


jsonEncode : Position -> Je.Value
jsonEncode { left, top } =
    Je.object
        [ ( "left", Je.int left )
        , ( "top", Je.int top )
        ]


jsonDecoder : Jd.Decoder Position
jsonDecoder =
    Jd.map2
        Position
        (Jd.field "left" Jd.int)
        (Jd.field "top" Jd.int)


sub : Position -> Position -> Position
sub subtraction { left, top } =
    { left = left - subtraction.left
    , top = top - subtraction.top
    }


clamp : Position -> Size -> Size -> Position -> Position
clamp offset outer inner position =
    { left = Basics.clamp offset.left (outer.width - inner.width) position.left
    , top = Basics.clamp offset.top (outer.height - inner.height) position.top
    }


fromSize : Size -> Position
fromSize { width, height } =
    { top = height
    , left = width
    }


mouseMoveDecoder : Position -> Jd.Decoder Position
mouseMoveDecoder { left, top } =
    Jd.map2
        Position
        (Jd.field "clientX" Jd.int)
        (Jd.field "clientY" Jd.int)
