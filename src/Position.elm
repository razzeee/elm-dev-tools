module Position exposing (Position, mouseMoveDecoder)

import Json.Decode as Jd


type alias Position =
    { left : Int
    , top : Int
    }


mouseMoveDecoder : Position -> Jd.Decoder Position
mouseMoveDecoder { left, top } =
    Jd.map2
        Position
        (Jd.field "clientX" Jd.int)
        (Jd.field "clientY" Jd.int)
