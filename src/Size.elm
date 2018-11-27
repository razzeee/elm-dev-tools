module Size exposing (Size, fromViewport, jsonDecoder, jsonEncode, mapFromInts)

import Browser.Dom exposing (Viewport)
import Json.Decode as Jd
import Json.Encode as Je


type alias Size =
    { width : Int
    , height : Int
    }


fromViewport : Viewport -> Size
fromViewport { viewport } =
    { width = round viewport.width
    , height = round viewport.height
    }


mapFromInts : (Size -> msg) -> Int -> Int -> msg
mapFromInts toMsg width height =
    toMsg
        { width = width
        , height = height
        }


jsonEncode : Size -> Je.Value
jsonEncode { width, height } =
    Je.object
        [ ( "width", Je.int width )
        , ( "height", Je.int height )
        ]


jsonDecoder : Jd.Decoder Size
jsonDecoder =
    Jd.map2
        Size
        (Jd.field "width" Jd.int)
        (Jd.field "height" Jd.int)
