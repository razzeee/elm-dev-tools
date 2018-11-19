module Size exposing (Size, fromViewport, map2)

import Browser.Dom


type alias Size =
    { width : Int
    , height : Int
    }


fromViewport : Browser.Dom.Viewport -> Size
fromViewport { viewport } =
    { width = round viewport.width
    , height = round viewport.height
    }


map2 : (Size -> msg) -> Int -> Int -> msg
map2 toMsg width height =
    toMsg
        { width = width
        , height = height
        }
