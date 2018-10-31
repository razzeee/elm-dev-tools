module ZipList exposing
    ( ZipList
    , append
    , atHead
    , atTail
    , backward
    , forward
    , prepend
    , singleton
    , toHead
    , toIndex
    , toList
    , toTail
    )


type alias ZipList value =
    { heads : List value
    , current : value
    , tails : List value
    }


singleton : value -> ZipList value
singleton value =
    { heads = []
    , current = value
    , tails = []
    }


toList : ZipList value -> List value
toList zl =
    List.reverse zl.heads ++ [ zl.current ] ++ zl.tails


atHead : ZipList value -> Bool
atHead zl =
    zl.heads == []


atTail : ZipList value -> Bool
atTail zl =
    zl.tails == []


append : value -> ZipList value -> ZipList value
append value zl =
    { zl
        | heads = toList zl
        , current = value
        , tails = []
    }


prepend : value -> ZipList value -> ZipList value
prepend value zl =
    { zl
        | heads = []
        , current = value
        , tails = toList zl
    }


toIndex : Int -> ZipList value -> ZipList value
toIndex index zl =
    let
        diff =
            List.length zl.heads - index

        op =
            if diff > 0 then
                forward

            else
                backward
    in
    zl
        |> List.foldl
            (>>)
            identity
            (List.repeat (abs diff) op)


toHead : ZipList value -> ZipList value
toHead zl =
    case zl.heads of
        [] ->
            zl

        newCurrent :: newHeads ->
            toHead
                { zl
                    | heads = newHeads
                    , current = newCurrent
                    , tails = zl.current :: zl.tails
                }


toTail : ZipList value -> ZipList value
toTail zl =
    case zl.tails of
        [] ->
            zl

        newCurrent :: newTails ->
            toTail
                { zl
                    | heads = zl.current :: zl.heads
                    , current = newCurrent
                    , tails = newTails
                }


forward : ZipList value -> ZipList value
forward zl =
    case zl.heads of
        [] ->
            zl

        newCurrent :: newHeads ->
            { heads = newHeads
            , current = newCurrent
            , tails = zl.current :: zl.tails
            }


backward : ZipList value -> ZipList value
backward zl =
    case zl.tails of
        [] ->
            zl

        newCurrent :: newTails ->
            { heads = zl.current :: zl.heads
            , current = newCurrent
            , tails = newTails
            }
