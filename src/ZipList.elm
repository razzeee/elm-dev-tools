module ZipList exposing
    ( ZipList
    , atHead
    , atTail
    , backward
    , dropHeads
    , forward
    , insert
    , length
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


insert : value -> ZipList value -> ZipList value
insert value zl =
    { zl
        | heads = zl.heads
        , current = value
        , tails = zl.current :: zl.tails
    }


dropHeads : ZipList value -> ZipList value
dropHeads zl =
    { zl | heads = [] }


length : ZipList value -> Int
length zl =
    List.length zl.heads + 1 + List.length zl.tails


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


toIndex : Int -> ZipList value -> ZipList value
toIndex index zl =
    let
        diff =
            List.length zl.tails - index

        op =
            if diff > 0 then
                backward

            else
                forward
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
