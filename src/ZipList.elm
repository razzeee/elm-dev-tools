module ZipList exposing
    ( ZipList
    , atHead
    , atTail
    , backward
    , dropHeads
    , forward
    , indexedMap
    , insert
    , length
    , map
    , singleton
    , toHead
    , toIndex
    , toList
    , toTail
    , trim
    )


type alias ZipList value =
    { heads : List value
    , current : value
    , tails : List value
    }


trim : Int -> ZipList value -> ZipList value
trim newLength zl =
    if length zl <= newLength then
        zl

    else if List.length zl.heads >= List.length zl.tails then
        trim newLength { zl | heads = List.take (List.length zl.heads - 1) zl.heads }

    else
        trim newLength { zl | tails = List.take (List.length zl.tails - 1) zl.tails }


map : (value -> a) -> ZipList value -> ZipList a
map op zl =
    { heads = List.map op zl.heads, current = op zl.current, tails = List.map op zl.tails }


indexedMap : (Int -> value -> a) -> ZipList value -> ZipList a
indexedMap op zl =
    { heads = List.indexedMap (\headIndex value -> op (headIndex + 1 + List.length zl.tails) value) zl.heads
    , current = op (List.length zl.tails) zl.current
    , tails = List.reverse (List.indexedMap op (List.reverse zl.tails))
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
    List.reverse zl.tails ++ [ zl.current ] ++ zl.heads


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
