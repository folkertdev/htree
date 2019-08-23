module HTree exposing
    ( fromList, toList, toOutline, depth, nodeCount
    , tagWithDepth
    )

{-| Convert hierarchical lists to a rose tree,
convert a rose tree to a list of node labels,
convert a rose tree to a string representing the
corresponding outline, etc. See HTreeExample.elm.

@docs fromList, toList, toOutline, tag, depth, nodeCount

-}

import Tree exposing (Tree, singleton)
import Tree.Zipper as Zipper exposing (Zipper)


{-| Suppose give an list of items of type `a`, a "root" element
of type `a`, and a function that determines the level of an item.
The function `fromList` returns the corresponding rose tree.
For an example, consider the outline given by the string
\`Example.Test.o3:

    A
        p
        1
        2
        3
        q

    B
        r
        s

    C

Then we have

    > import HTree exposing(..)
    > import HTree.String as HS
    > import Example.Test as Example

    > data = String.split "\n" o2 |> List.filter (\line -> line /= "")
    ["A","  p","  q","B","  r","  s","C"]
        : List String

    > t = fromList "*" HS.level data
    Tree "*"
       [Tree "A" [Tree ("  p") [],Tree ("  q") []]
       ,Tree "B" [Tree ("  r") [],Tree ("  s") []]
       ,Tree "C" []]

The function `HS.level` returns the number of leading spaces divided by 2 --
integer division, no remainder.

-}
fromList : a -> (a -> Int) -> List a -> Tree a
fromList rooLabel level lst =
    lst
        |> List.foldl (\s z -> step level s z) (Zipper.fromTree (Tree.singleton rooLabel))
        |> Zipper.toTree


step : (a -> Int) -> a -> Zipper a -> Zipper a
step level s z =
    let
        ld =
            levelDifference level s z
    in
    case ld of
        Nothing ->
            appendAtFocus level s z

        Just 0 ->
            appendAtFocus level s z

        Just 1 ->
            addChildAtFocus level s z

        _ ->
            let
                levelsBack =
                    negate (ld |> Maybe.withDefault 0)
            in
            addAtNthParent level levelsBack s z


{-| The `tag` function transforms a tree of items into
a tree of tuples of the form `(a, k)`, where `k` is the
depth of `a` in the tree.

    > tag (Tree.map String.trim t)
      Tree ("*",0) [
        Tree ("A",1) [Tree ("p",2) [],Tree ("q",2) []]
       ,Tree ("B",1) [Tree ("r",2) [],Tree ("s",2) []]
       ,Tree ("C",1) []]

-}
tagWithDepth : Tree a -> Tree ( a, Int )
tagWithDepth t =
    tagWithDepth_ ( t, 0 )


tagWithDepth_ : ( Tree a, Int ) -> Tree ( a, Int )
tagWithDepth_ ( t, k ) =
    let
        c =
            Tree.children t
    in
    Tree.tree ( Tree.label t, k ) (List.map (\t_ -> tagWithDepth_ ( t_, k + 1 )) c)


{-| Map a tree to a list of node contents:

    > import Tree

    > t |> Tree.map (\label -> String.trim label)
      Tree "*" [
         Tree "A" [Tree "p" [],Tree "q" []]
        ,Tree "B" [Tree "r" [],Tree "s" []]
        ,Tree "C" []]

     > t |> Tree.map (\label -> String.trim label) |> toList
       ["*","A","p","q","B","r","s","C"]

-}
toList : Tree b -> List b
toList t =
    Tree.flatten t



-- ADDING THINGS --


appendAtFocus : (a -> Int) -> a -> Zipper a -> Zipper a
appendAtFocus level s z =
    let
        t =
            Zipper.tree z

        newTree =
            Tree.appendChild (singleton s) t
    in
    Zipper.replaceTree newTree z


addChildAtFocus : (a -> Int) -> a -> Zipper a -> Zipper a
addChildAtFocus level s z =
    case Zipper.lastChild z of
        Nothing ->
            z

        Just zz ->
            appendAtFocus level s zz


addChildAtParentOfFocus : (a -> Int) -> a -> Zipper a -> Zipper a
addChildAtParentOfFocus level s z =
    case Zipper.parent z of
        Nothing ->
            z

        Just zz ->
            appendAtFocus level s zz


addAtNthParent : (a -> Int) -> Int -> a -> Zipper a -> Zipper a
addAtNthParent level k s z =
    case manyParent k z of
        Nothing ->
            z

        Just zz ->
            appendAtFocus level s zz


nthParentOfFocus : Int -> Zipper a -> Zipper a
nthParentOfFocus k z =
    case manyParent k z of
        Nothing ->
            z

        Just zz ->
            zz



-- MOVING AROUND --


manyParent : Int -> Zipper a -> Maybe (Zipper a)
manyParent k z =
    let
        zz =
            Zipper.parent z
    in
    iterate (k - 1) (\zi -> Maybe.andThen Zipper.parent zi) zz


manyBackward : Int -> Zipper a -> Maybe (Zipper a)
manyBackward k z =
    if k < 0 then
        Nothing

    else if k == 0 then
        Just z

    else
        let
            zz =
                Zipper.backward z
        in
        iterate k (\zi -> Maybe.andThen Zipper.backward zi) zz


iterate : Int -> (a -> a) -> a -> a
iterate k f x =
    List.foldl (\i acc -> f acc) x (List.range 1 k)



-- LEVELS --


levelDifference : (a -> Int) -> a -> Zipper a -> Maybe Int
levelDifference level current zipper =
    Zipper.lastChild zipper
        |> Maybe.map (Zipper.tree >> Tree.label >> level)
        |> Maybe.map (\levelOfLastChild -> level current - levelOfLastChild)



-- STANDARD TREE FUNCTIONS --


{-| depth t is the depth of the tree.

    > depth t
    2 : I

-}
depth : Tree a -> Int
depth t =
    -- rose trees are always non-empty (have a root)
    -- so depth is always at least 1
    let
        childDepth =
            Tree.children t
                |> List.map depth
                |> List.maximum
                |> Maybe.withDefault 0
    in
    1 + childDepth


{-| nodecount t is the number of notes in the tree t

    > nodeCount t
    8 : Int

The count includes the root.

-}
nodeCount : Tree a -> Int
nodeCount =
    Tree.foldl (\_ accum -> accum + 1) 0



-- STRING REPRESENTATION --


{-| Given a function that maps labels to strings, construct
a string that represents the tree as an outline.

    > t |> toOutline identity
    "*\nA\n    p\n    q\nB\n    r\n    s\nC" : String

The string returned is

    *
        A
            p
            q
        B
            r
            s
        C

-}
toOutline : (a -> String) -> Tree a -> String
toOutline stringOfLabel t =
    t
        |> tagWithDepth
        |> toOutlineHelp stringOfLabel


toOutlineHelp : (a -> String) -> Tree ( a, Int ) -> String
toOutlineHelp labelToString =
    let
        combine : ( String, Int ) -> List String -> String
        combine ( currentLabel, currentLevel ) children =
            let
                prefix =
                    String.repeat (2 * (currentLevel - 1)) " "
            in
            case children of
                [] ->
                    prefix ++ currentLabel

                _ ->
                    prefix ++ currentLabel ++ "\n" ++ String.join "\n" children
    in
    Tree.restructure (Tuple.mapFirst labelToString) combine
