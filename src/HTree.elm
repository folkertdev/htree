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
fromList root toLevel list =
    list
        |> List.foldl (step toLevel) (Zipper.fromTree (Tree.singleton root))
        |> Zipper.toTree


step : (a -> Int) -> a -> Zipper a -> Zipper a
step toLevel s z =
    case levelDifference toLevel s z of
        Nothing ->
            appendAtFocus toLevel s z

        Just 0 ->
            appendAtFocus toLevel s z

        Just 1 ->
            addChildAtFocus toLevel s z

        Just levelsBack ->
            addAtNthParent toLevel (negate levelsBack) s z


{-| The `tag` function transforms a tree of items into
a tree of tuples of the form `(a, k)`, where `k` is the
depth of `a` in the tree.

    > tag (Tree.map String.trim t)
      Tree ("*",0) [
        Tree ("A",1) [Tree ("p",2) [],Tree ("q",2) []]
       ,Tree ("B",1) [Tree ("r",2) [],Tree ("s",2) []]
       ,Tree ("C",1) []]

-}
tagWithDepth : Tree a -> Tree { label : a, depth : Int }
tagWithDepth t =
    tagWithDepthHelp t 0


tagWithDepthHelp : Tree a -> Int -> Tree { label : a, depth : Int }
tagWithDepthHelp t k =
    let
        children =
            Tree.children t
                |> List.map (\subtree -> tagWithDepthHelp subtree (k + 1))
    in
    Tree.tree { label = Tree.label t, depth = k } children


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
    Zipper.lastChild z
        |> Maybe.map (appendAtFocus level s)
        |> Maybe.withDefault z


addChildAtParentOfFocus : (a -> Int) -> a -> Zipper a -> Zipper a
addChildAtParentOfFocus level s z =
    Zipper.parent z
        |> Maybe.map (appendAtFocus level s)
        |> Maybe.withDefault z


addAtNthParent : (a -> Int) -> Int -> a -> Zipper a -> Zipper a
addAtNthParent toLevel k s z =
    manyParent k z
        |> Maybe.map (appendAtFocus toLevel s)
        |> Maybe.withDefault z


nthParentOfFocus : Int -> Zipper a -> Zipper a
nthParentOfFocus k z =
    manyParent k z
        |> Maybe.withDefault z



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
        -- Question: aren't we doing k+1 backwards here? zz already has 1 applied, then we do k more
        iterate k (\zi -> Maybe.andThen Zipper.backward zi) zz


iterate : Int -> (a -> a) -> a -> a
iterate remaining f accumulator =
    if remaining > 0 then
        iterate (remaining - 1) f (f accumulator)

    else
        accumulator



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


toOutlineHelp : (a -> String) -> Tree { label : a, depth : Int } -> String
toOutlineHelp labelToString =
    let
        combineChildren : { label : String, depth : Int } -> List String -> String
        combineChildren node children =
            let
                prefix =
                    String.repeat (2 * (node.depth - 1)) " "
            in
            case children of
                [] ->
                    prefix ++ node.label

                _ ->
                    prefix ++ node.label ++ "\n" ++ String.join "\n" children

        mapLabel : { label : a, depth : Int } -> { label : String, depth : Int }
        mapLabel node =
            { label = labelToString node.label, depth = node.depth }
    in
    Tree.restructure mapLabel combineChildren
