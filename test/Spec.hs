import Test.Tasty
import Test.Tasty.HUnit

import Debug.Trace
import Data.Maybe
import Webbi.Utils.RoseTree (RoseTree(..), datum)
import Webbi.Utils.TreeZipper

main :: IO ()
main = do
--    traceShowM (up =<< (up lol))
    traceShowM secondChild2'
    defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests]

roseLeaf = RoseTree 3 []
roseLeaf1 = RoseTree 2 []
roseTree = RoseTree 1 [roseLeaf1, roseLeaf]
roseTree2 = RoseTree 10 [RoseTree 20 [], RoseTree 30 []]
root = Root [roseTree, roseTree2]
firstChild' = firstChild root
secondChild' = nextSibling =<< firstChild'
firstFirstChild' = firstChild =<< firstChild'
firstSecondChild' = nextSibling =<< firstFirstChild'


roseTree3 = RoseTree 100 [RoseTree 200 [], RoseTree 300 []]
root2 = Root [roseTree, roseTree2, roseTree3 ]
thirdChild' = down 100 root2
secondChild2' = previousSibling =<< thirdChild'
firstChild2' = previousSibling =<< secondChild2'

--- test all these as props
unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "Up (on root)" $
        up root @?= Nothing

    , testCase "Up (on firstChild)" $
        (up =<< firstChild') @?= (Just root)

    , testCase "Up (on firstFirstChild)" $
        (up =<< firstFirstChild') @?= (down 1 root)

    , testCase "Down (on root)" $
        down 1 root @?= Just (Tree (RoseTree 1 [RoseTree 2 [],RoseTree 3 []]) [] [] [RoseTree 10 [RoseTree 20 [],RoseTree 30 []]])

    , testCase "Down (on firstChild)" $
        (down 2 =<< firstChild') @?= Just (Tree (RoseTree 2 []) [Context [] 1 [RoseTree 3 []]] [] [RoseTree 10 [RoseTree 20 [],RoseTree 30 []]])

    , testCase "Down (on firstFirstChild)" $
        (down 2 =<< firstFirstChild') @?= Nothing

    , testCase "Rights' (on root)" $
        (rights' root) @?= []

    , testCase "Rights' (on firstChild)" $
        (rights' <$> firstChild') @?= Just [roseTree2]

    , testCase "Rights' (on firstFirstChild)" $
        (rights' <$> firstFirstChild') @?= Just [roseLeaf]

    , testCase "Rights (on root)" $
        rights root @?= []

    , testCase "Rights (on firstChild)" $
        (rights <$> firstChild') @?= sequence [down 10 root]

    , testCase "Rights (on firstFirstChild)" $
        (rights <$> firstFirstChild') @?= sequence [down 3 =<< down 1 root]

    , testCase "Lefts (on root)" $
        lefts root @?= []

    , testCase "Lefts (on firstChild)" $
        (lefts <$> firstChild') @?= (Just [])

    , testCase "Lefts (on firstFirstChild)" $
        (lefts <$> firstFirstChild') @?= (Just [])

    , testCase "Children (on root)" $
        children root @?= (catMaybes [down 1 root, down 10 root])

    , testCase "Children (on firstChild)" $
        (children <$> firstChild') @?= (Just (catMaybes [down 2 =<< down 1 root, down 3 =<< down 1 root]))

    , testCase "Children (on firstFirstChild)" $
        (children <$> firstFirstChild') @?= (Just [])

    , testCase "firstChild (on root)" $
        firstChild root @?= (down 1 root)

    , testCase "firstChild (on firstChild)" $
        (firstChild =<< firstChild') @?= (down 2 =<< down 1 root)

    , testCase "firstChild (on firstFirstChild)" $
        (firstChild =<< firstFirstChild') @?= Nothing

    , testCase "toRoseTree (on root)" $
        toRoseTree root @?= Nothing

    , testCase "toRoseTree (on firstChild)" $
        (toRoseTree =<< firstChild') @?= Just roseTree

    , testCase "toRoseTree (on firstFirstChild)" $
        (toRoseTree =<< firstFirstChild') @?= Just roseLeaf1

    , testCase "siblings (on root)" $
        siblings root @?= []

    , testCase "sibling (on firstChild)" $
        (siblings <$> firstChild') @?= sequence [down 10 root]

    , testCase "sibling (on firstFirstChild)" $
        (siblings <$> firstFirstChild') @?= sequence [down 3 =<< down 1 root]

    , testCase "nextSibling (on root)" $
        nextSibling root @?= Nothing

    , testCase "nextSibling (on firstChild)" $
        (nextSibling =<< firstChild') @?= (down 10 root)

    , testCase "nextSibling (on firstFirstChild)" $
        (nextSibling =<< firstFirstChild') @?= (down 3 =<< down 1 root)

    , testCase "previousSibling (on root)" $
        previousSibling root @?= Nothing

    , testCase "previousSibling (on secondChild')" $
        (previousSibling =<< secondChild') @?= firstChild'

    , testCase "previousSibling (on secondFirstChild')" $
        (previousSibling =<< firstSecondChild') @?= firstFirstChild'

    , testCase "nextSiblingOfAncestor (on root)" $
        nextSiblingOfAncestor root @?= Nothing

    , testCase "nextSiblingOfAncestor (on firstChild)" $
        (nextSiblingOfAncestor =<< firstChild') @?= Nothing

    , testCase "nextSiblingOfAncestor (on firstFirstChild)" $
        (nextSiblingOfAncestor =<< firstFirstChild') @?= (down 10 root)

    , testCase "leafs (on root)" $
        leafs root @?= []

    , testCase "leafs (on firstChild)" $
        leafs <$> firstChild' @?= sequence [down 2 =<< firstChild', down 3 =<< firstChild']

    , testCase "leafs (on firstFirstChild)" $
        leafs <$> firstFirstChild' @?= (Just [])

    , testCase "forward (on root)" $
        forward root @?= (firstChild root)

    , testCase "forward (on firstChild)" $
        (forward =<< firstChild') @?= (down 2 =<< firstChild')

    , testCase "forward (on firstFirstChild)" $
        (forward =<< firstFirstChild') @?= (down 3 =<< firstChild')

    , testCase "hierachy (on root)" $
        hierachy root @?= [siblings root]

    , testCase "hierachy (on firstChild)" $
        (hierachy <$> firstChild') @?= sequence [Just (siblings root), siblings <$> firstChild']

    , testCase "hierachy (on firstFirstChild)" $
        (hierachy <$> firstFirstChild') @?= sequence [Just (siblings root), siblings <$> firstChild', siblings <$> firstFirstChild']

    




    --- NEWSHIT
    , testCase "lefts (thirdChild')" $
        (lefts' <$> thirdChild') @?= Just [RoseTree 10 [RoseTree 20 [],RoseTree 30 []],RoseTree 1 [RoseTree 2 [],RoseTree 3 []]]

    , testCase "previousSibling (thirdChild)" $
        (previousSibling =<< thirdChild') @?= (down 10 root2)

    , testCase "previousSibling (thirdChild)" $
        (previousSibling =<< (secondChild2')) @?= (down 1 root2)

    , testCase "sibling (on lol)" $
        fmap (\x -> fmap datum (toRoseTree x)) (siblings lol) @?= [Just "chair/"]


    , testCase "sibling (on upup')" $
        fmap (\x -> fmap datum (toRoseTree x)) (siblings upup') @?= []

    ]

lol = Tree (RoseTree "index.html" [])
        [Context [RoseTree "chair/" [RoseTree "index.html" []]] "projects/" []]
        [ RoseTree "about/" [RoseTree "index.html" []]
        , RoseTree "contact/" [RoseTree "index.html" []]
        , RoseTree "index.html" [],RoseTree "posts/"
                [RoseTree "applicative/" [RoseTree "index.html" []]
                ,RoseTree "bifunctors/" [RoseTree "index.html" []]
                ,RoseTree "functors/" [RoseTree "index.html" []]
                ,RoseTree "index.html" []
                ,RoseTree "monads/" [RoseTree "index.html" []]
                ,RoseTree "profunctors/" [RoseTree "index.html" []]
                ]
        ]
        [ RoseTree "test/" [RoseTree "index.html" []]]

up' = Tree (RoseTree "projects/" 
        [RoseTree "chair/" 
            [RoseTree "index.html" []]
        ,RoseTree "index.html" []])
        [] 
        [RoseTree "about/" 
            [RoseTree "index.html" []]
        ,RoseTree "contact/" 
            [RoseTree "index.html" []]
        ,RoseTree "index.html" []
        ,RoseTree "posts/" 
            [RoseTree "applicative/" 
                [RoseTree "index.html" []]
            ,RoseTree "bifunctors/" 
                [RoseTree "index.html" []]
            ,RoseTree "functors/" 
                [RoseTree "index.html" []]
            ,RoseTree "index.html" []
            ,RoseTree "monads/" 
                [RoseTree "index.html" []]
            ,RoseTree "profunctors/" 
                [RoseTree "index.html" []]]] 
        [RoseTree "test/" [RoseTree "index.html" []]]

previosSib = Tree (RoseTree "about/" 
                    [RoseTree "index.html" []]) [] [] 
                    [RoseTree "contact/" 
                        [RoseTree "index.html" []]
                    ,RoseTree "index.html" []
                    ,RoseTree "posts/" 
                        [RoseTree "applicative/" 
                            [RoseTree "index.html" []]
                            ,RoseTree "bifunctors/" 
                                [RoseTree "index.html" []]
                            ,RoseTree "functors/" 
                                [RoseTree "index.html" []]
                            ,RoseTree "index.html" []
                            ,RoseTree "monads/" 
                                [RoseTree "index.html" []]
                            ,RoseTree "profunctors/" 
                                [RoseTree "index.html" []]]
                            ,RoseTree "projects/" 
                                [RoseTree "chair/" 
                                    [RoseTree "index.html" []]
                                ,RoseTree "index.html" []]
                            ,RoseTree "test/"
                                [RoseTree "index.html" []]
                    ]




upup' = Root [RoseTree "about/"
                [RoseTree "index.html" []]
            ,RoseTree "contact/" 
                [RoseTree "index.html" []]
            ,RoseTree "index.html" []
            ,RoseTree "posts/" 
                [RoseTree "applicative/" 
                    [RoseTree "index.html" []]
                    ,RoseTree "bifunctors/" 
                        [RoseTree "index.html" []]
                    ,RoseTree "functors/" 
                        [RoseTree "index.html" []]
                    ,RoseTree "index.html" []
                    ,RoseTree "monads/" 
                        [RoseTree "index.html" []]
                    ,RoseTree "profunctors/" 
                        [RoseTree "index.html" []]]
                    ,RoseTree "projects/" 
                        [RoseTree "chair/" 
                            [RoseTree "index.html" []]
                        ,RoseTree "index.html" []
                        ]
                    ,RoseTree "test/" 
                        [RoseTree "index.html" []]
            ]













