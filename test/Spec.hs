{-# LANGUAGE ScopedTypeVariables #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Webbi.Utils.Arbitrary.Instances

import           System.FilePath                ( splitPath)
import Debug.Trace
import Data.Maybe
import Webbi.Utils.RoseTree (RoseTree(..))
import qualified Webbi.Utils.RoseTree as RT
import Webbi.Utils.TreeZipper

import qualified Webbi.Utils.Trie as T

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests, quickChecks]


quickChecks :: TestTree
quickChecks = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "up . firstChild is self" $
        \(tz :: TreeZipper Int) -> (isJust (firstChild tz)) ==> ((up =<< (firstChild tz)) === (Just tz))
  , QC.testProperty "lefts/rights" $
        \(tz :: TreeZipper Int) -> within 1000000 $ isJust (up tz) ==> (Just (lefts tz ++ (tz : (rights tz)))) === (children <$> up tz)
  ]


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
root3 = Root [roseLeaf]


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
    ]


