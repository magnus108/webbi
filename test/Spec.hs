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


hasCtxWithSiblings :: [Context a] -> Bool
hasCtxWithSiblings [] = False
hasCtxWithSiblings ((Context [] x []):xss) = False
hasCtxWithSiblings ((Context ls x rs):xss) = True

hasCtx :: [Context a] -> Bool
hasCtx [] = False
hasCtx _= True

hasRoots :: TreeZipper a -> Bool
hasRoots (Root _) = False
hasRoots (Tree _ _ [] []) = False
hasRoots (Tree _ _ ls rs) = True

isLeaf :: TreeZipper a -> Bool
isLeaf (Root _) = False
isLeaf (Tree (RoseTree x []) _ _ _) = True
isLeaf (Tree _ _ ls rs) = False

isEmptyRoot :: TreeZipper a -> Bool
isEmptyRoot (Root []) = True
isEmptyRoot _ = False

isRoot :: TreeZipper a -> Bool
isRoot (Root _) = True
isRoot _ = False


summarize :: TreeZipper Int -> String
summarize (Root []              ) = "empty root"
summarize (Root _               ) = "root"
summarize (Tree item ctx lss rss) = "tree" ++ isLeaf item ++ (hasRoots (lss ++ rss)) ++ (hasCtx ctx)
    where
        isLeaf (RoseTree x []) = ", is leaf"
        isLeaf (RoseTree x xs) = ", is branch"
        hasRoots [] = ", no roots"
        hasRoots xs = ", has roots"
        hasCtx [] = ", no ctx"
        hasCtx ((Context [] x []):xss)= ", has ctx with no siblings"
        hasCtx ((Context ls x rs):xss)= ", has ctx with siblings"

quickChecks :: TestTree
quickChecks = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "(up . down) is id"
        $ \(tz :: TreeZipper Int) ->
            (isJust (firstChild tz)) ==>
                cover 40 (hasCtxWithSiblings (toContexts tz) && hasRoots tz && (not (isLeaf tz))) "tree, is branch, has roots, has ctx with siblings" $
                cover 25 (not (hasCtxWithSiblings (toContexts tz)) && hasRoots tz && (not (isLeaf tz))) "tree, is branch, has roots, has ctx with no siblings" $
                cover 5 (not (hasCtxWithSiblings (toContexts tz)) && (not (hasRoots tz)) && (not (isLeaf tz))) "tree, is branch, no roots, has ctx with no siblings" $
                cover 5 ((hasCtxWithSiblings (toContexts tz)) && (hasRoots tz) && (not (isLeaf tz))) "tree, is branch, has roots, has ctx with siblings" $
                cover 5 (isRoot tz) "root" $
                cover 5 (isRoot tz) "tree, is branch, has roots, no ctx" $
                cover 5 (isRoot tz) "tree, is branch, no roots, no ctx" $
                    ((up =<< (firstChild tz)) === (Just tz))
  , QC.testProperty "siblings are (children . up)" $
        \(tz :: TreeZipper Int) ->
            (isJust (up tz)) ==>
                cover 35 (hasCtxWithSiblings (toContexts tz) && hasRoots tz && (not (isLeaf tz))) "tree, is branch, has roots, has ctx with siblings" $
                cover 15 ((not (hasCtxWithSiblings (toContexts tz))) && hasRoots tz && (not (isLeaf tz))) "tree, is branch, has roots, has ctx with no siblings" $
                cover 15 ((hasCtxWithSiblings (toContexts tz)) && (not (hasRoots tz)) && (not (isLeaf tz))) "tree, is branch, no roots, has ctx with siblings" $
                cover 5 ((hasCtxWithSiblings (toContexts tz)) && (hasRoots tz) && (isLeaf tz)) "tree, is leaf, has roots, has ctx with siblings" $
                cover 5 ((not (hasCtxWithSiblings (toContexts tz))) && (not (hasRoots tz)) && (not (isLeaf tz))) "tree, is branch, no roots, has ctx with no siblings" $
                cover 5 ((not (hasCtx (toContexts tz))) && (hasRoots tz) && (not (isLeaf tz))) "tree, is branch, has roots, no ctx" $
                cover 1 ((not( hasCtxWithSiblings (toContexts tz)))&& (hasRoots tz) && (isLeaf tz)) "tree, is leaf, has roots, has ctx with no siblings" $
                cover 1 ((not (hasCtx (toContexts tz))) && (not (hasRoots tz)) && (not(isLeaf tz))) "tree, is branch, no roots, no ctx" $
                cover 1 ((not (hasCtxWithSiblings (toContexts tz))) && (not (hasRoots tz)) && (isLeaf tz)) "tree, is leaf, no roots, has ctx with no siblings" $
                    (Just (lefts tz ++ (tz : (rights tz)))) === (children <$> up tz)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" []
