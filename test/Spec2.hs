{-# LANGUAGE ScopedTypeVariables #-}
module Spec2 where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Webbi.Utils.Arbitrary.Instances

import           System.FilePath                ( splitPath)
import Debug.Trace
import Data.Maybe
import Webbi.Utils.RoseTree (RoseTree(..))
import qualified Webbi.Utils.RoseTree as RT
import Webbi.Utils.TreeZipper2

import qualified Webbi.Utils.Trie as T


quickChecks2 :: TestTree
quickChecks2 = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "(up . down) is id"
        $ \(tz :: TreeZipper Int) ->
            (isJust (firstChild tz)) ==>
                    ((up =<< (firstChild tz)) === (Just tz))
  , QC.testProperty "siblings are (children . up)" $
        \(tz :: TreeZipper Int) ->
            (isJust (up tz)) ==>
                    (Just (lefts tz ++ (tz : (rights tz)))) === (children <$> up tz)
  ]
