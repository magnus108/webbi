{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Webbi.Utils.Arbitrary.Instances where
import           Webbi.Utils.TreeZipper
import           Test.Tasty.QuickCheck         as QC
import           Webbi.Utils.RoseTree           ( RoseTree
                                                , Forest(..)
                                                )
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.Trie              as T
import           Data.Maybe
import           Data.List
import           Control.Monad

import           Debug.Trace

import qualified Data.Set                      as Set


instance (Eq a, Arbitrary a) => Arbitrary (RoseTree a) where
    arbitrary = sized gen
      where
        gen 0 = RT.RoseTree <$> arbitrary <*> (return [])
        gen n = do
            numberOfChildren <- choose (0, 4)
            root             <- arbitrary
            children         <- nubBy RT.eqDatum
                <$> vectorOf numberOfChildren (gen (n - (1 + (n `div` 3))))
            return $ RT.RoseTree root children


instance (Eq a, Arbitrary a) => Arbitrary (TreeZipper a) where
    arbitrary = sized gen
      where
        gen n = do
            roseTree <- arbitrary
            steps    <- choose (0, 20)
            randomWalk steps (fromRoseTree roseTree)


randomWalk :: Eq a => Int -> TreeZipper a -> Gen (TreeZipper a)
randomWalk 0     tz = return tz
randomWalk steps tz = do
    let children' = children tz
    if (length children' > 0)
        then do
            index <- choose (0, length children' - 1)
            let child = children' !! index
            let tz'   = fromJust $ down (datum child) tz
            randomWalk (steps - 1) tz'
        else return tz
