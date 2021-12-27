{-# LANGUAGE ScopedTypeVariables #-}
module Webbi.Utils.Arbitrary.Instances where
import           Webbi.Utils.TreeZipper
import           Test.Tasty.QuickCheck         as QC
import           Webbi.Utils.RoseTree           ( RoseTree
                                                , Forest(..)
                                                )
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.Trie              as T
import Data.Maybe
import Data.List
import Control.Monad

import Debug.Trace

instance (Eq a, Arbitrary a) => Arbitrary (RoseTree a) where
    arbitrary = sized gen
        where 
            gen 0 = RT.RoseTree <$> arbitrary <*> (return [])
            gen n = RT.RoseTree <$> arbitrary <*> (fmap nub (vectorOf (n `div` 2) (gen (n `div` 2))))


instance (Eq a, Arbitrary a) => Arbitrary (TreeZipper a) where
    arbitrary = sized gen
        where
            gen 0 = fromRoseTree <$> arbitrary
            gen n = randomWalk 4 =<< (fromRoseTree <$> arbitrary)

randomWalk :: Eq a => Int -> TreeZipper a -> Gen (TreeZipper a)
randomWalk 0 t = return t
randomWalk s t = do
        let children' = children t
        s' <- choose (0, (length children')-1)
        let child = children' !! s'
        let t' = fromMaybe t Nothing --(down (head (datum child)) t)
        randomWalk (s - 1) t'
