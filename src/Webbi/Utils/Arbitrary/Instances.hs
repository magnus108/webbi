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
            gen n = do
                s' <- choose (1, 4)
                RT.RoseTree <$> arbitrary <*> (fmap (nubBy (\x y -> (RT.datum x == (RT.datum y)))) (vectorOf s' (gen (n - (1 + (n `div` 3))))))


instance (Eq a, Arbitrary a) => Arbitrary (TreeZipper a) where
    arbitrary = sized gen
        where
            gen 0 = return $ Root []
            gen n = do
                s'' <- choose (0, 4)
                l <- nubBy (\x y -> (RT.datum x == (RT.datum y))) <$> (vectorOf s'' arbitrary)
                s' <- choose (0, n+(n`div`2))
                randomWalk s' (fromForest (RT.Forest l))


randomWalk :: Eq a => Int -> TreeZipper a -> Gen (TreeZipper a)
randomWalk 0 t = return t
randomWalk s t = do
        let children' = children t
        if (length children' > 0) then do
            s' <- choose (0, (length children')-1)
            let child = children' !! s'
            let datums = fmap RT.datum (toRoseTree child)
            let t' = fromMaybe t ((\d -> (down d t)) =<< datums)
            randomWalk (s - (1 + (s `div` 3))) t'
        else 
            return t



