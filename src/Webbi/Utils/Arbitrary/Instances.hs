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
import Control.Monad


instance (Ord a, Arbitrary a) => Arbitrary (TreeZipper a) where
    arbitrary = frequency [(1, tree)
                          ,(2, (\t -> fromMaybe t (firstChild t)) <$> tree)
                          ,(3, (\t -> fromMaybe t (firstChild =<< (firstChild t))) <$> tree)
                          ]
        where
            tree = (fromForest . RT.fromTrie . T.fromList T.insert) <$> arbitrary
