{-# LANGUAGE ScopedTypeVariables #-}
module Webbi.Utils.Arbitrary.Instances where
import           Webbi.Utils.TreeZipper
import           Test.Tasty.QuickCheck         as QC
import           Webbi.Utils.RoseTree           ( RoseTree
                                                , Forest(..)
                                                )
import qualified Webbi.Utils.RoseTree          as RT


instance Arbitrary a => Arbitrary (RoseTree a) where
    arbitrary = sized genRose


genChildren :: Arbitrary a => Int ->  Gen [RoseTree a]
genChildren 0 = return []
genChildren n = vectorOf (n `div` 4) (genRose (n `div` 2))


genRose :: Arbitrary a => Int -> Gen (RoseTree a)
genRose 0 = RT.RoseTree <$> arbitrary <*> (return [])
genRose n =
    RT.RoseTree
        <$> arbitrary
        <*> (genChildren (n `div` 2))


instance Arbitrary a => Arbitrary (TreeZipper a) where
    arbitrary = sized gen
      where
        roots :: Int -> Gen [RoseTree a]
        roots 0 = return []
        roots n = vectorOf (n `div` 4) (genRose (n `div` 2))

        ctx :: Int -> Gen [Context a]
        ctx 0 = return []
        ctx n = vectorOf (n `div` 4) (genCtx (n `div` 2))

        gen :: Int -> Gen (TreeZipper a)
        gen 0 = Root <$> arbitrary
        gen n =
            Tree
                <$> arbitrary
                <*> (ctx (n `div` 2))
                <*> (roots (n `div` 2))
                <*> (roots (n `div` 2))


instance Arbitrary a => Arbitrary (Context a) where
    arbitrary = sized genCtx


siblingsCtx :: Arbitrary a => Int -> Gen [RoseTree a]
siblingsCtx 0 = return []
siblingsCtx n = vectorOf (n`div` 4) (genRose (n `div` 2))


genCtx :: Arbitrary a => Int -> Gen (Context a)
genCtx 0 = Context <$> (return []) <*> arbitrary <*> (return [])
genCtx n =
    Context
        <$> (siblingsCtx n)
        <*> arbitrary
        <*> (siblingsCtx n)
