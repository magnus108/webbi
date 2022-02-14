module Webbi.Utils.Free where

import           Data.Char
import           System.FilePath                ( splitPath )
import Data.Bifoldable

import Control.Monad

title :: FilePath -> String
title "index.html" = "HOME"
title y   = map toUpper $ title' (splitPath y)
  where
    title' (x : []            ) = x --fjern
    title' (x : ["index.html"]) = x 
    title' (x : xs            ) = title' xs

link :: FilePath -> String
link "/index.html" = ""
link y = y

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do { b <- mb
                    ; when b thing }

foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step return xs mempty
  where
    step x r z = f x >>= \y -> r $! z `mappend` y


bifoldMapM :: (Monoid mo, Monad m, Bifoldable p) => (a -> m mo) -> (b -> m mo) -> p a b -> m mo
bifoldMapM f g xs = bifoldlM (\a b -> (fmap (\x -> x <> a) (f b))) (\a b -> fmap (\x -> x <> a) (g b)) mempty xs


myZip :: (a -> a -> a) -> [a] -> [a] -> [a]
myZip f _ [] = []
myZip f [] (x:xs) = x : myZip f [] xs
myZip f (y:ys) (x:xs) = f y x : myZip f ys xs
