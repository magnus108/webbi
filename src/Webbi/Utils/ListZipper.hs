module Webbi.Utils.ListZipper where


import           Prelude                 hiding ( tail )
import           Control.Comonad

import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , (<|)
                                                , tail
                                                , cons
                                                )

data ListZipper a = ListZipper [a] a [a]
    deriving (Show, Eq, Ord)
    deriving (Functor)


instance Comonad ListZipper where
    extract (ListZipper _ a _) = a
    duplicate a = ListZipper (shift backward') a (shift forward')
        where shift move = tail $ iterate' move a

instance Foldable ListZipper where
    foldMap f (ListZipper l x r) = foldMap f (reverse l) <> f x <> foldMap f r

instance Traversable ListZipper where
    traverse f (ListZipper l x r) =
        ListZipper <$> traverse f l <*> f x <*> traverse f r


mapC :: (a -> b) -> (a -> b) -> ListZipper a -> ListZipper b
mapC f g (ListZipper ls x rs) = ListZipper (fmap f ls) (g x) (fmap f rs)


mapCM :: Monad m => (a -> m b) -> (a -> m b) -> ListZipper a -> m (ListZipper b)
mapCM f g (ListZipper ls x rs) = do
    ls' <- mapM f ls
    x'  <- g x
    rs' <- mapM f rs
    return (ListZipper ls' x' rs')


toList :: ListZipper a -> [a]
toList (ListZipper ls x rs) = ls ++ (x : rs)


iterate' :: (a -> Maybe a) -> a -> NonEmpty a
iterate' f x = case f x of
    Just x' -> x <| (iterate' f x')
    Nothing -> x :| []


backward' :: ListZipper a -> Maybe (ListZipper a)
backward' (ListZipper (l : ls) a rs) = Just (ListZipper ls l (a : rs))
backward' (ListZipper []       _ _ ) = Nothing


forward' :: ListZipper a -> Maybe (ListZipper a)
forward' (ListZipper ls a (r : rs)) = Just (ListZipper (a : ls) r rs)
forward' (ListZipper _  _ []      ) = Nothing
