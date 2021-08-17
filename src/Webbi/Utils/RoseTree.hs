module Webbi.Utils.RoseTree where

import Webbi.Utils.Trie (Trie)
import qualified Webbi.Utils.Trie                      as T

import qualified Data.Map                      as M


data RoseTree b l = Leaf l | Branch b [RoseTree  b l]
    deriving (Eq, Ord, Show)

datum :: RoseTree b l -> Either b l
datum (Branch b _) = Left b
datum (Leaf l) = Right l

children :: RoseTree b l -> [RoseTree b l]
children (Branch _ xs) = xs
children _ = []


fromTrie :: Trie -> RoseTree String String
fromTrie (T.Trie b m) = Branch "/" (fmap (\(k, (T.Trie b v)) -> if null v then Leaf k else Branch k (mkRoseTree' (M.toList v))) xs)
    where
        xs = M.toList m

mkRoseTree' :: [(String, Trie)] -> [RoseTree String String]
mkRoseTree' (xs) = (fmap (\((k, T.Trie b v)) -> if null v then Leaf k else Branch k (mkRoseTree' (M.toList v))) xs)
