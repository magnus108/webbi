module Webbi.Utils.RoseTree where

import Webbi.Utils.Trie (Trie)
import qualified Webbi.Utils.Trie                      as T

import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A



data RoseTree a = RoseTree a [RoseTree a]
    deriving (Eq, Ord, Show)


datum :: RoseTree a -> a
datum (RoseTree x _) = x


children :: RoseTree a -> [RoseTree a]
children (RoseTree _ xs) = xs


fromTrie :: String -> Trie String -> RoseTree String
fromTrie root trie = RoseTree root (children trie)
    where
        children = fmap (\(k,v) -> RoseTree k (children v)) . M.toList . T.map


showIt :: RoseTree String -> H.Html
showIt (RoseTree x []) = H.p ! A.style "background: green" $ (H.toHtml x)
showIt (RoseTree x xs) = H.div ! A.style "background: purple" $ do
                            -- show x as part of context?
                            mapM_ (H.p . H.toHtml . datum) xs
