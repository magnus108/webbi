module SpecCheck where


import           QuickSpec
import           Test.QuickCheck.Poly           ( OrdA(..) )
import           Test.Tasty.QuickCheck         as QC

import qualified Webbi.Utils.ListZipper        as L
import qualified Webbi.Utils.TreeZipper        as T
import qualified Webbi.Utils.RoseTree          as R
import           Webbi.Utils.Arbitrary.Instances

import           Data.Maybe

isNotTop :: T.TreeZipper a -> Bool
isNotTop = isJust . T.up

isNotLeaf :: Eq a => T.TreeZipper a -> Bool
isNotLeaf = isJust . T.firstChild

hasNext :: Eq a => T.TreeZipper a -> Bool
hasNext = isJust . T.nextSibling

hasPrev :: Eq a => T.TreeZipper a -> Bool
hasPrev = isJust . T.previousSibling

siblingsAsList :: Eq a => T.TreeZipper a -> [T.TreeZipper a]
siblingsAsList t = ls ++ (x : rs) where (L.ListZipper ls x rs) = T.siblings t


treeSig :: [Sig]
treeSig =
    [ background [prelude]
    , monoTypeWithVars ["t", "t1", "t2"] (Proxy :: Proxy (T.TreeZipper Int))
    , predicate "isNotTop"  (isNotTop :: T.TreeZipper Int -> Bool)
    , predicate "isNotLeaf" (isNotLeaf :: T.TreeZipper Int -> Bool)
    , predicate "hasNext"   (hasNext :: T.TreeZipper Int -> Bool)
    , predicate "hasPrev"   (hasPrev :: T.TreeZipper Int -> Bool)
    , con "down" (T.down :: Int -> T.TreeZipper Int -> Maybe (T.TreeZipper Int))
    , con "up"       (T.up :: T.TreeZipper Int -> Maybe (T.TreeZipper Int))
    , con "rights"   (T.rights :: T.TreeZipper Int -> [T.TreeZipper Int])
    , con "lefts"    (T.lefts :: T.TreeZipper Int -> [T.TreeZipper Int])
    , con "leafs"    (T.leafs :: T.TreeZipper Int -> [T.TreeZipper Int])
    , con "children" (T.children :: T.TreeZipper Int -> [T.TreeZipper Int])
    , con "forward"  (T.forward :: T.TreeZipper Int -> Maybe (T.TreeZipper Int))
    , con "firstChild"
          (T.firstChild :: T.TreeZipper Int -> Maybe (T.TreeZipper Int))
    , con "nextSibling"
          (T.nextSibling :: T.TreeZipper Int -> Maybe (T.TreeZipper Int))
    , con
        "previousSibling"
        (T.previousSibling :: T.TreeZipper Int -> Maybe (T.TreeZipper Int))
    , con "siblings" (siblingsAsList :: T.TreeZipper Int -> [T.TreeZipper Int])
    ]


specCheck :: IO ()
specCheck = quickSpec treeSig
