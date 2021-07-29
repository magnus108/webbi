module Webbi.Utils.RoseTree where

import Debug.Trace

data RoseTree b l = Leaf l | Branch b [RoseTree  b l]
    deriving (Eq, Ord, Show)

datum :: RoseTree b l -> Either b l
datum (Branch b _) = Left b
datum (Leaf l) = Right l

children :: RoseTree b l -> [RoseTree b l]
children (Branch _ xs) = xs
children _ = []


data Context b l = Context [RoseTree b l] b [RoseTree b l]
    deriving (Show, Eq, Ord)


data TreeZipper b l = TreeZipper (RoseTree b l) [Context b l]
    deriving (Show, Eq, Ord)


toRoseTree :: TreeZipper b l -> RoseTree b l
toRoseTree (TreeZipper item _) = item


toContext :: TreeZipper b l -> [Context b l]
toContext (TreeZipper _ item) = item


mkTreeZipper :: RoseTree b l -> TreeZipper b l
mkTreeZipper x = TreeZipper x []

down :: (Show b, Show l, Eq b, Eq l) => Either b l -> TreeZipper b l -> Maybe (TreeZipper b l)
down x (TreeZipper (Branch parent items) bs) =
    let
        (ls, rs) = break (\item -> (traceShow (datum item) (datum item)) == (traceShow x x)) items
    in
        case rs of
            y:ys -> Just (TreeZipper y (Context ls parent ys:bs))
            _ -> Nothing
down _ _ = Nothing
