{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div, span)
import           Clay
import Data.List.NonEmpty (fromList)
import Data.Monoid
import Data.Semigroup

import qualified Data.Text.Lazy.IO             as T

styleMenu :: Css
styleMenu = do
    body ?  do
        lineHeight (unitless 1)
        margin nil nil nil nil
    ul ? do
        margin nil nil nil nil
        padding nil nil nil nil
        


main :: IO ()
main = T.putStr $ renderWith compact [] $ styleMenu
