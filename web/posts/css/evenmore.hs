{-# LANGUAGE OverloadedStrings #-}
import           Clay

import qualified Data.Text.Lazy.IO             as T

styleMenu :: Css
styleMenu = return ()

main :: IO ()
main = T.putStr $ renderWith compact [] $ styleMenu
