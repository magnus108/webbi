module Webbi.Utils.Free where

import           Data.Char
import           System.FilePath                ( splitPath )

title :: FilePath -> String
title "/" = "home"
title y   = map toUpper $ title' (splitPath y)
  where
    title' (x : []            ) = x
    title' (x : ["index.md"  ]) = x --- må ikke stå index.md her
    title' (x : ["index.html"]) = x --- må ikke stå index.md her
    title' (x : xs            ) = title' xs
