module Main where

import Hakyll.Main

import Lib

main :: IO ()
main = hakyll $ do
    compileCss
    compileMenu
    compileContent
    compileTemplates
