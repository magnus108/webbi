{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE                     ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Webbi.Css
    ( Css(..)
    , fromTreeZipper
    )
where

import           Debug.Trace
import           Data.String
import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper        as TZ
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Data.Functor                   ( (<&>) )
import           Control.Monad.Reader
import           Text.Blaze.Internal

import           Data.Kind
import           Webbi.Utils.Has


import           Webbi.Utils.App


newtype CssPath = CssPath { unCssPath :: FilePath }


data Env = Env
    { css :: Css
    , cssPath :: CssPath
    } deriving (Has Css) via Field "css" Env
      deriving (Has CssPath) via Field "cssPath" Env


data Css = Css (TZ.TreeZipper FilePath)
    deriving (Show)


fromTreeZipper :: TZ.TreeZipper FilePath -> H.Html
fromTreeZipper z = run (Env (Css z) (CssPath "css/")) createCss


createCss :: (MonadReader env m, Has Css env, Has CssPath env) => m H.Html
createCss = do
    (Css     tz  ) <- grab @Css
    (CssPath path) <- grab @CssPath
    let paths =
            catMaybes (collect path tz)
                >>= TZ.children
                <&> (("/" ++) . mconcat . TZ.path)
    return $ mapM_ showLink paths


showLink :: FilePath -> H.Html
showLink path = H.link ! A.rel "stylesheet" ! A.href (fromString path)


collect
    :: FilePath -> TZ.TreeZipper FilePath -> [Maybe (TZ.TreeZipper FilePath)]
collect path tz = collectLeafs path tz : case TZ.up tz of
    Nothing  -> [collectRoot path tz]
    Just tz' -> collect path tz'


collectLeafs
    :: FilePath -> TZ.TreeZipper FilePath -> Maybe (TZ.TreeZipper FilePath)
collectLeafs path tz = TZ.down path tz


collectRoot
    :: FilePath -> TZ.TreeZipper FilePath -> Maybe (TZ.TreeZipper FilePath)
collectRoot path tz = TZ.navigateTo [path] (TZ.toForest tz)
