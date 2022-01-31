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
    , showCss
    )
where

import           Debug.Trace
import           Data.String
import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper       as TZ
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import Data.Functor ((<&>))
import           Control.Monad.Reader
import Text.Blaze.Internal

import           Data.Kind
import Webbi.Utils.Has


newtype App env a = App
    { unApp :: ReaderT env MarkupM a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadReader env
                       , MonadCss
                       )

class Monad m => MonadCss m where
    cssIt :: MarkupM () -> m ()


instance (MonadCss m) => MonadCss (ReaderT env m) where
    cssIt = lift . cssIt


instance MonadCss MarkupM where
    cssIt = id


usingReaderT :: r -> ReaderT r m a -> m a
usingReaderT = flip runReaderT


run :: env -> App env a -> MarkupM a
run env = usingReaderT env . unApp

data Env = Env
    { css :: Css
    } deriving (Has Css) via Field "css" Env


data Css = Css (TZ.TreeZipper FilePath)
    deriving (Show)


fromTreeZipper :: TZ.TreeZipper FilePath -> H.Html
fromTreeZipper z = run (Env (Css z)) createCss


createCss :: ( MonadReader env m, MonadCss m, Has Css env ) => m ()
createCss = do
    css <- grab @Css
    cssIt $ showCss css

showLink :: FilePath -> H.Html
showLink path = H.link ! A.rel "stylesheet" ! A.href (fromString path)


showCss :: Css -> H.Html
showCss (Css tz) = mapM_ showLink $ fmap (("/" ++) . mconcat) paths
    where paths = catMaybes (collect "css/" tz) >>= TZ.children <&> TZ.path


collect :: FilePath -> TZ.TreeZipper FilePath -> [Maybe (TZ.TreeZipper FilePath)]
collect path tz = collectLeafs path tz : case TZ.up tz of
        Nothing  -> [collectRoot path tz]
        Just tz' -> collect path tz'


collectLeafs :: FilePath -> TZ.TreeZipper FilePath -> Maybe (TZ.TreeZipper FilePath)
collectLeafs path tz = TZ.down path tz


collectRoot :: FilePath -> TZ.TreeZipper FilePath -> Maybe (TZ.TreeZipper FilePath)
collectRoot path tz = TZ.navigateTo [path] (TZ.toForest tz)
