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

{-# LANGUAGE TupleSections #-}
module Webbi.Menu
    ( Menu(..)
    , fromTreeZipper
    )
where

import           Data.Functor                   ( (<&>) )
import           Data.Char
import           Data.String
import           System.FilePath                ( takeFileName
                                                , takeExtension
                                                , dropTrailingPathSeparator
                                                , splitPath
                                                , joinPath
                                                )
import qualified Webbi.Utils.RoseTree          as RT
import           Debug.Trace
import           Webbi.Utils.ListZipper

import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper       as TZ
import qualified Webbi.Utils.RoseTree          as R
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Webbi.Utils.Free              as F

import qualified Data.DList                    as D

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
    { menu :: Menu
    , style :: Style
    } deriving (Has Menu) via Field "menu" Env
      deriving (Has Style) via Field "style" Env


data Menu = Menu (TZ.TreeZipper FilePath)
    deriving (Show)

newtype Style = Style (H.AttributeValue)

fromTreeZipper :: TZ.TreeZipper FilePath -> H.Html
fromTreeZipper z = run (Env (Menu z) (Style "menu")) createMenu




showHeader :: H.Html -> H.Html
showHeader nav = H.header ! A.class_ "header" $ nav


createMenu :: (MonadReader env m, MonadCss m, Has Style env, Has Menu env) => m ()
createMenu = do
    (Menu tz) <- grab @Menu
    (Style style) <- grab @Style
    items <- F.foldMapM showItems $ collect tz
    cssIt $ showHeader $ H.nav ! A.class_ style $ items


collect
    :: TZ.TreeZipper FilePath -> D.DList (ListZipper (TZ.TreeZipper FilePath))
collect tz = case TZ.up tz of
    Nothing  -> D.singleton (makeRoot tz)
    Just tz' -> D.snoc (collect tz') (makeLevel tz)


makeLevel :: TZ.TreeZipper FilePath -> ListZipper (TZ.TreeZipper FilePath)
makeLevel tz = TZ.siblings tz


makeRoot :: TZ.TreeZipper FilePath -> ListZipper (TZ.TreeZipper FilePath)
makeRoot (TZ.TreeZipper x _ ls rs) = fmap TZ.fromRoseTree $ ListZipper ls x rs


showItems :: (MonadReader env m, MonadCss m, Has Style env, Has Menu env) => ListZipper (TZ.TreeZipper String) -> m H.Html
showItems (ListZipper ls x rs) = do
    (Style style) <- grab @Style
    let itemStyle = style <> "-link"
    let ls'       = fmap (itemStyle, ) ls
    let rs'       = fmap (itemStyle, ) rs
    let x'        = (itemStyle <> "-selection", x)
    let items = filter (\x -> TZ.datum (snd x) /= "index.html") $ ls' ++ (x' : rs')
    items' <- F.foldMapM showItem items
    return $ H.ul ! A.class_ (style <> "-level") $ items'

showItem :: (MonadReader env m, MonadCss m, Has Style env, Has Menu env) => (H.AttributeValue, TZ.TreeZipper String) -> m H.Html
showItem (itemStyle, tz) = do
    (Style style) <- grab @Style
    let link = (++) "/" $ mconcat $ TZ.path tz
    let text = showText tz
    return $ H.li
            ! A.class_ (style <> "-item")
            $ H.a
            ! A.class_ itemStyle
            ! A.href (fromString link)
            $ H.toHtml text


showText :: TZ.TreeZipper String -> String
showText = fmap toUpper . dropTrailingPathSeparator . TZ.datum
