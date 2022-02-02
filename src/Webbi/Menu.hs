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

import           Data.Function                  ( (&) )
import           Control.Comonad
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

import qualified Webbi.Utils.TreeZipper        as TZ
import qualified Webbi.Utils.RoseTree          as R
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Webbi.Utils.Free              as F
import           Webbi.Utils.App

import qualified Data.DList                    as D

import           Control.Monad.Reader
import           Text.Blaze.Internal

import           Data.Kind
import           Webbi.Utils.Has


data Env = Env
    { menu :: Menu
    , style :: Style
    , levelStyle :: LevelStyle
    , itemStyle :: ItemStyle
    , linkStyle :: LinkStyle
    , linkSelectionStyle :: LinkSelectionStyle
    } deriving (Has Menu) via Field "menu" Env
      deriving (Has LevelStyle) via Field "levelStyle" Env
      deriving (Has Style) via Field "style" Env
      deriving (Has ItemStyle) via Field "itemStyle" Env
      deriving (Has LinkStyle) via Field "linkStyle" Env
      deriving (Has LinkSelectionStyle) via Field "linkSelectionStyle" Env


data Menu = Menu (TZ.TreeZipper FilePath)
    deriving (Show)


newtype Style = Style (H.AttributeValue)
newtype LevelStyle = LevelStyle (H.AttributeValue)
newtype ItemStyle = ItemStyle (H.AttributeValue)
newtype LinkStyle = LinkStyle (H.AttributeValue)
newtype LinkSelectionStyle = LinkSelectionStyle (H.AttributeValue)


fromTreeZipper :: TZ.TreeZipper FilePath -> H.Html
fromTreeZipper z = run
    (Env (Menu z)
         (Style "menu")
         (LevelStyle "menu-level")
         (ItemStyle "menu-item")
         (LinkStyle "menu-link")
         (LinkSelectionStyle "menu-link-selection")
    )
    createMenu


showHeader :: H.Html -> H.Html
showHeader nav = H.header ! A.class_ "header" $ nav


createMenu
    :: ( MonadReader env m
       , Has ItemStyle env
       , Has LevelStyle env
       , Has LinkStyle env
       , Has LinkSelectionStyle env
       , Has Style env
       , Has Menu env
       )
    => m H.Html
createMenu = do
    (Menu  tz   ) <- grab @Menu
    (Style style) <- grab @Style
    items         <- F.foldMapM showItems $ collect tz
    return $ showHeader $ H.nav ! A.class_ style $ items


collect
    :: TZ.TreeZipper FilePath -> D.DList (ListZipper (TZ.TreeZipper FilePath))
collect tz = case TZ.up tz of
    Nothing  -> D.singleton (makeRoot tz)
    Just tz' -> D.snoc (collect tz') (makeLevel tz)


makeLevel :: TZ.TreeZipper FilePath -> ListZipper (TZ.TreeZipper FilePath)
makeLevel tz = TZ.siblings tz


makeRoot :: TZ.TreeZipper FilePath -> ListZipper (TZ.TreeZipper FilePath)
makeRoot (TZ.TreeZipper x _ ls rs) = fmap TZ.fromRoseTree $ ListZipper ls x rs


showItems
    :: ( MonadReader env m
       , Has ItemStyle env
       , Has Style env
       , Has LevelStyle env
       , Has LinkSelectionStyle env
       , Has LinkStyle env
       , Has Menu env
       )
    => ListZipper (TZ.TreeZipper String)
    -> m H.Html
showItems zipper = do
    (Style              style             ) <- grab @Style
    (LinkStyle          linkStyle         ) <- grab @LinkStyle
    (LinkSelectionStyle linkSelectionStyle) <- grab @LinkSelectionStyle
    (LevelStyle         levelStyle        ) <- grab @LevelStyle
    let items = filter (\x -> TZ.datum (snd x) /= "index.html") $ toList $ mapC
            (linkStyle         , )
            (linkSelectionStyle, )
            zipper
    items' <- F.foldMapM showItem items
    return $ H.ul ! A.class_ levelStyle $ items'


showItem
    :: (MonadReader env m, Has ItemStyle env, Has Style env, Has Menu env)
    => (H.AttributeValue, TZ.TreeZipper String)
    -> m H.Html
showItem (itemStyle', tz) = do
    (Style     style    ) <- grab @Style
    (ItemStyle itemStyle) <- grab @ItemStyle
    let link = (++) "/" $ mconcat $ TZ.path tz
    let text = showText tz
    return
        $ H.li
        ! A.class_ itemStyle
        $ H.a
        ! A.class_ itemStyle'
        ! A.href (fromString link)
        $ H.toHtml text


showText :: TZ.TreeZipper String -> String
showText = fmap toUpper . dropTrailingPathSeparator . TZ.datum
