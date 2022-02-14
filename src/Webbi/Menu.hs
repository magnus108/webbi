{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveFoldable #-}
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


import           Data.Bifunctor
import           Data.Bifoldable
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


import           Control.Applicative


instance Traversable D.DList where
  traverse f = foldr cons_f (pure D.empty)
    where
      cons_f x = liftA2 D.cons (f x)


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
    items <- F.bifoldMapM showIndex showItems $ collect tz
    return $ showHeader $ H.nav ! A.class_ style $ items


collect
    :: TZ.TreeZipper FilePath
    -> A [TZ.TreeZipper FilePath] (D.DList (ListZipper (TZ.TreeZipper FilePath)))
collect x = if isIndex' x then (collect'' (TZ.up x)) else B (collect' x) --altid nederste level kan være unslected
  where
    collect'' Nothing = A (makeRoot' x) D.empty
    collect'' (Just tz) = A (makeLevel' x) (collect' tz)

    makeRoot' (TZ.TreeZipper x _ ls rs) = fmap TZ.fromRoseTree $ ls ++ rs

    collect' tz = case TZ.up tz of
        Nothing  -> D.singleton (makeRoot tz)
        Just tz' -> D.snoc (collect' tz') (makeLevel tz)


data A a b = A a b | B b


instance Bifoldable A where
  bifoldMap f g (A a b) = f a <> g b
  bifoldMap _ g (B b) = g b


makeLevel :: TZ.TreeZipper FilePath -> ListZipper (TZ.TreeZipper FilePath)
makeLevel tz = ListZipper (filter (not . isIndex') (TZ.lefts tz)) tz (filter (not . isIndex') (TZ.rights tz))


makeLevel' :: TZ.TreeZipper FilePath -> [TZ.TreeZipper FilePath]
makeLevel' tz = (TZ.lefts tz) ++ (TZ.rights tz)


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
    => D.DList (ListZipper (TZ.TreeZipper String))
    -> m H.Html
showItems xs = do
    (LevelStyle levelStyle) <- grab @LevelStyle
    levels                  <- mapM (mapCM showNormalItem showSelectionItem) xs
    return $ mconcat $ D.toList $ fmap (H.ul ! A.class_ levelStyle) $ fmap (mconcat . toList) levels

showIndex
    :: ( MonadReader env m
       , Has ItemStyle env
       , Has Style env
       , Has LevelStyle env
       , Has LinkSelectionStyle env
       , Has LinkStyle env
       , Has Menu env
       )
    => [TZ.TreeZipper String]
    -> m H.Html
showIndex xs = do
    (LevelStyle levelStyle) <- grab @LevelStyle
    levels                  <- mapM showNormalItem xs
    return $ H.ul ! A.class_ levelStyle $ mconcat $ levels


isIndex' :: TZ.TreeZipper String -> Bool
isIndex' x = TZ.datum x == "index.html"


showItem
    :: (MonadReader env m, Has ItemStyle env, Has Style env, Has Menu env)
    => H.AttributeValue
    -> TZ.TreeZipper String
    -> m H.Html
showItem linkStyle tz = do
    (Style     style    ) <- grab @Style
    (ItemStyle itemStyle) <- grab @ItemStyle
    let link = (++) "/" $ mconcat $ TZ.path tz
    let text = showText tz
    return
        $ H.li
        ! A.class_ itemStyle
        $ H.a
        ! A.class_ linkStyle
        ! A.href (fromString link)
        $ H.toHtml text

showNormalItem
    :: ( MonadReader env m
       , Has LinkStyle env
       , Has ItemStyle env
       , Has Style env
       , Has Menu env
       )
    => TZ.TreeZipper String
    -> m H.Html
showNormalItem tz = do
    (LinkStyle linkStyle) <- grab @LinkStyle
    showItem linkStyle tz

showSelectionItem
    :: ( MonadReader env m
       , Has LinkSelectionStyle env
       , Has ItemStyle env
       , Has Style env
       , Has Menu env
       )
    => TZ.TreeZipper String
    -> m H.Html
showSelectionItem tz = do
    (LinkSelectionStyle linkSelectionStyle) <- grab @LinkSelectionStyle
    showItem linkSelectionStyle tz


showText :: TZ.TreeZipper String -> String
showText = fmap toUpper . dropTrailingPathSeparator . TZ.datum
