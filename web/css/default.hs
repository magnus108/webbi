{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Dhall as D
import Data.String

import GHC.Generics
import           Clay hiding (grab, id)
import Clay.Stylesheet (StyleM)
import qualified Clay.Flexbox as FB
import qualified Clay.Size as S
import qualified Data.Text.Lazy.IO             as T
import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)
import Data.Kind

import Control.Monad.Reader

primaryBoxShadow :: Css
primaryBoxShadow = boxShadowWithSpread nil (px 2) (px 5) nil (rgba 0 0 0 0.3)

secondaryBoxShadow :: Css
secondaryBoxShadow = boxShadow $ pure $ bsColor (rgba 0 0 0 0.3) (shadow (px 10) (px 11))


primaryColor_ :: Color
primaryColor_ = "#00BCD4"


lightPrimaryColor :: Color
lightPrimaryColor = "#B2EBF2"


iconColor :: Color
iconColor = "#FFFFFF"


primaryTextColor :: Color
primaryTextColor = "#212121"


secondaryTextColor :: Color
secondaryTextColor = "#757575"


accentColor :: Color
accentColor = "#FFEB3B"


dividerColor :: Color
dividerColor = "#BDBDBD"


margin1 :: Size a -> Css
margin1 x = margin x x x x


padding1 :: Size a -> Css
padding1 x = padding x x x x


styleMenu :: Css
styleMenu = do

    ".menu" ? do
        marginLeft auto
        marginRight auto
        maxWidth (px 1140)

    ".menu-level" ? do
        display flex
        padding nil nil nil nil
        margin nil auto nil auto

    ".menu-item" ? do
        FB.flex 1 1 auto
        display flex

    ".menu-link" ? do
        ":hover" & do
            backgroundColor "#f9f9f9"
            secondaryBoxShadow
        linkStyle

    ".menu-link-selection" ? do
        backgroundColor accentColor
        secondaryBoxShadow
        linkStyle


linkStyle :: Css
linkStyle = do
    FB.flex 1 1 auto
    padding1 (S.rem 1)
    ".focus" & fontWeight bold


generalStyle :: Css
generalStyle = do
    ".main" ? do
        marginTop (S.rem 2)
        marginLeft auto
        marginRight auto
        maxWidth (px 1140)
        FB.flex 1 1 auto
        width (pct 100)

    ".headshot" ? do
        borderRadius (S.rem 1.2) (S.rem 1.2) (S.rem 1.2) (S.rem 1.2)
        boxShadow $ pure $ bsColor (rgba 0 0 0 0.3) (shadowWithBlur (px 4) (px 4) (px 20))
        transition "all" (sec 0.3) ease (sec 0)
        border dashed (S.rem 0.2) "#ffa5ff"
        ":hover" & do
            boxShadow $ pure $ bsColor (rgba 0 0 0 0.3) (shadowWithBlur (px 8) (px 8) (px 20))

    ".about" ? do
        margin (S.rem 0) (S.rem 0) (S.rem 4) (S.rem 0)


    ".front" ? do
        backgroundColor "#f5f5dc"

    ".front-container" ? do
        marginLeft auto
        marginRight auto
        maxWidth (px 1140)
        display flex
        padding (S.rem 2) (S.rem 0) (S.rem 2) (S.rem 0)
        alignItems baseline
        flexFlow row FB.wrap

    ".front-description" ? do
        FB.flex 1 0 auto
        padding (S.rem 1) (S.rem 0.66) (S.rem 0.66) (S.rem 3)

    ".front-link" ? do
        fontSize (S.rem 3)
        FB.flex 0 0 auto
        padding (S.rem 0.66) (S.rem 0.66) (S.rem 0.66) (S.rem 0.66)
        secondaryBoxShadow
        backgroundColor "#ffffb5"
        ":hover" & do
            backgroundColor "#f9f9f9"

    ".footer" ? do
        backgroundColor "#ffa500"
        boxShadow $ pure $ bsColor (rgba 0 0 0 0.3) (shadowWithBlur (px 0) (px 0) (px 8))
        minHeight (S.rem 5)

    ".subfooter-container" ? do
        marginLeft auto
        marginRight auto
        maxWidth (px 1140)
        padding (S.rem 1) (S.rem 0) (S.rem 1) (S.rem 0)

    ".footer-list" ? do
        display flex
        flexFlow row FB.wrap
        padding nil nil nil nil
        margin nil auto nil auto
        maxWidth (px 1140)

    ".footer-item" ? do
        display flex
        margin (S.rem 1) (S.rem 1) (S.rem 1) (S.rem 1)

    ".footer-link" ? do
        padding (S.rem 0.66) (S.rem 0.66) (S.rem 0.66) (S.rem 0.66)
        secondaryBoxShadow
        backgroundColor "#ffffb5"
        ":hover" & do
            backgroundColor "#f9f9f9"

    ".title-main" ? do
        FB.flex 1 1 auto

    ".title-container" ? do
        display flex
        alignItems center

    ".title-pdflink" ? do
        padding (S.rem 1) (S.rem 3) (S.rem 1) (S.rem 3)
        secondaryBoxShadow
        alignItems center
        backgroundColor "#ffffb5"
        ":hover" & do
            backgroundColor "#f9f9f9"

    ".title-pdf" ? do
        display flex

    body ? do
        color "#563D7C"
        fontFamily [] [monospace]
        display flex
        flexFlow column FB.nowrap
        minHeight (vh 100)

styleAll :: Css
styleAll = do
    generalStyle
    styleMenu



newtype DarkPrimaryColor = DarkPrimaryColor { unDarkPrimaryColor :: String }
    deriving (Generic)
    deriving newtype (D.FromDhall)

data Env (m :: Type -> Type) = Env
    { darkPrimaryColor :: DarkPrimaryColor
    } deriving (Has DarkPrimaryColor) via Field "darkPrimaryColor" (Env m)




usingReaderT :: r -> ReaderT r m a -> m a
usingReaderT = flip runReaderT

run :: env -> App env a -> StyleM a
run env = usingReaderT env . unApp

class Has field env where
    obtain :: env -> field

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

newtype Field (s :: Symbol) env = Field
    { unField :: env
    }

instance forall s f env . (HasField s env f) => Has f (Field s env) where
    obtain :: Field s env -> f
    obtain = getField @s . unField
    {-# INLINE obtain #-}


newtype App env a = App
    { unApp :: ReaderT env StyleM a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadReader env
                       , MonadStyle
                       )

class Monad m => MonadStyle m where
    styleIt :: StyleM () -> m ()


instance (MonadStyle m) => MonadStyle (ReaderT env m) where
    styleIt = lift . styleIt


instance MonadStyle StyleM where
    styleIt = id


createStyle :: (MonadStyle m, MonadReader env m, Has DarkPrimaryColor env) => m ()
createStyle = do
    styleIt $ styleAll
    createHeader


createHeader :: (MonadStyle m, MonadReader env m, Has DarkPrimaryColor env) => m ()
createHeader = do
    (DarkPrimaryColor darkPrimaryColor) <- grab @DarkPrimaryColor
    styleIt $ ".header" ? do
        primaryBoxShadow
        backgroundColor (fromString darkPrimaryColor)

main :: IO ()
main = do
    --loadConfig
    --mkEnv
    --run
    x <- (D.input D.auto "./css/config.dhall")
    T.putStr $ renderWith compact [] $ do
        let e = Env { darkPrimaryColor = DarkPrimaryColor x }
        run e createStyle
