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

import           Debug.Trace

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Void
import           Dhall.Src
import qualified Dhall.Map
import qualified Dhall.Core                    as DC
import qualified Dhall                         as D
import           Data.String

import           GHC.Generics
import           Clay                    hiding ( grab
                                                , id
                                                , maxWidth
                                                , fontWeight
                                                , FontWeight
                                                , color
                                                )
import           Clay.Stylesheet                ( StyleM )
import qualified Clay.Flexbox                  as FB
import qualified Clay.Size                     as S
import qualified Clay                          as C
import qualified Data.Text.Lazy.IO             as T
import           GHC.Records                    ( HasField(getField) )
import           GHC.TypeLits                   ( Symbol )
import           Data.Kind

import           Control.Monad.Reader

import qualified Control.Lens                  as Lens
import           Control.Lens                   ( (^.) )
import           GHC.OverloadedLabels           ( IsLabel(..) )
import           Data.Generics.Labels    hiding ( Field
                                                , getField
                                                , HasField
                                                )
import           Data.Generics.Product.Fields
                                         hiding ( HasField
                                                , getField
                                                )

import Webbi.Utils.Has


newtype DarkPrimaryColor = DarkPrimaryColor { unDarkPrimaryColor :: Color }
      deriving (Generic)

newtype PrimaryColor = PrimaryColor { unPrimaryColor :: Color }
      deriving (Generic)

newtype LightPrimaryColor = LightPrimaryColor { unLigthPrimaryColor :: Color }
      deriving (Generic)

newtype IconColor = IconColor { unIconColor :: Color }
      deriving (Generic)

newtype PrimaryTextColor = PrimaryTextColor { unPrimaryTextColor :: Color }
      deriving (Generic)

newtype SecondaryTextColor = SecondaryTextColor { unSecondaryTextColor :: Color }
      deriving (Generic)

newtype AccentColor = AccentColor { unAccentColor :: Color }
      deriving (Generic)

newtype SecondaryAccentColor = SecondaryAccentColor { unSecondaryAccentColor :: Color }
      deriving (Generic)

newtype DividerColor = DividerColor { unDividerColor :: Color }
      deriving (Generic)


data MenuButtonStyle = MenuButtonStyle { a :: Size LengthUnit
                               , b :: Size LengthUnit
                               , c :: Size LengthUnit
                               , d :: Size LengthUnit
                               }
      deriving (Generic)


data MenuStyle = MenuStyle { maxWidth :: Size LengthUnit, fontWeight :: C.FontWeight }
      deriving (Generic)

data TitleStyle = TitleStyle
      deriving (Generic)

data FooterStyle = FooterStyle
      deriving (Generic)

data ShadowStyle = ShadowStyle { color :: Color, x :: Size LengthUnit, y :: Size LengthUnit, blur :: Size LengthUnit }
      deriving (Generic)

toShadow :: ShadowStyle -> Css
toShadow x = boxShadow $ pure $ bsColor
    (x ^. #color)
    (shadowWithBlur (x ^. #x) (x ^. #y) (x ^. #blur))

data PrimaryShadowStyle = PrimaryShadowStyle ShadowStyle
data SecondaryShadowStyle = SecondaryShadowStyle ShadowStyle


data Env (m :: Type -> Type) = Env
    { darkPrimaryColor :: DarkPrimaryColor
    , primaryColor :: PrimaryColor
    , lightPrimaryColor :: LightPrimaryColor
    , iconColor :: IconColor
    , primaryTextColor :: PrimaryTextColor
    , secondaryTextColor :: SecondaryTextColor
    , accentColor :: AccentColor
    , secondaryAccentColor :: SecondaryAccentColor
    , dividerColor :: DividerColor
    , menuStyle :: MenuStyle
    , menuButtonStyle :: MenuButtonStyle
    , titleStyle :: TitleStyle
    , footerStyle :: FooterStyle
    , primaryShadowStyle :: PrimaryShadowStyle
    , secondaryShadowStyle :: SecondaryShadowStyle
    } deriving (Has DarkPrimaryColor) via Field "darkPrimaryColor" (Env m)
      deriving (Has PrimaryColor) via Field "primaryColor" (Env m)
      deriving (Has LightPrimaryColor) via Field "lightPrimaryColor" (Env m)
      deriving (Has IconColor) via Field "iconColor" (Env m)
      deriving (Has PrimaryTextColor) via Field "primaryTextColor" (Env m)
      deriving (Has SecondaryTextColor) via Field "secondaryTextColor" (Env m)
      deriving (Has AccentColor) via Field "accentColor" (Env m)
      deriving (Has SecondaryAccentColor) via Field "secondaryAccentColor" (Env m)
      deriving (Has DividerColor) via Field "dividerColor" (Env m)
      deriving (Has MenuStyle) via Field "menuStyle" (Env m)
      deriving (Has MenuButtonStyle) via Field "menuButtonStyle" (Env m)
      deriving (Has TitleStyle) via Field "titleStyle" (Env m)
      deriving (Has FooterStyle) via Field "footerStyle" (Env m)
      deriving (Has PrimaryShadowStyle) via Field "primaryShadowStyle" (Env m)
      deriving (Has SecondaryShadowStyle) via Field "secondaryShadowStyle" (Env m)
      deriving (Generic)


data Config = Config
    { darkPrimaryColor :: String
    , primaryColor :: String
    , lightPrimaryColor :: String
    , iconColor :: String
    , primaryTextColor :: String
    , secondaryTextColor :: String
    , accentColor :: String
    , secondaryAccentColor :: String
    , dividerColor :: String
    , menuConfig :: MenuConfig
    , titleConfig :: TitleConfig
    , footerConfig :: FooterConfig
    , primaryShadowConfig :: ShadowConfig
    , secondaryShadowConfig :: ShadowConfig
    } deriving (Generic)
      deriving (D.FromDhall)

data ButtonConfig = ButtonConfig { a :: Double
                                 , b :: Double
                                 , c :: Double
                                 , d :: Double
                                 }
      deriving (Generic)
      deriving (D.FromDhall)

data TitleConfig = TitleConfig {}
      deriving (Generic)
      deriving (D.FromDhall)

data FooterConfig = FooterConfig {}
      deriving (Generic)
      deriving (D.FromDhall)

data ShadowConfig = ShadowConfig { color :: String, x :: Double, y :: Double, blur :: Double}
      deriving (Generic)
      deriving (D.FromDhall)

data MenuConfig = MenuConfig
    { maxWidth :: Double
    , fontWeight :: FontWeight
    , buttonConfig :: ButtonConfig
    } deriving (Generic)
      deriving (D.FromDhall)

data FontWeight = Bold | Bolder
      deriving (Show)
      deriving (Generic)
      deriving (D.FromDhall)


usingReaderT :: r -> ReaderT r m a -> m a
usingReaderT = flip runReaderT

run :: env -> App env a -> StyleM a
run env = usingReaderT env . unApp


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


fontWeightTypeDecoder :: D.Decoder FontWeight
fontWeightTypeDecoder = D.auto

fontWeightType :: DC.Expr Src Void
fontWeightType = D.expected fontWeightTypeDecoder



createStyle
    :: ( MonadStyle m
       , MonadReader env m
       , WithColorPalette env m
       , WithShadowPalette env m
       , Has MenuStyle env
       , Has MenuButtonStyle env
       , Has FooterStyle env
       )
    => m ()
createStyle = do
    styleIt $ styleAll
    createHeader
    styleMenu
    styleFooter


type WithShadowPalette env m = (MonadReader env m, Has PrimaryShadowStyle env, Has SecondaryShadowStyle env)

type WithColorPalette env m
    = ( MonadReader env m
      , Has DarkPrimaryColor env
      , Has PrimaryColor env
      , Has LightPrimaryColor env
      , Has IconColor env
      , Has PrimaryTextColor env
      , Has SecondaryTextColor env
      , Has AccentColor env
      , Has SecondaryAccentColor env
      , Has DividerColor env
      )

createHeader
    :: (MonadStyle m, MonadReader env m, WithShadowPalette env m, WithColorPalette env m) => m ()
createHeader = do
    (DarkPrimaryColor darkPrimaryColor) <- grab @DarkPrimaryColor
    (PrimaryShadowStyle primaryShadow) <- grab @PrimaryShadowStyle
    styleIt $ ".header" ? do
        toShadow primaryShadow
        backgroundColor darkPrimaryColor


styleMenu
    :: ( MonadStyle m
       , MonadReader env m
       , Has MenuStyle env
       , Has MenuButtonStyle env
       , WithColorPalette env m
       , WithShadowPalette env m
       )
    => m ()
styleMenu = do
    (AccentColor accentColor      ) <- grab @AccentColor
    (IconColor   iconColor        ) <- grab @IconColor
    (MenuStyle maxWidth fontWeight) <- grab @MenuStyle
    (MenuButtonStyle a b c d) <- grab @MenuButtonStyle
    (SecondaryShadowStyle secondaryShadow) <- grab @SecondaryShadowStyle
    styleIt $ do
        ".menu" ? do
            C.maxWidth maxWidth
            marginLeft auto        --
            marginRight auto       --

        ".menu-level" ? do
            display flex             --

        ".menu-item" ? do
            FB.flex 1 1 auto         --
            display flex             --

        ".menu-link" ? do
            ":hover" & do
                backgroundColor iconColor
                toShadow secondaryShadow
            C.fontWeight fontWeight

            FB.flex 1 1 auto     --
            padding a b c d

        ".menu-link-selection" ? do
            backgroundColor accentColor
            toShadow secondaryShadow
            C.fontWeight fontWeight

            FB.flex 1 1 auto    --
            padding a b c d



styleFooter
    :: ( MonadStyle m
       , MonadReader env m
       , Has FooterStyle env
       , WithColorPalette env m
       , WithShadowPalette env m
       )
    => m ()
styleFooter = do
    (SecondaryAccentColor secondaryAccentColor) <- grab @SecondaryAccentColor
    (IconColor            iconColor           ) <- grab @IconColor
    styleIt $ do
        ".footer" ? do
            backgroundColor "#ffa500"
            boxShadow $ pure $ bsColor (rgba 0 0 0 0.3)
                                       (shadowWithBlur (px 0) (px 0) (px 8))
            minHeight (S.rem 5)

        ".subfooter-container" ? do
            marginLeft auto
            marginRight auto
            C.maxWidth (px 1140)
            padding (S.rem 1) (S.rem 0) (S.rem 1) (S.rem 0)

        ".footer-list" ? do
            display flex
            flexFlow row FB.wrap
            padding nil nil nil nil
            margin nil auto nil auto
            C.maxWidth (px 1140)

        ".footer-item" ? do
            display flex
            margin (S.rem 1) (S.rem 1) (S.rem 1) (S.rem 1)

        ".footer-link" ? do
            padding (S.rem 0.66) (S.rem 0.66) (S.rem 0.66) (S.rem 0.66)
            secondaryBoxShadow
            backgroundColor secondaryAccentColor
            ":hover" & do
                backgroundColor iconColor


main :: IO ()
main = do
    let evaluateSettings = Lens.over
            D.substitutions
            (Dhall.Map.insert "FontWeight" fontWeightType)
            D.defaultEvaluateSettings
    (x :: Config) <- D.inputFileWithSettings evaluateSettings
                                             D.auto
                                             "./css/config.dhall"

    T.putStr $ renderWith compact [] $ do
        let
            e = Env
                { darkPrimaryColor = DarkPrimaryColor
                                         (fromString (x ^. #darkPrimaryColor))
                , primaryColor = PrimaryColor (fromString (x ^. #primaryColor))
                , lightPrimaryColor = LightPrimaryColor
                                          (fromString (x ^. #lightPrimaryColor))
                , iconColor = IconColor (fromString (x ^. #iconColor))
                , primaryTextColor = PrimaryTextColor
                                         (fromString (x ^. #primaryTextColor))
                , secondaryTextColor =
                    SecondaryTextColor (fromString (x ^. #secondaryTextColor))
                , accentColor = AccentColor (fromString (x ^. #accentColor))
                , secondaryAccentColor =
                    SecondaryAccentColor
                        (fromString (x ^. #secondaryAccentColor))
                , dividerColor = DividerColor (fromString (x ^. #dividerColor))
                , menuStyle = MenuStyle
                                  (px (x ^. #menuConfig . #maxWidth))
                                  (toFontWeight (x ^. #menuConfig . #fontWeight))
                , menuButtonStyle = MenuButtonStyle (S.rem (x ^. #menuConfig . #buttonConfig . #a)) (S.rem (x ^. #menuConfig . #buttonConfig . #b)) (S.rem (x ^. #menuConfig . #buttonConfig . #c)) (S.rem (x ^. #menuConfig . #buttonConfig . #d))
                , primaryShadowStyle = PrimaryShadowStyle $ ShadowStyle (fromString (x ^. #primaryShadowConfig . #color)) (px (x ^. #primaryShadowConfig . #x)) (px (x ^. #primaryShadowConfig . #y)) (px (x ^. #primaryShadowConfig . #blur))
                , secondaryShadowStyle = SecondaryShadowStyle $ ShadowStyle (fromString (x ^. #secondaryShadowConfig . #color)) (px (x ^. #secondaryShadowConfig . #x)) (px (x ^. #secondaryShadowConfig . #y)) (px (x ^. #secondaryShadowConfig . #blur))
                , titleStyle = TitleStyle
                , footerStyle = FooterStyle
                }
        run e createStyle

toFontWeight :: FontWeight -> C.FontWeight
toFontWeight Bold   = bold
toFontWeight Bolder = bolder




margin1 :: Size a -> Css
margin1 x = margin x x x x


padding1 :: Size a -> Css
padding1 x = padding x x x x



primaryBoxShadow :: Css
primaryBoxShadow = boxShadow $ pure $ bsColor
    ("#0000004d")
    (shadowWithBlur nil (px 2) (px 5))

secondaryBoxShadow :: Css
secondaryBoxShadow =
    boxShadow $ pure $ bsColor ("#0000004d") (shadow (px 10) (px 11))

generalStyle :: Css
generalStyle = do
    ".main" ? do
        marginTop (S.rem 2)
        marginLeft auto
        marginRight auto
        C.maxWidth (px 1140)
        FB.flex 1 1 auto
        width (pct 100)

    ".headshot" ? do
        borderRadius (S.rem 1.2) (S.rem 1.2) (S.rem 1.2) (S.rem 1.2)
        boxShadow $ pure $ bsColor (rgba 0 0 0 0.3)
                                   (shadowWithBlur (px 4) (px 4) (px 20))
        transition "all" (sec 0.3) ease (sec 0)
        border dashed (S.rem 0.2) "#ffa5ff"
        ":hover" & do
            boxShadow $ pure $ bsColor
                (rgba 0 0 0 0.3)
                (shadowWithBlur (px 8) (px 8) (px 20))

    ".about" ? do
        margin (S.rem 0) (S.rem 0) (S.rem 4) (S.rem 0)


    ".front" ? do
        backgroundColor "#f5f5dc"

    ".front-container" ? do
        marginLeft auto
        marginRight auto
        C.maxWidth (px 1140)
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
        C.color "#563D7C"
        fontFamily [] [monospace]
        display flex
        flexFlow column FB.nowrap
        minHeight (vh 100)

styleAll :: Css
styleAll = do
    generalStyle
