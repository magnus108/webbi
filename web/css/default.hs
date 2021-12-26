{-# LANGUAGE OverloadedStrings #-}

import           Clay
import qualified Clay.Flexbox as FB
import qualified Clay.Size as S
import qualified Data.Text.Lazy.IO             as T

primaryBoxShadow :: Css
primaryBoxShadow = boxShadowWithSpread nil (px 2) (px 5) nil (rgba 0 0 0 0.3)

secondaryBoxShadow :: Css
secondaryBoxShadow = boxShadow $ pure $ bsColor (rgba 0 0 0 0.3) (shadow (px 10) (px 11))

darkPrimaryColor :: Color
darkPrimaryColor = "#0097A7"


primaryColor :: Color
primaryColor = "#00BCD4"


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

    ".title" ? do
        fontSize (S.rem 2)
        padding (S.rem 0) (S.rem 0) (S.rem 2) (S.rem 0)

    ".header" ? do
        primaryBoxShadow
        backgroundColor darkPrimaryColor

    ".front" ? do
        backgroundColor "#f5f5dc"

    ".front-container" ? do
        marginLeft auto
        marginRight auto
        maxWidth (px 1140)
        display flex
        padding (S.rem 2) (S.rem 1) (S.rem 2) (S.rem 1)
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

    ".footer-list" ? do
        marginTop (S.rem 1)
        marginLeft auto
        marginRight auto
        maxWidth (px 1140)
        minHeight (S.rem 4)

    ".footer-link" ? do
        padding (S.rem 0) (S.rem 0) (S.rem 1) (S.rem 0)

    ".section" ? do
        padding (S.rem 0) (S.rem 0) (S.rem 1) (S.rem 0)
        fontSize (S.rem 1)

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


main :: IO ()
main = T.putStr $ renderWith compact [] $ styleAll
