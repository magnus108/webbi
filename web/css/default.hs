{-# LANGUAGE OverloadedStrings #-}

import           Clay
import qualified Clay.Flexbox as FB
import qualified Data.Text.Lazy.IO             as T

primaryBoxShadow :: Css
primaryBoxShadow = boxShadowWithSpread nil (px 2) (px 5) nil (rgba 0 0 0 0.3)


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
        display flex
        flexDirection column
        primaryBoxShadow
    ".menu-level" ? do
        display flex
        primaryBoxShadow
    ".menu-item" ? do
        FB.flex 1 1 auto
        display flex
        primaryBoxShadow
        listStyleType none
    ".menu-link" ? do
        backgroundColor darkPrimaryColor
        ":hover" & backgroundColor "#f9f9f9"
        linkStyle
    ".menu-link-selection" ? do
        backgroundColor accentColor
        linkStyle

linkStyle :: Css
linkStyle = do
    FB.flex 1 1 auto
    primaryBoxShadow
    ".focus" & fontWeight bold


styleAll :: Css
styleAll = do
    styleMenu
    body ? do
        fontFamily [] [monospace]
        color "#563D7C"
        margin1 nil
        height (pct 100)
        display flex
        flexDirection column
        star # selection ?
            background accentColor


main :: IO ()
main = T.putStr $ renderWith compact [] $ styleAll
