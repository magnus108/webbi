{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div, span)
import           Clay
import Data.List.NonEmpty (fromList)
import Data.Monoid
import Data.Semigroup

import qualified Data.Text.Lazy.IO             as T

styleMenu :: Css
styleMenu = do
    sconcat (fromList
        [ html
        , body
        , div
        , span
        , object
        , iframe
        , h1
        , h2
        , h3
        , h4
        , h5
        , h6
        , p
        , blockquote
        , pre
        , a
        , abbr
        , address
        , cite
        , code
        , del
        , dfn
        , img
        , ins
        , kbd
        , q
        , s
        , samp
        , small
        , strong
        , sub
        , sup
        , var
        , b
        , u
        , i
        , dl
        , dt
        , dd
        , ol
        , ul
        , li
        , fieldset
        , form
        , label
        , legend
        , table
        , caption
        , tbody
        , tfoot
        , thead
        , tr
        , th
        , td
        , article
        , aside
        , canvas
        , details
        , embed
        , figure
        , figcaption
        , footer
        , header
        , hgroup
        , menu
        , nav
        , output
        , ruby
        , section
        , summary
        , time
        , mark
        , audio
        , video
        ]) ? do
            margin nil nil nil nil
            padding nil nil nil nil
            border none nil none
            fontSize (pct 100)
            font (Required (inherit) (Just inherit) [] [])
            verticalAlign vAlignBaseline

    sconcat (fromList
                [ article
                , aside
                , details
                , figcaption
                , figure
                , footer
                , header
                , hgroup
                , menu
                , nav
                , section
                ]) ? do
                    display block

    sconcat (fromList
        [ h1
        , h2
        , h3
        , h4
        , h5
        , h6
        ]) ? do
            fontSize (pct 100)
            fontWeight normal
    --List

    body ?  do
        lineHeight (unitless 1)

    ol <> ul ? do
        listStyle none none none

    table ? do
        borderCollapse collapse
        borderSpacing nil


main :: IO ()
main = T.putStr $ renderWith compact [] $ styleMenu
