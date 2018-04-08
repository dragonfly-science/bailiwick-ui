{-# LANGUAGE OverloadedStrings   #-}
module Bailiwick.View.Header
where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP.Types.URI (urlEncode)

import Reflex.Dom.Core

import Bailiwick.State (State(..), Message(..))
import Bailiwick.View.Widgets


header :: (Monad m, DomBuilder t m) => State -> m (Event t Message)
header _state = do
  elAttr "div" ("class" =: "title" <> "data-region" =: "new-zealand") $
    divClass "content" $ do
      divClass "left" $ do
        elClass "span" "block-label context-text" $ text "You're looking at"
        divClass "page-header summary-page-header" $ do
          el "div" $ text "New Zealand"
          el "div" $ return ()
      divClass "right" $ do
        divClass "title-menus" $ 
          divClass "dropdown" $
            divClass "dropdown-container" $ do
              elClass "p" "dropdown-button" $ text "Select a region"
              elClass "ul" "dropdown-menu dropdown-select" $ do
                el "li" $ text "New Zealand"
                el "li" $ text "Auckland"
                el "li" $ text "Bay of Plenty"
                el "li" $ text "Canterbury"
  return never
                

regions :: Map Text Text
regions =
  Map.fromList [ (slugify(reg), reg) |
    reg <- ["New Zealand", "Auckland", "Bay of Plenty", "Canterbury" ]]



slugify :: T.Text -> T.Text
slugify = T.intercalate "-"
    . filter (/= "")
    . map wordslug
    . T.words
  where wordslug :: T.Text -> T.Text
        wordslug = T.decodeUtf8
          . urlEncode True
          . T.encodeUtf8
          . T.replace "'" ""
          . T.replace "&" "and"
          . T.replace "(" ""
          . T.replace ")" ""
          . T.replace "," ""
          . T.replace ";" ""
          . T.replace ":" ""
          . T.replace "$m" ""
          . T.replace "$" ""
          . T.replace "=" ""
          . T.replace "%" "pc"
          . T.replace "+" "plus"
          . T.replace "/" "or"
          . T.replace "---" "-"
          . T.replace "ā" "a"
          . T.replace "ē" "e"
          . T.replace "ī" "i"
          . T.replace "ō" "o"
          . T.replace "ū" "u"
          . T.replace "!" "--"
          . T.toLower

