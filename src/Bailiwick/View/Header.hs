{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
module Bailiwick.View.Header
where

import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP.Types.URI (urlEncode)
import Reflex.Dom.Core

import Bailiwick.State (State(..), Message(..))


header 
    :: ( Monad m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => State -> m (Event t Message)
header state = do
  let initialRegion = case state of 
         Summary reg -> Just reg
         _           -> Nothing
      start = fromMaybe "New Zealand" $ do
                  ini <- initialRegion
                  Map.lookup ini regions

  elAttr "div" (  "class" =: "title" 
               <> "data-region" =: fromMaybe "new-zealand" initialRegion) $
    divClass "content" $ mdo
      divClass "left" $ do
        elClass "span" "block-label context-text" $ text "You're looking at"
        divClass "page-header summary-page-header" $ do
          el "div" $ text start
          el "div" $ return ()
      region
        <- divClass "right" $ do
             divClass "title-menus" $ do
               dropdownMenu start regions
      return $ (SetRegion . slugify) <$> updated region
                

regions :: Map Text Text
regions =
  Map.fromList [ (slugify(reg), reg) |
    reg <- ["New Zealand", "Auckland", "Bay of Plenty", "Canterbury" ]]

firstKey :: Map Text Text -> Text
firstKey map =
    if Map.size map == 0 
        then ""
        else snd $ head $ Map.toList map

dropdownMenu
    :: ( Monad m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => Text -> Map Text Text -> m (Dynamic t Text)
dropdownMenu start regions = do
  divClass "dropdown" $ do
    divClass "dropdown-container" $ mdo
    
      currentValue :: Dynamic t Text
        <- holdDyn start (firstKey <$> selectedRegion)

      open :: Dynamic t Bool
        <- holdDyn False $
            leftmost [ (not <$> tag (current open) (domEvent Click p))
                     , False <$ selectedRegion ]
   
      let label = ffor currentValue $ \val ->
              if val == "New Zealand"
                        then  "Select a region"
                        else val
      (p, _) <-  elClass' "p" "dropdown-button" $ dynText label

      let ulClass = ffor open $ \isOpen ->
              if isOpen then "dropdown-menu dropdown-select show-menu"
                        else "dropdown-menu dropdown-select"
      selectedRegion :: Event t (Map Text Text)
        <- elDynClass "ul" ulClass $ do
             listViewWithKey (constDyn regions) $ \k v -> do
               (li, _) <- el' "li" $ dynText v
               return (tag (current v) (domEvent Click li))

      return currentValue


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

