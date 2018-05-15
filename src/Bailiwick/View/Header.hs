{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
module Bailiwick.View.Header
where

import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, listToMaybe)

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Ordered (OMap, (<|))
import qualified Data.Map.Ordered as OMap
import Reflex.Dom.Core hiding (Home)

import Bailiwick.State
import Bailiwick.Types


type AreaSlug = Text


header 
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => Areas -> Dynamic t State -> m (Event t Message)
header areas state = mdo

  let urlRegion = (fmap areaId) . getRegion <$> state
      urlTa = (fmap areaId) . getSubArea <$> state
      regionsD = do
        let regions = OMap.filter (\_ a -> areaLevel a == "reg") areas
        return $
           case OMap.lookup "new-zealand" areas of
              Just nz -> ("new-zealand", nz) <| regions
              Nothing -> regions
      tasD = do
        mreg <- urlRegion
        return $ fromMaybe OMap.empty $ do
          reg <- mreg  
          thisArea <- OMap.lookup reg areas
          return $ OMap.filter (\_ a -> areaId a `elem` areaChildren thisArea) areas
      background = do
        reg <- (fromMaybe "new-zealand" <$> urlRegion)
        return $ (  "class" =: "title" <> "data-region" =: reg)

      dispRegion = maybe "New Zealand" areaName . getRegion <$> state
      dispConnect = maybe "" (const ":") . getSubArea <$> state
      dispSubArea = maybe "" areaName . getSubArea <$> state

      showSubareaD = not . ( == OMap.empty) <$> tasD

      subAreaMessage = do
        reg <- urlRegion
        if reg == Just "auckland"
            then return "Select a ward"
            else return "Select a territorial authority"

  elDynAttr "div" background $
    divClass "content" $ mdo
      divClass "left" $ do
        elClass "span" "block-label context-text" $ text "You're looking at"
        divClass "page-header summary-page-header" $ do
          el "div" $ do
            dynText dispRegion
            dynText dispConnect
          el "div" $ dynText dispSubArea
      divClass "right" $ do
        divClass "title-menus" $ do
          (region, regionOpen) <-
            dropdownMenu (constDyn "Select a region") never
                         (constDyn True) urlRegion regionsD
          (subarea, _) <-
            dropdownMenu subAreaMessage
                         (() <$ (ffilter id $ updated regionOpen))
                         showSubareaD urlTa tasD

          uniqRegion <- holdUniqDyn region
          uniqSubarea <- holdUniqDyn subarea

          return $ SetRegion <$> leftmost [ fmapMaybe id $ updated uniqSubarea
                                          , fmapMaybe id $ updated uniqRegion
                                          ]

dropdownMenu
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => Dynamic t Text              -- empty value presentation
    -> Event t ()                  -- Close event
    -> Dynamic t Bool              -- Is hidden or not
    -> Dynamic t (Maybe Text)      -- Initial value
    -> Dynamic t (OMap Text Area)    -- Options (ordered)
    -> m (Dynamic t (Maybe Text), Dynamic t Bool)
dropdownMenu emptyPresentD closeE seenD initialD valuesD = do

  let dropdownAttrD = do
        canSee <- seenD
        let visibility = if canSee then "visibility: block"
                                   else "visibility: hidden"
        return ("class" =: "dropdown" <> "style" =: visibility)

  elDynAttr "div" dropdownAttrD $ do
    divClass "dropdown-container" $ mdo
    
      currentValue :: Dynamic t (Maybe Text)
        <- holdDyn Nothing (firstKey <$> selectedValue)

      open :: Dynamic t Bool
        <- holdDyn False $
            leftmost [ (not <$> tag (current open) (domEvent Click p))
                     , False <$ selectedValue 
                     , False <$ closeE ]
   
      let label = do 
            mval <- initialD
            case mval of
              Nothing -> emptyPresentD
              Just val -> do
                values <- valuesD
                return $ maybe "" areaName $ OMap.lookup val values

          ulClass = ffor open $ \isOpen ->
              if isOpen then "dropdown-menu dropdown-select show-menu"
                        else "dropdown-menu dropdown-select"

          shuffle (i, (k, v)) = ((i,k), areaName v)
          optionsD = Map.fromList . map shuffle . zip [1..] . OMap.assocs <$> valuesD

      (p, _) <-  elClass' "p" "dropdown-button" $ dynText label
      selectedValue :: Event t (Map (Int, Text) Text)
        <- elDynClass "ul" ulClass $ do
             listViewWithKey optionsD $ \_k v -> do
               (li, _) <- el' "li" $ dynText v
               return (tag (current v) (domEvent Click li))

      return (currentValue, open)

  where
    firstKey :: Map (Int, Text) Text -> Maybe Text
    firstKey = fmap (snd . fst) . listToMaybe . Map.toList


