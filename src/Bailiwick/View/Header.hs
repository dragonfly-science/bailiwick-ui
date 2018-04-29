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
import Data.List (find)

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Servant.Reflex
import Reflex.Dom.Core

import Bailiwick.State (State(..), Message(..))
import Bailiwick.Types as BT
import Bailiwick.Store (getAreas)


type AreaSlug = Text


header 
    :: ( Monad m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       , SupportsServantReflex t m
       )
    => Dynamic t State -> m (Event t Message)
header state = mdo

  ready <- getPostBuild
  areasE <- getAreas ready
  areasD <- holdDyn [] $ fmapMaybe reqSuccess areasE

  let urlArea = getArea <$> state

      urlRegion = do
        area <- urlArea
        areas <- areasD
        return $ do
          thisArea <- findArea area areas
          if areaLevel thisArea  == "reg"
            then return (areaId thisArea)
            else do 
              let isRegion p = fromMaybe False $ do
                      pa <- findArea p areas
                      return $ areaLevel pa == "reg"
              find isRegion $ areaParents thisArea

      urlTa = zipDynWith selectTa urlArea areasD
      regionsD  = mkRegions <$> areasD
      tasD = zipDynWith mkTas urlRegion areasD
      
      background = do
        reg <- (fromMaybe "new-zealand" <$> urlRegion)
        return $ (  "class" =: "title" <> "data-region" =: reg)

      dispRegion = do 
        mthis <- urlRegion
        areas <- areasD
        mta <- urlTa
        return $ fromMaybe "New Zealand" $ do
                        this <- mthis
                        thisReg <- findArea this areas
                        if mta == Nothing
                            then return $ areaName thisReg
                            else return $ areaName thisReg <> ":"

      dispSubArea = do
        mta <- urlTa
        areas <- areasD
        return $ fromMaybe "" $ do
                    ta <- mta
                    thisTa <- findArea ta areas
                    return (areaName thisTa)

      showSubareaD = not . (==[]) <$> tasD

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
          el "div" $ dynText dispRegion
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

  where

    mkRegions :: [Area] -> [(Text,Text)]
    mkRegions as = ("new-zealand", "New Zealand") :
                   [ (areaId a, areaName a) 
                   | a <- as
                   , areaLevel a == "reg" ]  
    getArea :: State -> Text
    getArea (Summary area) = area
    getArea _ = "new-zealand"

    findArea :: Text -> [Area] -> Maybe Area
    findArea area areas = find ((area ==) . areaId) areas
      

    selectTa :: Text -> [Area] -> Maybe Text
    selectTa area areas = do
        thisArea <- findArea area areas
        if areaLevel thisArea `elem` ["ta", "ward"]
            then Just area
            else Nothing
   
    mkTas :: Maybe Text -> [Area] -> [(Text, Text)]
    mkTas maybeReg areas = fromMaybe [] $ do
        reg <- maybeReg
        thisArea <- findArea reg areas
        return $ [ (areaId a, areaName a)
                 | a <- areas
                 , areaId a `elem` areaChildren thisArea ]
        
        
                

dropdownMenu
    :: ( Monad m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => Dynamic t Text              -- empty value presentation
    -> Event t ()                  -- Close event
    -> Dynamic t Bool              -- Is hidden or not
    -> Dynamic t (Maybe Text)      -- Initial value
    -> Dynamic t [(Text, Text)]    -- Options (ordered)
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
                return $ fromMaybe "" $ lookup val values

          ulClass = ffor open $ \isOpen ->
              if isOpen then "dropdown-menu dropdown-select show-menu"
                        else "dropdown-menu dropdown-select"

          shuffle (i, (k, v)) = ((i,k), v)
          optionsD = Map.fromList . map shuffle . zip [1..] <$> valuesD

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


