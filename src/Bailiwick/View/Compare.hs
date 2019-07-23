{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick.View.Compare
  ( compareMenu
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Fix (MonadFix)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Bailiwick.Route
import Bailiwick.Types

compareMenu
  :: ( MonadFix m
     , MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t (Maybe AreaId)
  -> Dynamic t (Maybe Areas)
  -> m (Event t Message)
compareMenu compareAreaD areasD = mdo

  let clickE = fmap (domEvent Click . fst)
      ca = do
        mCompareArea <- compareAreaD
        mAreas <- areasD
        return $ fromMaybe "" $ do
          caid <- mCompareArea
          Areas areas <- mAreas
          Area{..} <- OMap.lookup caid areas
          return areaName


  (clearE, popupE) <-
    divClass "compare-menu" $ do
      divClass "compare-nav" $ do
        clearE' <- do
          let showhide = maybe " hide" (const "") <$> compareAreaD
          elDynClass "div" (("compare-label"<>) <$> showhide) $ do
            el "div" $ do
              text "Comparing to"
              elClass "strong" "compared-area" $ dynText ca

            clickE $
              elClass' "button" "clear-compare-area" $
                el "i" $ return ()
        popupE' <- clickE $
          elClass' "button" "menu-button" $ do
            text "set compare area"
        return (clearE', popupE')

  showPopupD <- holdDyn " hide" $ leftmost [ " hide" <$ setCompareAreaE
                                           , " hide" <$ closeE
                                           , "" <$ popupE  ]

  (closeE, setCompareAreaE) <- comparePopup showPopupD compareAreaD areasD

  return $ leftmost [ setCompareAreaE, UnsetCompareArea <$ clearE ]


comparePopup
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Text
  -> Dynamic t (Maybe AreaId)
  -> Dynamic t (Maybe Areas)
  -> m (Event t (), Event t Message)
comparePopup showhideD compareAreaD areasD = mdo

  let regionsD = do
        mareas <- areasD
        let areas = maybe OMap.empty unAreas mareas
            regions = OMap.filter (\a -> areaLevel a == "reg") areas
            mnz = OMap.lookup "new-zealand" areas
        return $ Areas $ case mnz of
                    Just nz -> OMap.singleton "new-zealand" nz <> regions
                    Nothing -> regions

      subareasD = do
        maid <- currentRegionD
        mareas <- areasD
        return $
          fromMaybe (Areas OMap.empty) $ do
            Areas areas <- mareas
            aid <- maid
            thisArea <- OMap.lookup aid areas
            return $ Areas $
              OMap.filter (\a -> areaId a `elem` areaChildren thisArea) areas


  selectedRegionD <- holdDyn Nothing selectedRegionE
  selectedAreaD <- holdDyn Nothing selectedAreaE
  let currentRegionD = zipDynWith (<|>) selectedRegionD compareAreaD

  (closeE, selectedRegionE, selectedAreaE, clearE, setAreaE) <-
    divClass "compare-menu" $ do
      divClass "compare-nav" $ do
        elDynClass "div" (("compare-panel"<>) <$> showhideD) $
          divClass "panel" $ do
            closeE' <-
               el "header" $ do
                 el "h2" $
                   text "Set the area you want to compare with."
                 selectButton "close" $
                   elClass "i" "close-icon-rear-white" $ return ()
            (selectedRegionE', selectedAreaE', clearE', setAreaE') <-
              divClass "body" $ do
                divClass "row" $
                  divClass "col last" $
                    -- TODO: select in ember version was using Chosen.js - we'll
                    -- have to just use a select for now. Need to list all
                    -- available areas. Clicking an option disables the
                    -- "area-selction" row element by adding a "disabled" class.
                    elClass "select" "" $
                      el "option" $ text "Choose an area"
                -- TODO: need a dynamic to add a "disabled" class.
                (selectedRegionE'', selectedAreaE'') <-
                  divClass "row" $ do
                    (,) <$> selector "area-selection"
                                     "Select region"
                                     currentRegionD
                                     regionsD
                        <*> selector "ta-selection"
                                     "Select Territorial Authority"
                                     currentRegionD
                                     subareasD
                (clearE'', setAreaE'') <-
                  divClass "row" $ do
                    (,) <$> (selectButton "clear" $ text "clear")
                            -- TODO: this button needs to have a "disabled" class added
                            -- if no area is selected
                        <*> (selectButton "set" $ text "set this area")
                return (selectedRegionE'', selectedAreaE'', clearE'', setAreaE'')
            return (closeE', selectedRegionE', selectedAreaE', clearE', setAreaE')
  return (closeE, never)


selector
  :: ( MonadFix m
     , MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     )
  => Text
  -> Text
  -> Dynamic t (Maybe AreaId)
  -> Dynamic t Areas
  -> m (Event t (Maybe AreaId))
selector cssclass title currentD inputD = do

  let shuffle :: (Int, (AreaId, Area)) -> ((Int, AreaId), Area)
      shuffle (i, (k, v)) = ((i,k), v)
      optionsD = Map.fromList
               . map shuffle
               . zip [1..]
               . OMap.toList
               . unAreas <$> inputD
      mkclass aid aidD = do
        maid <- aidD
        return $ if Just aid == maid then "active" else ""

  divClass ("col " <> cssclass) $ do
    elClass "span" "label" $ text title
    divClass "container" $ do
      el "ul" $ do
        selectE <-
          listViewWithKey optionsD $ \(_k,aid) v -> do
            (li, _) <-
              elDynClass' "li" (mkclass aid currentD) $
                dynText (areaName <$> v)
            return (tag (current v) (domEvent Click li))
        return $ fmap areaId . fmap snd . listToMaybe . Map.toList <$> selectE


selectButton
  :: ( DomBuilder t m
     )
  => Text
  -> m ()
  -> m (Event t ())
selectButton cssclass content =
  divClass "col" $ 
    fmap (domEvent Click . fst) $
      elClass' "button" cssclass $ content
