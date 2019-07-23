{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick.View.Compare
  ( compareMenu
  )
where

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

  let clickE = fmap (domEvent Click . fst)
      firstArea = fmap snd . listToMaybe . Map.toList

      regionsD = do
        mareas <- areasD
        let areas = maybe OMap.empty unAreas mareas
            regions = OMap.filter (\a -> areaLevel a == "reg") areas
            mnz = OMap.lookup "new-zealand" areas
        return $ case mnz of
                    Just nz -> OMap.singleton "new-zealand" nz <> regions
                    Nothing -> regions
      subareasD = do
        area <- currentRegionD
        mareas <- areasD
        return $
          fromMaybe OMap.empty $ do
            Areas areas <- mareas
            aid <- areaId <$> area
            thisArea <- OMap.lookup aid areas
            return $ OMap.filter (\a -> areaId a `elem` areaChildren thisArea) areas


      shuffle (i, (k, v)) = ((i,k), v)
      regionOptionsD = Map.fromList . map shuffle . zip [1..] . OMap.toList <$> regionsD
      subareaOptionsD = Map.fromList . map shuffle . zip [1..] . OMap.toList <$> subareasD

  currentRegionD :: Dynamic t (Maybe Area)
     <- holdDyn Nothing (firstArea <$> selectedRegionE)

  (closeE, selectedRegionE, selectedAreaE, clearE, setAreaE) <-
    divClass "compare-menu" $ do
      divClass "compare-nav" $ do
        elDynClass "div" (("compare-panel"<>) <$> showhideD) $
          divClass "panel" $ do
            closeE' <-
               el "header" $ do
                 el "h2" $
                   text "Set the area you want to compare with."
                 clickE $
                   elClass' "button" "close" $
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
                    selectedRegionE''' <-
                      divClass "col area-selection" $ do
                        elClass "span" "label" $ text "Selection region"
                        divClass "container" $ do
                          el "ul" $ do
                            listViewWithKey regionOptionsD $ \_k v -> do
                              (li, _) <- el' "li" $ dynText (areaName <$> v)
                              return (tag (current v) (domEvent Click li))
                    selectedAreaE''' <-
                      divClass "col ta-selection" $ do
                        elClass "span" "label" $ text "Selection Territorial Authority/Ward"
                        divClass "container" $ do
                          el "ul" $ do
                            listViewWithKey subareaOptionsD $ \_k v -> do
                              (li, _) <- el' "li" $ dynText (areaName <$> v)
                              return (tag (current v) (domEvent Click li))
                    return (selectedRegionE''', selectedAreaE''')
                (clearE'', setAreaE'') <-
                  divClass "row" $ do
                    clearE''' <-
                      divClass "col" $
                        clickE $ elClass' "button" "clear" $ text "clear"
                    setAreaE''' <-
                      divClass "col" $
                        -- TODO: this button needs to have a "disabled" class added
                        -- if no area is selected
                        clickE $ elClass' "button" "set" $ text "set this area"
                    return (clearE''', setAreaE''')
                return (selectedRegionE'', selectedAreaE'', clearE'', setAreaE'')
            return (closeE', selectedRegionE', selectedAreaE', clearE', setAreaE')
  return (closeE, never)


