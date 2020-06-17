{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
module Bailiwick.View.Header
where

import Control.Monad.Fix
import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, listToMaybe)

import Data.Text (Text, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Reflex.Dom.Core hiding (Home)

import Bailiwick.View.Text
import Bailiwick.View.Compare
import Bailiwick.Route
import Bailiwick.State
       (State(State, isSummaryD, regionD, areaD, compareAreaD, yearD, featureD, areasD,
              indicatorD),
        getDataAreasD)
import Bailiwick.Types

header
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => State t -> m (Event t Message)
header st@State{regionD,areaD,compareAreaD,featureD,areasD,indicatorD} = mdo
  let mareaD = toMaybe <$> regionD
      subareaD = toMaybe <$> areaD
      mareasD = toMaybe <$> areasD
      mindicatorD = toMaybe <$> indicatorD
      indicatorDataAreasD = toMaybe <$> (getDataAreasD st)

  let background = do
        area <- maybe "new-zealand" areaId <$> mareaD
        return $ (  "class" =: "title" <> "data-region" =: area)

      showSubareaD = do
        subareas <- subareasD
        if subareas == OMap.empty
            then return NotVisible
            else return Visible

      showFeaturesD = do
        mind <- mindicatorD
        return $ fromMaybe NotVisible $ do
            Indicator{..} <- mind
            if indicatorFeatures == []
                then return NotVisible
                else return Inverted

      subAreaMessage = do
        area <- fmap areaId <$> mareaD
        if area == Just "auckland"
            then return "Select a ward"
            else return "Select a territorial authority"

      featureMessage = do
        return "Select a feature"

      regionsD = do
        mareas <- mareasD
        let areas = maybe OMap.empty unAreas mareas
            regions = OMap.filter (\a -> areaLevel a == "reg") areas
            mnz = OMap.lookup "new-zealand" areas
        return $ case mnz of
                    Just nz -> OMap.singleton "new-zealand" nz <> regions
                    Nothing -> regions

      subareasD = do
        area <- mareaD
        mareas <- mareasD
        mdataareas <- indicatorDataAreasD
        return $
          fromMaybe OMap.empty $ do
            Areas areas <- mareas
            aid <- areaId <$> area
            dataareas <- mdataareas
            thisArea <- OMap.lookup aid areas
            return $ OMap.filter
                     (\a -> areaId a `elem` areaChildren thisArea
                            && areaId a `elem` dataareas)
                     areas

      featuresD = do
        mind <- mindicatorD
        return $
          fromMaybe OMap.empty $ do
            Indicator{..} <- mind
            ift <- indicatorFeatureText
            return $ OMap.mapKeys featureIdText ift


  elDynAttr "div" background $
    divClass "content" $ mdo
      backToSummaryE <-
        divClass "left" $ do
          backToSummary st
      menuE <-
        divClass "right" $
          divClass "title-menus" $ do
            (region, regionOpen) <-
              dropdownMenu (constDyn "Select a region")
                           never
                           (constDyn Visible)
                           (fmap areaId <$> mareaD)
                           (fmap areaName <$> regionsD)
            (subarea, _) <-
              dropdownMenu subAreaMessage
                           (() <$ ffilter id (updated regionOpen))
                           showSubareaD
                           (fmap areaId <$> subareaD)
                           (fmap areaName <$> subareasD)
            (feature, _) <-
              dropdownMenu featureMessage
                           (() <$ ffilter id (updated regionOpen))
                           showFeaturesD
                           (fmap featureIdText <$> featureD)
                           (featuresD)

            uniqRegion <- holdUniqDyn region
            uniqSubarea <- holdUniqDyn subarea
            uniqFeature <- holdUniqDyn feature

            let makeSetSubArea sa =
                 if "auckland" `isPrefixOf` sa
                   then SetSubArea "ward" sa
                   else SetSubArea "ta" sa

            return $ leftmost [ makeSetSubArea <$> fmapMaybe id (updated uniqSubarea)
                              , SetRegion <$> fmapMaybe id (updated uniqRegion)
                              , SetFeature . FeatureId <$> fmapMaybe id (updated uniqFeature)
                              ]
      compareE <- compareMenu (load Nothing id <$> compareAreaD) mareasD
      return $ leftmost [backToSummaryE, menuE, compareE]

backToSummary
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => State t -> m (Event t Message)
backToSummary
  State{isSummaryD,regionD,areaD,yearD,featureD,indicatorD} = do
  let mareaD = toMaybe <$> regionD
      subareaD = toMaybe <$> areaD
      mindicatorD = toMaybe <$> indicatorD

  let displayNone = "style" =: "display: none;"
      disp boolD cssclass  = do
        bool <- boolD
        return $
          if bool
            then "class" =: cssclass <> mempty
            else "class" =: cssclass <> displayNone
      notSummaryD = not <$> isSummaryD

  let subs = (textSubstitution
                <$> ((<|>) <$> subareaD <*> mareaD)
                <*> (constDyn Nothing)
                <*> (mindicatorD)
                <*> featureD
                <*> (constDyn Nothing)
                <*> yearD
                <*>)

  (e, _) <-
    elDynAttr' "div" (disp notSummaryD "back-to-summary context-text") $
      el "a" $ do
        elClass "i" "fa fa-arrow-left" $ return ()
        text " Back to summary page"
  elDynAttr "div" (disp notSummaryD "page-header indicator-page-header") $ do
    elAttr "div" ("id" =: "header-title") $ do
      dynText $ fmap capitalize $ subs $ maybe "" indicatorHeaderTitle <$> mindicatorD
  elDynAttr "span" (disp isSummaryD "block-label context-text") $ text "You're looking at"
  elDynAttr "div" (disp isSummaryD "page-header summary-page-header") $ do
    el "div" $ do
      dynText $ maybe "" areaName <$> mareaD
      dynText $ maybe "" (const ":") <$> subareaD
    el "div" $ dynText $ maybe "" areaName <$> subareaD
  return $ GoTo Summary <$ domEvent Click e

data Visible = NotVisible | Visible | Inverted deriving (Eq)

dropdownMenu
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => Dynamic t Text              -- empty value presentation
    -> Event t ()                  -- Close event
    -> Dynamic t Visible           -- Is hidden, inverted, or visible
    -> Dynamic t (Maybe Text)      -- Initial value
    -> Dynamic t (InsOrdHashMap Text Text)    -- Options (ordered)
    -> m (Dynamic t (Maybe Text), Dynamic t Bool)
dropdownMenu emptyPresentD closeE seenD initialD valuesD = do

  let dropdownAttrD = do
        canSee <- seenD
        let visibility = case canSee of
                            NotVisible -> "visibility: hidden"
                            _          -> "visibility: block"
        let classname = case canSee of
                            Inverted   -> "dropdown-invert"
                            _          -> "dropdown"
        return ("class" =: classname <> "style" =: visibility)

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
                return $ fromMaybe "" $ OMap.lookup val values

          ulClass = ffor open $ \isOpen ->
              if isOpen then "dropdown-menu dropdown-select show-menu"
                        else "dropdown-menu dropdown-select"

          shuffle (i, (k, v)) = ((i,k), v)
          optionsD = Map.fromList . map shuffle . zip [1..] . OMap.toList <$> valuesD

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


