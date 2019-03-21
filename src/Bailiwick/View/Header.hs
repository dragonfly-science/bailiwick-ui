{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
module Bailiwick.View.Header
where

import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, listToMaybe)

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OMap
import Reflex.Dom.Core hiding (Home)

import Bailiwick.State (HeaderState(..))
import qualified Bailiwick.State as State
import Bailiwick.Route
import Bailiwick.Types

type AreaSlug = Text

header
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => HeaderState t -> m (Event t Message)
header HeaderState{..} = mdo
  let urlRegion = areaId <$> areaD
      urlSubarea = fmap areaId <$> subareaD

      background = do
        reg <- urlRegion
        return $ (  "class" =: "title" <> "data-region" =: reg)

      dispRegion = areaName <$> areaD
      dispConnect = maybe "" (const ":") <$> subareaD
      dispSubArea = maybe "" areaName <$> subareaD

      showSubareaD = not . ( == OMap.empty) <$> subareasD

      subAreaMessage = do
        reg <- urlRegion
        if reg == "auckland"
            then return "Select a ward"
            else return "Select a territorial authority"

      regionsD =
        let regions = OMap.filter (\a -> areaLevel a == "reg") (unAreas areas)
            Just nz = OMap.lookup "new-zealand" (unAreas areas)
        in  constDyn $ OMap.singleton "new-zealand" nz <> regions

      subareasD = do
        reg <- areaD
        return $
          fromMaybe OMap.empty $ do
            thisArea <- OMap.lookup (areaId reg) (unAreas areas)
            return $ OMap.filter (\a -> areaId a `elem` areaChildren thisArea) (unAreas areas)

  elDynAttr "div" background $
    divClass "content" $ mdo
      backToSummaryE <-
        divClass "left" $ do
          backToSummary pageD dispRegion dispConnect dispSubArea
      menuE <-
        divClass "right" $
          divClass "title-menus" $ do
            (region, regionOpen) <-
              dropdownMenu (constDyn "Select a region") never
                           (constDyn True) (Just <$> urlRegion)
                           (fmap areaName <$> regionsD)
            (subarea, _) <-
              dropdownMenu subAreaMessage
                           (() <$ ffilter id (updated regionOpen))
                           showSubareaD urlSubarea
                           (fmap areaName <$> subareasD)

            uniqRegion <- holdUniqDyn region
            uniqSubarea <- holdUniqDyn subarea

            return $ leftmost [ SetSubArea <$> fmapMaybe id (updated uniqSubarea)
                              , SetRegion <$> fmapMaybe id (updated uniqRegion)
                              ]
      return $ leftmost [backToSummaryE, menuE]

backToSummary
    :: ( MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => Dynamic t Page              -- current page
    -> Dynamic t Text              -- region
    -> Dynamic t Text              -- connect
    -> Dynamic t Text              -- subarea
    -> m (Event t Message)
backToSummary pageD regionD connectD subAreaD= do
  let displayNone = "style" =: "display: none;"
      divcssD = do
        page <- pageD
        return $
          if page /= Summary
            then "class" =: "back-to-summary context-text" <> displayNone
            else "class" =: "back-to-summary context-text" <> mempty
      spancssD = do
        page <- pageD
        return $
          if page == Summary
            then "class" =: "block-label context-text" <> displayNone
            else "class" =: "block-label context-text" <> mempty
  (e, _) <-
    elDynAttr' "div" divcssD $
      el "a" $ do
        elClass "i" "fa fa-arrow-left" $ return ()
        text "Back to summary page"
  elDynAttr "span" spancssD $ text "You're looking at"
  divClass "page-header summary-page-header" $ do
    el "div" $ do
      dynText regionD
      dynText connectD
    el "div" $ dynText subAreaD
  return $ GoTo Summary <$ domEvent Click e

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
    -> Dynamic t (InsOrdHashMap Text Text)    -- Options (ordered)
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


