{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Bailiwick.View.Indicators (
    indicators
  , IndicatorState(..)
) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Traversable (forM)

import Reflex
import Reflex.Dom.Core

import Bailiwick.View.Text (capitalize)
import Bailiwick.Javascript (switchDynM)
import Bailiwick.Types
import Bailiwick.Route

data IndicatorState t
  = IndicatorState
  { selectedAreaD  :: Dynamic t (Maybe Area)
  , indicatorD     :: Dynamic t (Maybe Indicator)
  , yearD          :: Dynamic t (Maybe Year)
  , areaTypeD      :: Dynamic t (Maybe AreaType)
  , themesD        :: Dynamic t (Maybe [Theme])
  }



indicators
  :: forall m t.
     ( Monad m
     , MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     )
  => IndicatorState t
  -> m (Event t Message)
indicators is@IndicatorState{..} = do
  divClass "themes-outer indicators-section" $ do
    elAttr "span" ("id" =: "indicators") $ return ()
    divClass "content" $
      divClass "themes" $ do
        divClass "theme-top" $
          divClass "section-header" $
            el "p" $ do
              el "i" $ return ()
              text "Indicators for "
              dynText $ maybe "New Zealand" areaName <$> selectedAreaD
        divClass "theme-cards" $ switchDynM $ ffor themesD $ \case
          Nothing -> return never
          Just themes -> do
            fmap leftmost . forM themes $ \Theme{..} ->
              divClass "theme-card" $ do
                divClass ("card-header card-header-" <> themeId) $ do
                  el "i" $ return ()
                  text themeName
                divClass "card-copy" $
                  el "ul" $
                    fmap leftmost . forM themeIndicators $ \ind -> do
                      let ilcss = do
                            indid <- fmap indicatorId <$> indicatorD
                            return $
                              if indid == Just (indicatorId ind)
                                then "class" =: "indicator-list selected"
                                else "class" =: "indicator-list"
                      click <- fmap (domEvent Click . fst) . elDynAttr' "li" ilcss $
                        text $ capitalize (indicatorName ind)
                      return $ tagPromptlyDyn (makeGoto ind is) click

makeGoto
  :: Monad (Dynamic t)
  => Indicator
  -> IndicatorState t
  -> Dynamic t Message
makeGoto Indicator{..} IndicatorState{..} = do
  myear <- yearD
  areatype <- areaTypeD
  let year = case myear of
                Just y -> if y `elem` indicatorYears
                            then y
                            else maximum indicatorYears
                Nothing -> maximum indicatorYears
  return $
    GoTo (ThemePage $ ThemePageArgs
                      indicatorId
                      indicatorDefaultChartRight
                      year
                      (FeatureId <$> indicatorDefaultFeature)
                      Nothing
                      (fromMaybe "reg" areatype)
                      "indexed")
