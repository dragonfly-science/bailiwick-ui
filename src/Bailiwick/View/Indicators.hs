{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Bailiwick.View.Indicators (indicators) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Traversable (forM)

import Reflex
import Reflex.Dom.Core

import Bailiwick.View.Text (capitalize)
import Bailiwick.Javascript (switchDynM)
import Bailiwick.State
       (State(State, selectedAreaD, indicatorD, yearD, areaTypeD, themesD))
import Bailiwick.Types
import Bailiwick.Route


indicators
  :: forall m t.
     ( Monad m
     , MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     )
  => State t
  -> m (Event t Message)
indicators st@State{selectedAreaD,indicatorD,themesD} = do
  let areaD = toMaybe <$> selectedAreaD
  divClass "themes-outer indicators-section" $ do
    elAttr "span" ("id" =: "indicators") $ return ()
    divClass "content" $
      divClass "themes" $ do
        divClass "theme-top" $
          divClass "section-header" $
            el "p" $ do
              el "i" $ return ()
              text "Indicators for "
              dynText $ maybe "New Zealand" areaName <$> areaD
        divClass "theme-cards" $ switchDynM $ ffor (toMaybe <$> themesD) $ \case
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
                            indid <- fmap indicatorId <$> (toMaybe <$> indicatorD)
                            noDataIndicators <- fmap areaNoData <$> areaD
                            return $
                              if indid == Just (indicatorId ind)
                                then "class" =: "indicator-list selected"
                              else if (indicatorId ind) `elem` (fromMaybe [] noDataIndicators)
                                then "class" =: "indicator-list nodata"
                                else "class" =: "indicator-list"
                      click <- fmap (domEvent Click . fst) . elDynAttr' "li" ilcss $
                        text $ capitalize (indicatorName ind)
                      return $ tag (current (makeGoto ind st)) click

makeGoto
  :: Monad (Dynamic t)
  => Indicator
  -> State t
  -> Dynamic t Message
makeGoto Indicator{..} State{yearD,areaTypeD} = do
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
                      "absolute")
