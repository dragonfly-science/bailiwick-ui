{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.View.Indicators (
    indicators
  , IndicatorState(..)
) where

import Data.Monoid ((<>))
import Data.Traversable (forM)

import Reflex
import Reflex.Dom.Core

import Bailiwick.Types
import Bailiwick.Route

switchDynM
 :: (MonadHold t m, DomBuilder t m, PostBuild t m)
 => Dynamic t (m (Event t a)) -> m (Event t a)
switchDynM = (switchHold never =<<) . dyn

data IndicatorState t
  = IndicatorState
  { area        :: Dynamic t (Maybe Area)
  , indicator   :: Dynamic t (Maybe IndicatorId)
  , themes      :: Dynamic t (Maybe [Theme])
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
indicators IndicatorState{..} = do
  divClass "themes-outer indicators-section" $ do
    elAttr "span" ("id" =: "indicators") $ return ()
    divClass "content" $
      divClass "themes" $ do
        divClass "theme-top" $
          divClass "section-header" $
            el "p" $ do
              el "i" $ return ()
              text "Indicators for "
              dynText $ maybe "New Zealand" areaName <$> area
        divClass "theme-cards" $ switchDynM $ ffor themes $ \case
          Nothing -> return never
          Just themes' -> do
            fmap leftmost . forM themes' $ \Theme{..} ->
              divClass "theme-card" $ do
                divClass ("card-header card-header-" <> themeId) $ do
                  el "i" $ return ()
                  text themeName
                divClass "card-copy" $
                  el "ul" $
                    fmap leftmost . forM themeIndicators $ \Indicator{..} -> do
                      let ilcss = do
                            indid <- indicator
                            return $
                              if indid == Just indicatorId
                                then "class" =: "indicator-list selected"
                                else "class" =: "indicator-list"
                      click <- fmap (domEvent Click . fst) . elDynAttr' "li" ilcss $
                        text $ capitalize indicatorName
                      return $ GoTo (ThemePage $ ThemePageArgs
                              indicatorId
                              indicatorDefaultChartLeft
                              indicatorDefaultChartRight
                              2017 Nothing Nothing "reg" "indexed" "indexed") <$ click
