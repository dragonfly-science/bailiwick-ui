{-# LANGUAGE OverloadedStrings #-}
module Bailiwick.View.IndicatorChart (
  indicatorChart
) where

import Reflex.Dom.Core (display, text, DomBuilder)
import Bailiwick.AreaTrees (AreaTrees)
import Reflex (PostBuild, Event(..), Dynamic(..))
import Bailiwick.State
       (getThemePage, ThemePageArgs(..), Message, State(..))
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

indicatorChart
  :: ( Monad m
     , DomBuilder t m
     , PostBuild t m
     )
  => AreaTrees
  -> Dynamic t State
  -> m (Event t Message)
indicatorChart areaTrees state = do
  let areaTree = -- ((`OM.lookup` areaTrees) =<<) .
                  fmap themePageIndicatorId . getThemePage <$> state
  display areaTree
--  text "TODO" >>
  return never
