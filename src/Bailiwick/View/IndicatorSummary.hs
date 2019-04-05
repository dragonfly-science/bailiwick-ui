{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Bailiwick.View.IndicatorSummary (
    indicatorSummary
  , IndicatorSummaryState(..)
) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, isJust)
import Data.Map (Map)
import qualified Data.Map as M (lookup)
import Data.Text (Text)
import qualified Data.Text as T (pack, strip, replace, unpack)
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

import Reflex
import Reflex.Dom.Core hiding (elDynHtmlAttr')
import GHCJS.DOM.Types (liftJSM, MonadJSM)
import GHCJS.DOM.Element (setInnerHTML)

import Bailiwick.Types
import Bailiwick.Route

elDynHtmlAttr'
  :: ( Monad m
     , DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Text
  -> Dynamic t (Map Text Text)
  -> Dynamic t Text
  -> m (Element EventResult (DomBuilderSpace m) t)
elDynHtmlAttr' elementTag attrs html = do
  (e, _) <- elDynAttr' elementTag attrs $ return ()
  postBuild <- getPostBuild
  performEvent_ $ liftJSM . setInnerHTML (_element_raw e) <$> leftmost [updated html, tag (current html) postBuild]
  return e

data IndicatorSummaryState t
  = IndicatorSummaryState
  { routeD             :: Dynamic t Route
  , areaD              :: Dynamic t (Maybe Area)
  , compareAreaD       :: Dynamic t (Maybe Area)
  , featureD           :: Dynamic t (Maybe Feature)
  , indicatorD         :: Dynamic t (Maybe Indicator)
  , indicatorSummaryD  :: Dynamic t IndicatorSummary
  }


indicatorSummary
  :: forall m t.
     ( Monad m
     , MonadFix m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => IndicatorSummaryState t
  -> m (Event t Message)
indicatorSummary IndicatorSummaryState{..} = mdo

  let subs = (textSubstitution
                <$> areaD
                <*> compareAreaD
                <*> indicatorD
                <*> featureD
                <*> (getThemePage <$> routeD)
                <*>)
      summaryNumsD = do
        mareaid <- fmap areaId <$> areaD
        myear   <- fmap themePageYear . getThemePage <$> routeD
        indicatorSummaryNumbers <- indicatorSummaryD
        return $ fromMaybe (SummaryNums ["", "", ""]) $ do
          areaid <- mareaid
          year <- myear
          OM.lookup (areaid, year) indicatorSummaryNumbers

  divClass "summary" $
    divClass "intersection" $ do
      divClass "intersection-number headline-number" $ do
        divClass "number" $ dynText (headlineNum <$> summaryNumsD)
        divClass "comparison-number" $ text "TODO"
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorHeadlineNumCaption <$> indicatorD
      divClass "intersection-number regional-value" $ do
        divClass "number" $ dynText (localNum <$> summaryNumsD)
        divClass "comparison-number" $ text "TODO"
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorLocalNumCaption <$> indicatorD
      divClass "intersection-number national-value" $ do
        divClass "number" $ dynText (nationalNum <$> summaryNumsD)
        divClass "comparison-number" $ text "TODO"
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorNationalNumCaption <$> indicatorD
  divClass "summary-links" $ do
    void . elDynHtmlAttr' "div" (constDyn $ "class" =: "source") $
           ("Source: " <>) <$> subs (maybe "" indicatorPublishers <$> indicatorD)
    divClass "notes" $
      el "div" $ do
        text "Notes:"
        void . dyn $ ffor (fromMaybe [] . (indicatorNotes =<<) <$> indicatorD)
                       (mapM_ $ elDynHtmlAttr' "p" (constDyn mempty) . subs . constDyn)
  elDynClass "div" (("table-view " <>) . bool "hide" "show" <$> showTableD) $
    elAttr "div" ("class" =: "panel" <> "style" =: "height: 799px;") $ do
      el "header" $
        divClass "table-caption text" $
          text "TODO"
      elAttr "div" ("class" =: "table-container" <> "style" =: "height: 657px;") $
        el "div" $
          text "TODO"

  showTableD <- holdDyn False $ not <$> tag (current showTableD) showTableE
  showTableE <- fmap (domEvent Click . fst) $
    divClass "table-button" $
      elClass' "button" "show-table" $
        el "span" $ do
          el "i" $ return ()
          dynText (bool "show table" "hide table" <$> showTableD)

  return never




textSubstitution
  :: Maybe Area
  -> Maybe Area
  -> Maybe Indicator
  -> Maybe Feature
  -> Maybe ThemePageArgs
  -> Text
  -> Text
textSubstitution area compareArea indicator feature themePage =
    let y = themePageYear <$> themePage
        fy = indicatorFirstYear <$> indicator
        yem = indicatorYearEndMonth =<< indicator
        sa = maybe "New Zealand" areaName area
        f = featureName <$> feature
        fp = if isJust f then featureParent =<< feature else Just ""
        _d = themePageDetailId <$> themePage --
        dl = Nothing -- TODO d <|> (indicatorTopDetailLabel =<< indicator)
        ip = indicatorPeriod =<< indicator
        p = (-) <$> y <*> ip
        a = case (areaName <$> compareArea) of
              Just ca' ->
                     "<span class='active'>" <> sa <>
                     "</span><span class='compare'> (and " <> ca' <> ")</span>"
              _ -> sa
        fl = case indicatorFeatureText =<< indicator of
              Just ft -> (`M.lookup` ft) =<< themePageFeatureId =<< themePage
              _ -> f <|> (indicatorTopFeatureLabel =<< indicator)
        replace findStr (Just replaceStr) = T.replace findStr replaceStr
        replace _ _ = id
    in T.strip
      . replace "$year$" (T.pack . show <$> y)
      . replace "$firstYear$" fy
      . replace "$yearEndMonth$" yem
      . T.replace "$area$" a
      . T.replace "$selectedArea$" sa
      . replace "$compareArea$" (areaName <$> compareArea)
      . replace "$prevYear$" (T.pack . show <$> p)
      . replace "$feature$" fl
      . replace "$featureType$" fp
      . replace "$detail$" dl
      . T.pack
      . removeDetailBrackets (themePageDetailId =<< themePage)
      . removeFeatureBrackets (featureName <$> feature)
      . T.unpack


removeFeatureBrackets :: Maybe Text -> String -> String
removeFeatureBrackets feature =
  if isJust feature
    then filter (\c -> c /= '[' && c /= ']')
    else go
  where
    go :: String -> String
    go "" = ""
    go ('[':xs) = go . drop 1 $ dropWhile (/= ']') xs
    go (x:xs) = x:go xs

removeDetailBrackets :: Maybe Text -> String -> String
removeDetailBrackets detail =
  if isJust detail
    then filter (\c -> c /= '{' && c /= '}')
    else go
  where
    go :: String -> String
    go "" = ""
    go ('{':xs) = go . drop 1 $ dropWhile (/= '}') xs
    go (s:'{':xs) | isSpace s = go . drop 1 $ dropWhile (/= '}') xs
    go (x:xs) = x:go xs

