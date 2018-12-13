{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Bailiwick.View.IndicatorSummary (
    indicatorSummary,
    elDynHtml'
) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Foldable (forM_)
import Data.Maybe (fromMaybe, isJust)
import Text.Read (readMaybe)
import Reflex
       (tagDyn, constDyn, PerformEvent, PostBuild, TriggerEvent, ffor,
        tag, leftmost, Event(..), Dynamic(..), Performable)
import Reflex.Dom.Core
       (elClass', (=:), el, elDynAttr', AttributeName, dyn, EventResult,
        dynText, text, divClass, GhcjsDomSpace, DomBuilder,
        DomBuilderSpace, Element, _element_raw, EventName(Click),
        elDynClass, elAttr)
import GHCJS.DOM.Types (liftJSM, MonadJSM)
import Bailiwick.Types
       (Area(..), AreaSummaries, Areas, Indicators, Indicator(..),
        Features, Feature(..))
import Bailiwick.State
       (getArea, getThemePage, ThemePageArgs(..), Message, State(..))
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map (mapKeys)
import GHCJS.DOM.Element (setInnerHTML)
import Data.Default (Default(..))
import Reflex.Dom.Widget.Input ((.~), (&))
import Reflex.Dom.Builder.Class
       (HasDomEvent(..), DomBuilder(..), InitialAttributes(..))
import Reflex.PostBuild.Class (PostBuild(..))
import Reflex.PerformEvent.Class (PerformEvent(..))
import qualified GHCJS.DOM.Types as DOM (Element(..))
import Data.Monoid ((<>))
import qualified Data.Text as T (pack, strip, replace, unpack)
import qualified Data.Map as M (lookup)
import Reflex.Class (MonadHold(..))

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

elDynHtml'
  :: ( Monad m
     , DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Text
  -> Dynamic t Text
  -> m (Element EventResult (DomBuilderSpace m) t)
elDynHtml' elementTag = elDynHtmlAttr' elementTag (constDyn mempty)

textSubstitution :: Indicators -> Features -> Bool -> State -> Text -> Text
textSubstitution indicators features addCompareArea state =
    let themePage = getThemePage state
        indicator = (`OM.lookup` indicators) =<< themePageIndicatorId <$> themePage
        y = themePageYear <$> themePage
        fy = indicatorFirstYear <$> indicator
        yem = indicatorYearEndMonth =<< indicator
        sa = maybe "New Zealand" areaName $ getArea state
        feature = (`OM.lookup` features) =<< themePageFeatureId =<< themePage
        f = featureName <$> feature
        fp = if isJust f then featureParent =<< feature else Just ""
        d = themePageDetailId <$> themePage --
        dl = Nothing -- TODO d <|> (indicatorTopDetailLabel =<< indicator)
        ip = indicatorPeriod =<< indicator
        p = (-) <$> y <*> ip
        ca = areaName <$> stateCompareArea state
        a = case (addCompareArea, ca) of
              (True, Just ca') -> "<span class='active'>" <> sa <> "</span><span class='compare'> (and " <> ca' <> ")</span>"
              _ -> sa
        fl = case indicatorFeatureText =<< indicator of
              Just ft -> (`M.lookup` ft) =<< themePageFeatureId =<< themePage
              _ -> f <|> (indicatorTopFeatureLabel =<< indicator)
        replace findStr (Just replaceStr) = T.replace findStr replaceStr
        replace _ _ = id
        removeFeatureBrackets :: String -> String
        removeFeatureBrackets =
          if isJust f
            then filter (\c -> c /= '[' && c /= ']')
            else removeFeatureBrackets'
        removeFeatureBrackets' :: String -> String
        removeFeatureBrackets' "" = ""
        removeFeatureBrackets' ('[':xs) = removeFeatureBrackets' . drop 1 $ dropWhile (/= ']') xs
        removeFeatureBrackets' (x:xs) = x:removeFeatureBrackets' xs
        removeDetailBrackets :: String -> String
        removeDetailBrackets =
          if isJust d
            then filter (\c -> c /= '{' && c /= '}')
            else removeDetailBrackets'
        removeDetailBrackets' :: String -> String
        removeDetailBrackets' "" = ""
        removeDetailBrackets' ('{':xs) = removeDetailBrackets' . drop 1 $ dropWhile (/= '}') xs
        removeDetailBrackets' (s:'{':xs) | isSpace s = removeDetailBrackets' . drop 1 $ dropWhile (/= '}') xs
        removeDetailBrackets' (x:xs) = x:removeDetailBrackets' xs
    in T.strip
      . replace "$year$" (T.pack . show <$> y)
      . replace "$firstYear$" fy
      . replace "$yearEndMonth$" yem
      . T.replace "$area$" a
      . T.replace "$selectedArea$" sa
      . replace "$compareArea$" ca
      . replace "$prevYear$" (T.pack . show <$> p)
      . replace "$feature$" fl
      . replace "$featureType$" fp
      . replace "$detail$" dl
      . T.pack
      . removeDetailBrackets
      . removeFeatureBrackets
      . T.unpack


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
  => Areas
  -> AreaSummaries
  -> Indicators
  -> Features
  -> Dynamic t State
  -> m (Event t Message)
indicatorSummary areas areaSummaries indicators features state = mdo
  let indicatorD = ((`OM.lookup` indicators) =<<) . fmap themePageIndicatorId . getThemePage <$> state
      subs = (textSubstitution indicators features True <$> state <*>)

  divClass "summary" $
    divClass "intersection" $ do
      divClass "intersection-number headline-number" $ do
        divClass "number" $ text "TODO"
        divClass "comparison-number" $ text "TODO"
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorHeadlineNumCaption <$> indicatorD
      divClass "intersection-number regional-value" $ do
        divClass "number" $ text "TODO"
        divClass "comparison-number" $ text "TODO"
        void . elDynHtmlAttr' "p" (constDyn $ "class" =: "caption") $
          subs $ maybe "" indicatorLocalNumCaption <$> indicatorD
      divClass "intersection-number national-value" $ do
        divClass "number" $ text "TODO"
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

