{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
module Bailiwick.View
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Fix
import Data.Bool (bool)
import Data.Maybe (isJust)
import Data.Monoid ((<>))

import Language.Javascript.JSaddle.Types (MonadJSM)
import qualified GHCJS.DOM.GlobalEventHandlers as Events (scroll)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Window (scrollTo, getPageYOffset)

import Servant.Reflex
import Reflex.Dom.Core hiding (Home)

import Bailiwick.Store (Store)
import Bailiwick.State
import Bailiwick.Types
import Bailiwick.View.Header (header)
import Bailiwick.View.Map
import Bailiwick.View.AreaSummary (areaSummary)
import Bailiwick.View.Indicators (indicators)
import Bailiwick.View.IndicatorSummary (indicatorSummary)
import Bailiwick.View.IndicatorChart (indicatorChart)
import Bailiwick.View.ToolBar (toolBar)

windowScrolled
  :: (Monad m, MonadJSM m, TriggerEvent t m)
  => m (Event t Double)
windowScrolled = do
  window <- currentWindowUnchecked
  wrapDomEvent window (`on` Events.scroll) $ getPageYOffset window

windowScrollDyn
  :: (Monad m, MonadJSM m, TriggerEvent t m, MonadHold t m)
  => m (Dynamic t Double)
windowScrollDyn = do
  initialPos <- currentWindowUnchecked >>= getPageYOffset
  holdDyn initialPos =<< windowScrolled

view
    :: ( Monad m
       , MonadJSM m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       , SupportsServantReflex t m
       , MonadIO m
       , DomBuilderSpace m ~ GhcjsDomSpace
       )
    => Dynamic t Store -> Dynamic t State -> m (Event t Message)
view storeD stateD = do
  let marginTop (ThemePage _) = bool Nothing (Just "349px") . (> 140)
      marginTop _ = bool Nothing (Just "279px") . (> 140)
  scrollPosD <- windowScrollDyn
  let marginTopD = marginTop <$> (getPage <$> stateD) <*> scrollPosD
  let wholeBodyClass s = "whole-body " <> case getPage s of
                              ThemePage _ -> "theme-whole-body"
                              Summary -> "summary-whole-body"
      mainHeaderClass s isOpen =
         "main-header " <> case getPage s of
                              ThemePage _ -> if isOpen then "large" else "small"
                              Summary -> "closed"
  elDynAttr "div" (("class" =:) <$>
         ((<>) <$> (wholeBodyClass <$> stateD)
               <*> (bool "" " fixed" . isJust <$> marginTopD))) $ mdo
    (headerE, isOpen)
      <- divClass "main-header-area" $
            elDynClass "header" (mainHeaderClass <$> stateD <*> isOpen) $ do
              navBarE <- navbar
              headerE' <- header storeD stateD
              (toolBarE, isOpen') <- toolBar storeD stateD
              return (leftmost [navBarE, headerE', toolBarE], isOpen')
    mainE <-
      elDynAttr "div" (("class" =: "content main-content" <>) .
             maybe mempty (("style" =:) . ("margin-top: " <>)) <$> marginTopD) $
        mainContent storeD stateD
    indicatorsE <- indicators storeD stateD

    -- We need to scroll up when these links are clicked (or you can't tell they do anything)
    performEvent_ $ ffor indicatorsE $ \case
      GoTo _ -> do
        window <- currentWindowUnchecked
        scrollTo window 0 185
      _ -> return ()

    footer
    return $ leftmost [headerE, mainE, indicatorsE]


navbar :: (Monad m, DomBuilder t m) => m (Event t Message)
navbar =
  elClass "nav" "content" $ do
    (logo, _) <- elAttr' "a" ("class" =: "logo font--droid-serif") $ do
      el "span" $ text "Regional"
      el "strong" $ text "Economic Activity"
      el "span" $ text "Web Tool"

    home <- divClass "menu-items" $ do
      home <- divClass "links" $ do
        (home, _) <- elAttr' "a" ("class" =: "home button") $ do
          el "i" $ return ()
          el "span" $ text "Home"
        elAttr "a" ("class" =: "indicators left" <> "href" =: "#indicators") $
          el "span" $ do
            text "Indicators"
            el "i" $ return ()
        return home
      divClass "menu-buttons" $
        elClass "button" "share" $ do
          el "i" $ return ()
          el "span" $ text "share"
      return home
    return $ GoToHomePage <$ leftmost [domEvent Click logo, domEvent Click home]


type ContentConstraints t m =
    ( Monad m
    , MonadJSM m
    , DomBuilder t m
    , MonadFix m
    , PostBuild t m
    , TriggerEvent t m
    , PerformEvent t m
    , HasJSContext (Performable m)
    , MonadJSM (Performable m)
    , MonadIO m
    , MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace
    )

mainContent
    :: ContentConstraints t m
    => Dynamic t Store
    -> Dynamic t State
    -> m (Event t Message)
mainContent storeD stateD = do
  isSummary <- holdUniqDyn ((== Summary) . getPage <$> stateD)
  switchHold never =<< dyn (ffor isSummary $ \case
    True -> summaryContent storeD stateD
    False -> indicatorContent storeD stateD)

summaryContent
    :: ContentConstraints t m
    => Dynamic t Store
    -> Dynamic t State
    -> m (Event t Message)
summaryContent storeD stateD =
  divClass "central-content summary" $ do
    messages
     <-
       divClass "navigation-map base-map" $ do
         zoomClick <- summaryText stateD
         mapClicks <-
           divClass "svg-wrapper" $
             nzmap storeD stateD
         return $ leftmost [zoomClick, mapClicks]

    summaryMessages <- divClass "area-summary" $
      areaSummary storeD stateD
    return $ leftmost [messages, summaryMessages]

indicatorContent
    :: ContentConstraints t m
    => Dynamic t Store
    -> Dynamic t State
    -> m (Event t Message)
indicatorContent storeD stateD = do
  contentE <- divClass "central-content indicator" $ do
    mapE <- divClass "indicator-map base-map" $
      divClass "map-wrapper" $ do
        zoomClick <- divClass "map-options" $ do
          divClass "zoom-controls map-zoom active" $ do
            el "label" $ do
              elAttr "input" ("type" =: "radio" <> "name" =: "map-zoom-left") $ return ()
              elClass "span" "zoom-in" $ return ()
            el "label" $ do
              elAttr "input" ("type" =: "radio" <> "name" =: "map-zoom-left") $ return ()
              elClass "span" "zoom-out" $ return ()
          return never --TODO

        mapClicks <-
          divClass "svg-wrapper" $
            nzmap storeD stateD
        return $ leftmost [zoomClick, mapClicks]
    chartE <- divClass "indicator-chart" $
      indicatorChart storeD stateD
    return $ leftmost [ mapE, chartE ]
  summaryE <- divClass "indicator-summary hide-table no-compare" $
    indicatorSummary storeD stateD
  return $ leftmost [contentE, summaryE]

summaryText
  :: ( DomBuilder t m
     , PostBuild t m )
  => Dynamic t State
  -> m (Event t Message)
summaryText stateD = do

  let homeAttr = stateD >>= \case
            State Summary [] _ _ -> return ("class" =: "text-wrapper" <> "style" =: "display: block")
            _    -> return ("class" =: "text-wrapper" <> "style" =: "display: none")
      summaryAttr = stateD >>= \case
            State Summary [] _ _ -> return ("class" =: "text-wrapper" <> "style" =: "display: none")
            _    -> return ("class" =: "text-wrapper" <> "style" =: "display: block")

      -- Zoom in and out button
      zoomed = hasAdapter Mapzoom <$> stateD
      zoomAttr = zoomed >>= \case
                    True -> return ( "class" =: "zoom-out-small")
                    False -> return ( "class" =: "zoom-in-small")
      zoomText = zoomed >>= \case
                    True -> return "Zoom out"
                    False -> return "Zoom in"

      dispArea = maybe "" areaName . getArea <$> stateD

  elDynAttr "div" homeAttr $
    divClass "background-wrapper" $ do
      divClass "intro-paragraph" $
        text "Welcome to the interactive Regional Economic Activity Web Tool."
      elClass "p" "body-paragraph" $
        text "This tool allows you to compare regions' economic performance, distinguish their attributes and specialisations, and understand the different roles they play in the New Zealand economy."
      elClass "p" "body-paragraph" $
        text "Click on regions to compare, or go straight into the detail by exploring the themed indicators. All data sets are annualised in order to make comparison easier and maximise the data available."
      elClass "p" "body-paragraph" $
        text "The tool is updated regularly, but more recent data may be available at its source, especially if it is frequently updated. Where possible, we include a link back to the source so you can check if more recent data is available."
  elDynAttr "div" summaryAttr $ do
    divClass "background-wrapper" $ do
      divClass "intro-paragraph" $
        dynText dispArea
      elClass "p" "body-paragraph" $ do
        text "Zoom in to compare different areas of "
        dynText dispArea
      elClass "p" "body-paragraph" $
        text "You can go into more detail by exploring the indicators below"
    (zoom, _)
       <- elAttr' "div" ("class" =: "map-zoom") $ do
           dynText zoomText
           elDynAttr "span" zoomAttr $ return ()
    return $ ffor (tagPromptlyDyn stateD (domEvent Click zoom)) $ \case
      s | hasAdapter Mapzoom s -> ZoomOut . fmap areaId $ getRegion s
      _ -> ZoomIn

footer :: (Monad m, DomBuilder t m) => m ()
footer =
  el "footer" $
    divClass "content" $
      divClass "bottombar" $ do
        divClass "icons" $ do
          elAttr "a" ("href" =: "/" <> "class" =: "logo font--droid-serif") $ do
            el "span" $ text "Regional"
            el "strong" $ text "Economic Activity"
            el "span" $ text "Web Tool"
          elAttr "a" ( "rel"   =: "license"
                     <> "href" =: "http://creativecommons.org/licenses/by/4.0/"
                     <> "alt"  =: "Creative Commons Attribution 4.0 International Licence" ) $ do
            divClass "cc-cc" $ return ()
            divClass "cc-by" $ return ()
        divClass "statement" $ do
          el "p" $ do
            text "Crown copyright ©. Copyright material on the "
            elAttr "span" ("property" =: "http://purl.org/dc/terms/title") $
                text "Regional Economic Activity interactive visualisation"
            text " is protected by copyright owned by "
            elAttr "a" ( "href" =: "http://www.mbie.govt.nz/"
                       <>"vocab" =: "http://creativecommons.org/ns#"
                       <>"property" =: "attributionURL" ) $
                elAttr "span" ("property"=:"attributionName") $
                    text "Ministry for Business, Innovation and Employment"
            text ". Unless indicated otherwise, this copyright material is licensed for re-use under a "
            elAttr "a" (  "rel" =: "license"
                       <> "href" =: "http://creativecommons.org/licenses/by/4.0/") $
                text "Creative Commons Attribution 4.0 International Licence"
            text ". Please note that this licence does not apply to any logos, design elements, or photography."
          el "p" $ do
            text "The Regional Economic Activity data visualisation is a collaboration between "
            elAttr "a" ("href"=:"http://www.dragonfly.co.nz/regional-economic-activity-report.html") $
                text "Dragonfly Data Science"
            text ", "
            elAttr "a" ("href"=:"http://saltedherring.co.nz") $ text "Salted Herring"
            text ", and the "
            elAttr "a" ("href"=:"http://mbie.govt.nz") $
                text "Ministry for Business, Innovation and Employment"
            text "."
