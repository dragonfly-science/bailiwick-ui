{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
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

import Bailiwick.State
import Bailiwick.Route
import Bailiwick.Types
import Bailiwick.View.Header (header)
import Bailiwick.View.Map
import Bailiwick.View.AreaSummary (areaSummary)
import Bailiwick.View.Indicators (indicators)
--import Bailiwick.View.IndicatorSummary (indicatorSummary)
--import Bailiwick.View.IndicatorChart (indicatorChart)
import Bailiwick.View.ToolBar (toolBar)

switchDynM
 :: (MonadHold t m, DomBuilder t m, PostBuild t m)
 => Dynamic t (m (Event t a)) -> m (Event t a)
switchDynM = (switchHold never =<<) . dyn

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
    => Dynamic t (State t) -> m (Event t Message)
view stateD = do
  scrollPosD <- windowScrollDyn
  let marginTop (ThemePage _) = bool Nothing (Just "349px") . (> 140)
      marginTop _ = bool Nothing (Just "279px") . (> 140)
      marginTopD = marginTop <$> (getPage <$> stateD) <*> scrollPosD
      wholeBodyClass s = "whole-body " <> case getPage s of
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
              headerE' <- switchDynM $ ffor stateD $ \case
                  Waiting    -> return never
                  State{..}  -> header headerState
              (toolBarE, isOpen') <- mdo
                  eithersE <-
                     switchDynM $ ffor stateD $ \case
                                      State{..} -> toolBar isOpen toolBarState
                                      _         -> return never
                  let (isOpenE, toolBarE) = fanEither eithersE
                  isOpen <- foldDyn (const not) False $ isOpenE
                  return (toolBarE, isOpen)
              return (leftmost [navBarE, headerE', toolBarE], isOpen')
    mainE <-
      elDynAttr "div" (("class" =: "content main-content" <>) .
             maybe mempty (("style" =:) . ("margin-top: " <>)) <$> marginTopD) $
        mainContent stateD
    indicatorsE <- switchDynM $ ffor stateD $ \case
        Waiting   -> return never
        State{..} -> indicators indicatorState

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
    => Dynamic t (State t)
    -> m (Event t Message)
mainContent stateD = do
  isSummary <- holdUniqDyn ((== Summary) . getPage <$> stateD)
  switchHold never =<< dyn (ffor isSummary $ \case
    True  -> summaryContent stateD
    False -> indicatorContent stateD)

summaryContent
    :: ContentConstraints t m
    => Dynamic t (State t)
    -> m (Event t Message)
summaryContent stateD =
  divClass "central-content summary" $ do
    messagesE
     <-
       divClass "navigation-map base-map" $ do
         zoomClick <- switchDynM $ ffor stateD $ \case
                Waiting    -> return never
                State{..}  -> summaryText route area
         mapClicks <-
           divClass "svg-wrapper" $
             -- nzmap stateD
             return never
         return $ leftmost [zoomClick, mapClicks]

    summaryMessagesE <- divClass "area-summary" $ switchDynM $ ffor stateD $ \case
          Waiting   -> return never
          State{..} -> areaSummary areaSummaryState
    return $ leftmost [messagesE, summaryMessagesE]

indicatorContent
    :: ContentConstraints t m
    => Dynamic t (State t)
    -> m (Event t Message)
indicatorContent stateD = do
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
            nzmap stateD
        return $ leftmost [zoomClick, mapClicks]
    chartE <- divClass "indicator-chart" $
      --indicatorChart stateD
      return never
    return $ leftmost [ mapE, chartE ]
  summaryE <- divClass "indicator-summary hide-table no-compare" $
    --indicatorSummary stateD
    return never
  return $ leftmost [contentE, summaryE]

summaryText
  :: ( DomBuilder t m
     , PostBuild t m )
  => Route
  -> Dynamic t Area
  -> m (Event t Message)
summaryText route areaD = do

  let page = routePage route
      homeAttr = case page of
            Summary -> ("class" =: "text-wrapper" <> "style" =: "display: block")
            _    -> ("class" =: "text-wrapper" <> "style" =: "display: none")
      summaryAttr = case page of
            Summary -> ("class" =: "text-wrapper" <> "style" =: "display: none")
            _    -> ("class" =: "text-wrapper" <> "style" =: "display: block")

      -- Zoom in and out button
      zoomed = hasAdapter Mapzoom route
      zoomAttr = case zoomed of
                    True -> ( "class" =: "zoom-out-small")
                    False -> ( "class" =: "zoom-in-small")
      zoomText = case zoomed of
                    True -> "Zoom out"
                    False -> "Zoom in"

      dispArea = areaName <$> areaD

  elAttr "div" homeAttr $
    divClass "background-wrapper" $ do
      divClass "intro-paragraph" $
        text "Welcome to the interactive Regional Economic Activity Web Tool."
      elClass "p" "body-paragraph" $
        text $ "This tool allows you to compare regions' economic performance, "
            <> "distinguish their attributes and specialisations, and "
            <> "understand the different roles they play in the New Zealand economy."
      elClass "p" "body-paragraph" $
        text $ "Click on regions to compare, or go straight into the detail "
            <> "by exploring the themed indicators. All data sets are "
            <> "annualised in order to make comparison easier and maximise "
            <> "the data available."
      elClass "p" "body-paragraph" $
        text $ "The tool is updated regularly, but more recent data may be "
            <> "available at its source, especially if it is frequently "
            <> "updated. Where possible, we include a link back to the source "
            <> "so you can check if more recent data is available."
  elAttr "div" summaryAttr $ do
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
           text zoomText
           elAttr "span" zoomAttr $ return ()
    return $ ffor (tagPromptlyDyn ((route,) <$> areaD) (domEvent Click zoom)) $ \case
      (r,a) | hasAdapter Mapzoom r -> ZoomOut (Just $ areaId a)
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
