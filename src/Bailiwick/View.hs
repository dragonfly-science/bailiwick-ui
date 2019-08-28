{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Bailiwick.View
where

import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<|>))
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

import Bailiwick.Javascript (switchDynM)
import Bailiwick.State
import Bailiwick.Route hiding (isSummary)
import Bailiwick.Types
import Bailiwick.View.Header (header)
import Bailiwick.View.ExportMenu (exportMenu)
import Bailiwick.View.Map
import Bailiwick.View.MapLegend
import Bailiwick.View.AreaSummary (areaSummary, AreaSummaryState)
import Bailiwick.View.Indicators (indicators)
import Bailiwick.View.IndicatorChart
import Bailiwick.View.IndicatorSummary
import Bailiwick.View.IndicatorTable
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
    => State t -> m (Event t Message)
view st@State{..} = do
  scrollPosD <- windowScrollDyn
  divClass "hide colour-palette" $ do
    divClass "background-rear-dark-gray colour" $ return ()
    divClass "background-rear-medium-gray colour" $ return ()
    divClass "background-rear-mid-gray colour" $ return ()
    divClass "background-rear-light-gray colour" $ return ()
    divClass "background-rear-white colour" $ return ()
    divClass "background-rear-black colour" $ return ()
    divClass "background-rear-background colour" $ return ()
    divClass "background-rear-main-highlight colour" $ return ()
    divClass "background-rear-alternative-highlight colour" $ return ()
    divClass "background-rear-secondary-highlight colour" $ return ()
    divClass "background-rear-inactive colour" $ return ()
    divClass "background-rear-focus colour" $ return ()
    divClass "background-rear-negative colour" $ return ()
    divClass "background-rear-negative-light colour" $ return ()
    divClass "background-rear-positive colour" $ return ()
    divClass "background-rear-positive-light colour" $ return ()
    divClass "background-rear-compare-panel-background colour" $ return ()
    divClass "background-rear-chart-color-1 colour" $ return ()
    divClass "background-rear-chart-color-2 colour" $ return ()
    divClass "background-rear-chart-color-3 colour" $ return ()
    divClass "background-rear-dark-map-main-bg colour" $ return ()
    divClass "background-rear-dark-map-hover-bg colour" $ return ()

  let marginTop False = bool Nothing (Just "349px") . (> 140)
      marginTop True = bool Nothing (Just "279px") . (> 140)
      marginTopD = marginTop <$> isSummaryD <*> scrollPosD
      wholeBodyClass isSummary =
        "whole-body " <> if isSummary
                           then "summary-whole-body"
                           else "theme-whole-body"
      mainHeaderClass isOpen isSummary =
         "main-header " <> if isSummary
                              then "closed"
                              else if isOpen then "large" else "small"
  elDynAttr "div" (("class" =:) <$>
         ((<>) <$> (wholeBodyClass <$> isSummaryD)
               <*> (bool "" " fixed" . isJust <$> marginTopD))) $ mdo
    (headerE, isOpen, modalE)
      <- divClass "main-header-area" $
            elDynClass "header" (mainHeaderClass <$> isOpen <*> isSummaryD) $ do
              (navBarE, modalE') <- navbar
              headerE' <- header (makeHeaderState st)
              (toolBarE, isOpen') <- mdo
                  eithersE <- toolBar isOpen (makeToolBarState st)
                  let (isOpenE, toolBarE) = fanEither eithersE
                  isOpen <- foldDyn (const not) False $ isOpenE
                  return (toolBarE, isOpen)
              return (leftmost [navBarE, headerE', toolBarE], isOpen', modalE')
    exportE <- exportMenu modalE
    mainE <-
      elDynAttr "div" (("class" =: "content main-content" <>) .
             maybe mempty (("style" =:) . ("margin-top: " <>)) <$> marginTopD) $
        mainContent st
    indicatorsE <- indicators (makeIndicatorState st)

    -- We need to scroll up when these links are clicked (or you can't tell they do anything)
    performEvent_ $ ffor indicatorsE $ \case
      GoTo _ -> do
        window <- currentWindowUnchecked
        scrollTo window 0 185
      _ -> return ()

    footer
    return $ leftmost [headerE, exportE, mainE, indicatorsE]


navbar
  :: ( Monad m
     , DomBuilder t m
     )
  => m (Event t Message, Event t Modal)
navbar = do
  let clickE = fmap (domEvent Click . fst)
  elClass "nav" "content" $ do
    (logo, _) <- elAttr' "a" ("class" =: "logo font--droid-serif") $ do
      el "span" $ text "Regional"
      el "strong" $ text "Economic Activity"
      el "span" $ text "Web Tool"

    (home, modalE) <-
      divClass "menu-items" $ do
        home' <- divClass "links" $ do
          (home, _) <- elAttr' "a" ("class" =: "home button") $ do
            el "i" $ return ()
            el "span" $ text "Home"
          elAttr "a" ("class" =: "indicators left" <> "href" =: "#indicators") $
            el "span" $ do
              text "Indicators"
              el "i" $ return ()
          return home
        modalE' <-
          divClass "menu-buttons" $ do
--            openDownloadE <- clickE $
--              elClass' "button" "download" $ do
--                el "i" $ return ()
--                el "span" $ text "Download"
--            openEmbedE <- clickE $
--              elClass' "button" "embed" $ do
--                el "i" $ return ()
--                el "span" $ text "Embed"
            openShareE <- clickE $
              elClass' "button" "share" $ do
                el "i" $ return ()
                el "span" $ text "Share"
            return $ leftmost [ Share <$ openShareE
--                              , Download <$ openDownloadE
--                              , Embed <$ openEmbedE
                              ]
        return (home', modalE')
    let gohomeE = GoToHomePage <$ leftmost [domEvent Click logo, domEvent Click home]
    return (gohomeE, modalE)


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
    => State t
    -> m (Event t Message)
mainContent st@State{..} = do
  let leftZoomD = hasAdapter Mapzoom <$> adaptersD
      rightZoomD = hasAdapter RightZoom <$> adaptersD
      mapState = makeMapState st
      mapLegendState = makeMapLegendState st
      areaSummaryState = makeSummaryState st
      indicatorSummaryState = makeIndicatorSummaryState st
      indicatorChartState = makeIndicatorChartState st
  switchDynM $
     ffor isSummaryD $ \case
        True  -> summaryContent adaptersD regionD areaD mapState areaSummaryState
        False -> indicatorContent leftZoomD rightZoomD regionD
                                  mapState mapLegendState
                                  indicatorChartState
                                  indicatorSummaryState
                                  (makeIndicatorTableState st)

summaryContent
    :: ContentConstraints t m
    => Dynamic t [Adapter]
    -> Dynamic t (Maybe Area)
    -> Dynamic t (Maybe Area)
    -> MapState t
    -> AreaSummaryState t
    -> m (Event t Message)
summaryContent adaptersD regionD areaD map_state area_summary_state=
  divClass "central-content summary" $ do
    messagesE
     <-
       divClass "navigation-map base-map" $ do
         zoomClick <- summaryText adaptersD regionD areaD
         mapClicks <- divClass "svg-wrapper" $ nzmap True map_state never
         return $ leftmost [zoomClick, mapClicks]

    summaryMessagesE <- divClass "area-summary" $
        areaSummary area_summary_state
    return $ leftmost [messagesE, summaryMessagesE]

indicatorContent
    :: ContentConstraints t m
    => Dynamic t Bool
    -> Dynamic t Bool
    -> Dynamic t (Maybe Area)
    -> MapState t
    -> MapLegendState t
    -> IndicatorChartState t
    -> IndicatorSummaryState t
    -> IndicatorTableState t
    -> m (Event t Message)
indicatorContent leftZoomD rightZoomD regionD
    map_state
    map_legend_state
    indicator_chart_state
    indicator_summary_state
    indicator_table_state = do
  contentE <- divClass "central-content indicator" $ do
    mapE <- divClass "indicator-map base-map" $
      divClass "map-wrapper" $ mdo
        zoomClick <- divClass "map-options" $ do
          divClass "zoom-controls map-zoom active" $ do
            let inpAttrD switchD = ffor switchD $ \case
                    True  -> ("type" =: "checkbox" <> "class" =: "checked")
                    False -> ("type" =: "checkbox")
            (eZoomIn, _) <-
              el "label" $ do
                elDynAttr "input" (inpAttrD leftZoomD) $
                   return ()
                elClass' "span" "zoom-in" $
                   return ()
            (eZoomOut, _) <-
              el "label" $ do
                elDynAttr "input" (inpAttrD (not <$> leftZoomD)) $
                   return ()
                elClass' "span" "zoom-out" $
                   return ()
            return $ leftmost [ tag (current (ZoomOut . fmap areaId <$> regionD))
                                    (domEvent Click eZoomOut)
                              , ZoomIn <$ domEvent Click eZoomIn
                              ]

        mapClicks <- divClass "svg-wrapper" $ nzmap False map_state setScaleE
        setScaleE <- divClass "legend indicator-map-legend" $ mapLegend map_legend_state
        return $ leftmost [zoomClick, mapClicks]
    chartE <- divClass "indicator-chart" $
      indicatorChart indicator_chart_state rightZoomD
    return $ leftmost [ mapE, chartE ]
  summaryE <- divClass "indicator-summary hide-table no-compare" $ do
    indicatorSummary indicator_summary_state
    indicatorTable indicator_table_state
  return $ leftmost [contentE, summaryE]

summaryText
  :: ( DomBuilder t m
     , PostBuild t m )
  => Dynamic t [Adapter]
  -> Dynamic t (Maybe Area)
  -> Dynamic t (Maybe Area)
  -> m (Event t Message)
summaryText adaptersD regionD areaD = do

  let area = fmap areaId <$> zipDynWith (<|>) areaD regionD
      homeAttr = ffor area $ \case
            Just "new-zealand" -> ("class" =: "text-wrapper" <> "style" =: "display: block")
            _                  -> ("class" =: "text-wrapper" <> "style" =: "display: none")
      summaryAttr = ffor area $ \case
            Just "new-zealand" -> ("class" =: "text-wrapper" <> "style" =: "display: none")
            _                  -> ("class" =: "text-wrapper" <> "style" =: "display: block")

      -- Zoom in and out button
      zoomed = hasAdapter Mapzoom <$> adaptersD
      zoomAttr = ffor zoomed $ \case
                    True -> ( "class" =: "zoom-out-small")
                    False -> ( "class" =: "zoom-in-small")
      zoomText = ffor zoomed $ \case
                    True -> "Zoom out"
                    False -> "Zoom in"

      dispArea = maybe "" areaName <$> (zipDynWith (<|>) areaD regionD)

  elDynAttr "div" homeAttr $
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
    return $ ffor (tag ((,) <$> (current adaptersD) <*> (current regionD))
                       (domEvent Click zoom)) $ \case
      (as,a) | hasAdapter Mapzoom as -> ZoomOut (areaId <$> a)
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
