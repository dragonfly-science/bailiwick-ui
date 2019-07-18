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
import Bailiwick.Route
import Bailiwick.Types
import Bailiwick.View.Header (header)
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

  let marginTop (ThemePage _) = bool Nothing (Just "349px") . (> 140)
      marginTop _ = bool Nothing (Just "279px") . (> 140)
      marginTopD = marginTop <$> (routePage <$> routeD) <*> scrollPosD
      wholeBodyClass route =
        "whole-body " <> case routePage route of
                              ThemePage _ -> "theme-whole-body"
                              Summary -> "summary-whole-body"
      mainHeaderClass isOpen route =
         "main-header " <> case routePage route of
                              ThemePage _ -> if isOpen then "large" else "small"
                              Summary -> "closed"
  elDynAttr "div" (("class" =:) <$>
         ((<>) <$> (wholeBodyClass <$> routeD)
               <*> (bool "" " fixed" . isJust <$> marginTopD))) $ mdo
    (headerE, isOpen)
      <- divClass "main-header-area" $
            elDynClass "header" (mainHeaderClass <$> isOpen <*> routeD) $ do
              navBarE <- navbar
              headerE' <- header (makeHeaderState st)
              (toolBarE, isOpen') <- mdo
                  eithersE <- toolBar isOpen (makeToolBarState st)
                  let (isOpenE, toolBarE) = fanEither eithersE
                  isOpen <- foldDyn (const not) False $ isOpenE
                  return (toolBarE, isOpen)
              return (leftmost [navBarE, headerE', toolBarE], isOpen')
    exportE <- exportMenu
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
      divClass "menu-buttons" $ do
        elClass "button" "download" $ do
            el "i" $ return ()
            el "span" $ text "download"
        elClass "button" "embed" $ do
            el "i" $ return ()
            el "span" $ text "embed"
        elClass "button" "share" $ do
          el "i" $ return ()
          el "span" $ text "share"
      return home
    return $ GoToHomePage <$ leftmost [domEvent Click logo, domEvent Click home]


exportMenu
    :: ( MonadFix m
        , MonadHold t m
        , PostBuild t m
        , DomBuilder t m
        )
    => m (Event t Message)
exportMenu = do
    exportE <-
        -- TODO: need to change "display:none" to "display:flex" when ready to show
        elAttr "div" ("class" =: "export-dialog" <> "style" =: "display:none;") $ do
            divClass "export-dialog-main" $ do
                divClass "export-header" $ do
                    divClass "export-menu" $ do
                        elClass "span" "export-type" $ text "Share"
                        elClass "span" "export-type" $ text "Embed"
                        elClass "span" "export-type" $ text "Download"
                    elClass "span" "export-close" $ return ()
                -- TODO: export-body requires the class of the export option
                -- selected - i.e. download-menu, share-menu, embed-menu
                divClass "export-body" $ do
                    -- SHARE MENU:
                    elAttr "div" ("class" =: "extra-wrapper" <> "style" =: "display:none") $ do
                        divClass "export-detail" $
                            divClass "share-text" $ return ()
                        divClass "export-controls" $
                            divClass "social-share" $ do
                                text "Share this on:"
                                -- TODO: Needs a share event - opens twitter share URL.
                                divClass "share-twitter" $ do
                                    el "i" $ return ()
                                    text "Twitter"
                                    return ()
                                -- TODO: Needs an event - opens modal to share in FB.
                                divClass "share-facebook" $ do
                                    el "i" $ return ()
                                    text "Facebook"
                                    return ()
                    elAttr "div" ("class" =: "extra-wrapper" <> "style" =: "display:none") $ do
                        divClass "export-detail" $
                            elAttr "input" ("readonly" =: "" <> "class" =: "share-url") $ return ()
                        divClass "export-controls" $
                            -- TODO: Needs event to copy URL
                            divClass "share-copy" $
                                text "Copy this link to clipboard"
                    -- EMBED MENU
                    elAttr "div" ("class" =: "embed-output embed-preview export-detail" <> "style" =: "display:none;") $ return ()
                        -- TODO: this needs to resize I think on window resize
                        -- events.
--                        elAttr "iframe" (
--                            "src" =: "/theme/tourism-spend/map/treemap/2018/new-zealand/northland/?embed=dynamic&amp;left-zoom=1&amp;right-transform=absolute&amp;preview=600" <>
--                            "frameborder" =: "0" <>
--                            "scrolling" =: "no" <>
--                            "marginheight" =: "0" <>
--                            "marginwidth" =: "0" <>
--                            "width" =: "600" <>
--                            "id" =: "iFrameResizer1" <>
--                            "style" =: "overflow: hidden; height: 1671px; transform: translateX(106px) scale(0.239378); visibility: inherit;"
--                        ) $ return ()
                          -- return ()
                    elAttr "div" ("class" =: "embed-controls export-controls" <> "style" =: "display:none;") $ do
                        el "div" $ do
                            divClass "embed-labels" $ text "Customise the view"
                            divClass "embed-components" $ do
                                -- TODO: emebed-component requires an event to check/uncheck the input field.
                                divClass "embed-component" $ do
                                    elAttr "input" ("type" =: "checkbox") $ return ()
                                    el "div" $ do
                                        el "i" $ return ()
                                        text "Show map"
                                divClass "embed-component" $ do
                                    elAttr "input" ("type" =: "checkbox") $ return ()
                                    el "div" $ do
                                        el "i" $ return ()
                                        text "Show chart"
                                divClass "embed-component" $ do
                                    elAttr "input" ("type" =: "checkbox") $ return ()
                                    el "div" $ do
                                        el "i" $ return ()
                                        text "Show numbers"
                            divClass "selector" $
                                divClass "selector-container" $ do
                                    -- TODO: requires a dynamic to change text based upon selection below & to add/remove
                                    -- the class "show-menu" to the ul below.
                                    elClass "p" "selector-button" $ text "Width: Medium"
                                    elClass "ul" "selector-menu selector-select" $ do
                                        -- TODO: li require events to change text above & change width of iframe.
                                        el "li" $ text "Narrow"
                                        el "li" $ text "Medium"
                                        el "li" $ text "Wide"
                        el "div" $ do
                            elAttr "textarea" (
                                    "readonly" =: "" <>
                                    "spellcheck" =: "false" <>
                                    "rows" =: "6" <>
                                    "class" =: "embed-snippet"
                                ) $ text "&lt;iframe src=\"http://webrear.mbie.govt.nz/theme/tourism-spend/map/treemap/2018/new-zealand/northland/?embed=dynamic&amp;left-zoom=1&amp;right-transform=absolute\" frameborder=\"0\" scrolling=\"no\" marginheight=\"0\" marginwidth=\"0\" width=\"600\" height=\"1671\"&gt;&lt;/iframe&gt;"
                            divClass "snippet-copy" $ text " &lt;/&gt; Copy html to clipboard"
                    -- DOWNLOAD MENU
                    elAttr "div" ("class" =: "extra-wrapper" <> "style" =: "display:none;") $ do
                        divClass "export-detail" $ do
                            divClass "download-text" $ do
                                el "p" $ do
                                    text "For the selected indicator "
                                    el "span" $ text "tourism spend"
                                    text " and the selected area "
                                    el "span" $ text "New Zealand"
                                    text " there are 4 data downloads available."
                                el "p" $ do
                                    el "i" $ text "Tourism spend"
                                    text "is all the original data and the derived values used to create visualisations for the tourism spend indicator."
                        divClass "export-controls download-controls" $ do
                            divClass "data-labels" $ text "Select the data"
                            divClass "embed-components" $ do
                                -- TODO: need events to check the input
                                divClass "embed-component" $ do
                                    elAttr "input" ("type" =: "radio" <> "value" =: "indicator") $ return ()
                                    el "div" $ do
                                        el "i" $ return ()
                                        text "Tourism Spend"
                                    el "div" $ text "813 kB"
                                divClass "embed-component" $ do
                                    elAttr "input" ("type" =: "radio" <> "value" =: "indicator") $ return ()
                                    el "div" $ do
                                        el "i" $ return ()
                                        text "New Zealand"
                                    el "div" $ text "147 kB"
                                divClass "embed-component" $ do
                                    elAttr "input" ("type" =: "radio" <> "value" =: "indicator") $ return ()
                                    el "div" $ do
                                        el "i" $ return ()
                                        text "All original data"
                                    el "div" $ text "2,371 kB"
                                divClass "embed-component" $ do
                                    elAttr "input" ("type" =: "radio" <> "value" =: "indicator") $ return ()
                                    el "div" $ do
                                        el "i" $ return ()
                                        text "All data"
                                    el "div" $ text "10,443 kB"
                    elAttr "div" ("class" =: "extra-wrapper" <> "style" =: "display:none;") $ do
                        divClass "export-detail" $ do
                            divClass "copyright" $ do
                                divClass "icons" $ do
                                    divClass "cc-cc" $ return ()
                                    divClass "cc-by" $ return ()
                                elAttr "a" (
                                    "target" =: "_blank" <>
                                    "href" =: "http://creativecommons.org/licenses/by/4.0/") $ text "Creative Commons Attribution 4.0 International Licence"
                        divClass "export-controls download-controls" $
                            -- TODO: needs event to download selected file.
                            divClass "download-button" $ text "Download file to desktop"

            return never

    return $ exportE

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
  isSummary <- holdUniqDyn ((== Summary) . routePage <$> routeD)
  let leftZoomD = hasAdapter Mapzoom <$> routeD
      rightZoomD = hasAdapter RightZoom <$> routeD
      mapState = makeMapState st
      mapLegendState = makeMapLegendState st
      areaSummaryState = makeSummaryState st
      indicatorSummaryState = makeIndicatorSummaryState st
      indicatorChartState = makeIndicatorChartState st
  switchDynM $
     ffor isSummary $ \case
        True  -> summaryContent routeD regionD areaD mapState areaSummaryState
        False -> indicatorContent leftZoomD rightZoomD regionD
                                  mapState mapLegendState
                                  indicatorChartState
                                  indicatorSummaryState
                                  (makeIndicatorTableState st)

summaryContent
    :: ContentConstraints t m
    => Dynamic t Route
    -> Dynamic t (Maybe Area)
    -> Dynamic t (Maybe Area)
    -> MapState t
    -> AreaSummaryState t
    -> m (Event t Message)
summaryContent routeD regionD areaD map_state area_summary_state=
  divClass "central-content summary" $ do
    messagesE
     <-
       divClass "navigation-map base-map" $ do
         zoomClick <- summaryText routeD regionD areaD
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
            return $ leftmost [ tagPromptlyDyn
                                    (ZoomOut . fmap areaId <$> regionD)
                                    (domEvent Click eZoomOut)
                              , ZoomIn <$ domEvent Click eZoomIn
                              ]

        mapClicks <- divClass "svg-wrapper" $ nzmap False map_state setScaleE
        setScaleE <- divClass "legend indicator-map-legend" $ mapLegend map_legend_state
        -- (mapLegendLabel indicator_chart_state)
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
  => Dynamic t Route
  -> Dynamic t (Maybe Area)
  -> Dynamic t (Maybe Area)
  -> m (Event t Message)
summaryText routeD regionD areaD = do

  let area = routeArea <$> routeD
      homeAttr = ffor area $ \case
            "new-zealand" -> ("class" =: "text-wrapper" <> "style" =: "display: block")
            _             -> ("class" =: "text-wrapper" <> "style" =: "display: none")
      summaryAttr = ffor area $ \case
            "new-zealand" -> ("class" =: "text-wrapper" <> "style" =: "display: none")
            _             -> ("class" =: "text-wrapper" <> "style" =: "display: block")

      -- Zoom in and out button
      zoomed = hasAdapter Mapzoom <$> routeD
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
    return $ ffor (tagPromptlyDyn ((,) <$> routeD <*> regionD) (domEvent Click zoom)) $ \case
      (r,a) | hasAdapter Mapzoom r -> ZoomOut (areaId <$> a)
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
