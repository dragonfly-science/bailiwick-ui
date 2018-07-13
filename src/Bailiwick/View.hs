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
import Data.Monoid ((<>))

import Language.Javascript.JSaddle.Types (MonadJSM)
import qualified GHCJS.DOM.GlobalEventHandlers as Events (scroll)
import GHCJS.DOM (currentDocumentUnchecked, currentWindowUnchecked)
import GHCJS.DOM.Document (getBodyUnchecked)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Element (getScrollTop)
import GHCJS.DOM.Window (getPageYOffset)

import Servant.Reflex
import Reflex.Dom.Core hiding (Home)

import Bailiwick.State
import Bailiwick.Types
import Bailiwick.View.Header
import Bailiwick.View.Map
import Bailiwick.View.AreaSummary (areaSummary)
import Bailiwick.View.Indicators (indicators)
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
    => Areas -> AreaSummaries -> Themes -> Indicators -> Dynamic t State -> m (Event t Message)
view areas areaSummaries themes inds state = do
  isScrolledD <- fmap (>137) <$> windowScrollDyn
  let wholeBodyClass s = "whole-body " <> case getPage s of
                              ThemePage _ -> "theme-whole-body"
                              Summary -> "summary-whole-body"
      mainHeaderClass s isOpen =
         "main-header " <> case getPage s of
                              ThemePage _ -> if isOpen then "large" else "small"
                              Summary -> "closed"
  elDynAttr "div" (("class" =:) <$> ((<>) <$> (wholeBodyClass <$> state) <*> (bool "" " fixed" <$> isScrolledD)))  $ mdo
    (headerE, isOpen)
      <- divClass "main-header-area" $
            elDynClass "header" (mainHeaderClass <$> state <*> isOpen) $ do
              navBarE <- navbar
              headerE <- header areas state
              (toolBarE, isOpen) <- toolBar areas state
              return (leftmost [navBarE, headerE, toolBarE], isOpen)
    mainE <-
      elDynAttr "div" (("class" =: "content main-content" <>) . bool mempty ("style" =: "margin-top: 279px") <$> isScrolledD) $
        maincontent areas areaSummaries inds state
    indicatorsE <- indicators themes inds state
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


maincontent
    :: ( Monad m
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
    => Areas
    -> AreaSummaries
    -> Indicators
    -> Dynamic t State
    -> m (Event t Message)
maincontent areas areaSummaries indicators state =
    divClass "central-content summary" $ do
      messages
       <-
         divClass "navigation-map base-map" $ do
           zoomClick <- summaryText state
           mapClicks <-
             divClass "svg-wrapper" $
               nzmap areas state
           return $ leftmost [zoomClick, mapClicks]
      summaryMessages <- divClass "area-summary" $
        areaSummary areas areaSummaries indicators state
      return $ leftmost [messages, summaryMessages]

summaryText
  :: ( DomBuilder t m
     , PostBuild t m )
  => Dynamic t State
  -> m (Event t Message)
summaryText state = do

  let homeAttr = state >>= \case
            State Summary [] _ -> return ("class" =: "text-wrapper" <> "style" =: "display: block")
            _    -> return ("class" =: "text-wrapper" <> "style" =: "display: none")
      summaryAttr = state >>= \case
            State Summary [] _ -> return ("class" =: "text-wrapper" <> "style" =: "display: none")
            _    -> return ("class" =: "text-wrapper" <> "style" =: "display: block")

      -- Zoom in and out button
      zoomed = hasAdapter Mapzoom <$> state
      zoomAttr = zoomed >>= \case
                    True -> return ( "class" =: "zoom-out-small")
                    False -> return ( "class" =: "zoom-in-small")
      zoomText = zoomed >>= \case
                    True -> return "Zoom out"
                    False -> return "Zoom in"

      dispArea = maybe "" areaName . getArea <$> state

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
    return $ ffor (tagPromptlyDyn state (domEvent Click zoom)) $ \case
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
