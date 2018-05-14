{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
module Bailiwick.View
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.List (find)
import Data.Maybe (fromMaybe)

import Language.Javascript.JSaddle.Types (MonadJSM)
import Servant.Reflex
import Reflex.Dom.Core hiding (Home)

import Bailiwick.State (State(..), Message(..), getArea, findArea, selectTa)
import Bailiwick.Store (getAreas)
import Bailiwick.Types
import Bailiwick.View.Header
import Bailiwick.View.Map

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
    => Dynamic t State -> m (Event t Message)
view state = do

  ready <- getPostBuild
  areasE <- getAreas ready
  areasD <- holdDyn [] $ fmapMaybe reqSuccess areasE

  divClass "whole-body summary-whole-body" $ do
    headerE 
      <-  divClass "main-header-area" $ do
            elClass "header" "main-header closed" $ do
              navbar
              header state areasD
    mainE <- maincontent state areasD
    indicatorsE <- indicators state
    footer
    return $ leftmost [headerE, mainE, indicatorsE]
    

navbar :: (Monad m, DomBuilder t m) => m ()
navbar = do
  elClass "nav" "content" $ do
    elAttr "a" ( "href" =: "/"
             <>"class" =: "logo font--droid-serif" ) $ do
      el "span" $ text "Regional"
      el "strong" $ text "Economic Activity"
      el "span" $ text "Web Tool"
    divClass "menu-items" $ do
      divClass "links" $ do
        elAttr "a" ( "href" =: "/"
                 <>"class" =: "home button") $ do
          el "i" $ return ()
          el "span" $ text "Home"
        elAttr "a" ("class" =: "indicators left") $ do
          el "span" $ do
            text "Indicators"
            el "i" $ return ()
      divClass "menu-buttons" $
        elClass "button" "share" $ do
          el "i" $ return ()
          el "span" $ text "share"


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
    => Dynamic t State 
    -> Dynamic t [Area]
    -> m (Event t Message)
maincontent state areasD = do
  divClass "content main-content" $ do
    divClass "central-content summary" $ do
      messages
       <- 
         divClass "navigation-map base-map" $ do
           summaryText state areasD
           divClass "svg-wrapper" $ do
             nzmap state
      divClass "area-summary" $ do
        text ""
      return messages

summaryText
  :: ( DomBuilder t m
     , PostBuild t m )
  => Dynamic t State
  -> Dynamic t [Area]
  -> m ()
summaryText state areasD = do
  let homeAttr = state >>= \case
            Home -> return ("class" =: "text-wrapper" <> "style" =: "display: block")
            _    -> return ("class" =: "text-wrapper" <> "style" =: "display: none")
      summaryAttr = state >>= \case
            Home -> return ("class" =: "text-wrapper" <> "style" =: "display: none")
            _    -> return ("class" =: "text-wrapper" <> "style" =: "display: block")

      urlArea = getArea <$> state

      urlRegion = do
        area <- urlArea
        areas <- areasD
        return $ do
          thisArea <- findArea area areas
          if areaLevel thisArea  == "reg"
            then return (areaId thisArea)
            else do 
              let isRegion p = fromMaybe False $ do
                      pa <- findArea p areas
                      return $ areaLevel pa == "reg"
              find isRegion $ areaParents thisArea

      urlTa = zipDynWith selectTa urlArea areasD
      
      dispRegion = do 
        mthis <- urlRegion
        areas <- areasD
        mta <- urlTa
        return $ fromMaybe "New Zealand" $ do
                        this <- mthis
                        thisReg <- findArea this areas
                        if mta == Nothing
                            then return $ areaName thisReg
                            else return $ areaName thisReg <> ":"


  elDynAttr "div" homeAttr $ do
    divClass "background-wrapper" $ do
      divClass "intro-paragraph" $ do
        text "Welcome to the interactive Regional Economic Activity Web Tool."
      elClass "p" "body-paragraph" $ do
        text "This tool allows you to compare regions' economic performance, distinguish their attributes and specialisations, and understand the different roles they play in the New Zealand economy."
      elClass "p" "body-paragraph" $ do
        text "Click on regions to compare, or go straight into the detail by exploring the themed indicators. All data sets are annualised in order to make comparison easier and maximise the data available."
      elClass "p" "body-paragraph" $ do
        text "The tool is updated regularly, but more recent data may be available at its source, especially if it is frequently updated. Where possible, we include a link back to the source so you can check if more recent data is available."
  elDynAttr "div" summaryAttr $ do
    divClass "background-wrapper" $ do
      divClass "intro-paragraph" $ do
        dynText dispRegion
      elClass "p" "body-paragraph" $ do
        text "Zoom in to compare different areas of West Coast."
      elClass "p" "body-paragraph" $ do
        text "You can go into more detail by exploring the indicators below"
    divClass "map-zoom" $ do
      text "Zoom in"
      elClass "span" "zoom-in-small" $ return ()


indicators
    :: ( Monad m , DomBuilder t m)
    => Dynamic t State -> m (Event t Message)
indicators _state = return never

footer :: (Monad m, DomBuilder t m) => m ()
footer = do
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
