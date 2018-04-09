{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
module Bailiwick.View
where

import Control.Monad.Fix
import Data.Monoid ((<>))

import Servant.Reflex
import Reflex.Dom.Core

import Bailiwick.State (State(..), Message(..))
import Bailiwick.View.Header

view 
    :: ( Monad m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       , SupportsServantReflex t m
       )
    => Dynamic t State -> m (Event t Message)
view state = do
  divClass "whole-body summary-whole-body" $ do
    headerE 
      <-  divClass "main-header-area" $ do
            elClass "header" "main-header closed" $ do
              navbar
              header state
    mainE <- maincontent state
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
    :: ( Monad m , DomBuilder t m)
    => Dynamic t State -> m (Event t Message)
maincontent _state = do
  divClass "content main-content" $ do
    divClass "central-content summary" $ do
      elAttr "div" ("style" =: "height: 550px") $ text ""
  return never

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
