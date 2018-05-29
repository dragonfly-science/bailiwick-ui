{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE MultiWayIf          #-}
module Bailiwick.View.Map
where

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad (join)
import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Types as DOM

import Language.Javascript.JSaddle.Types (MonadJSM)
import Reflex.Dom.Core
import Reflex.Dom.Builder.Immediate (wrapDomEvent)

import Bailiwick.State
import Bailiwick.Types

slugify :: Text -> Text
slugify = Text.replace "'" "" 
        . Text.replace " " "-"
        . Text.toLower

nzmap
    :: ( Monad m
       , MonadJSM m
       , MonadFix m
       , DomBuilder t m
       , PostBuild t m
       , TriggerEvent t m
       , HasJSContext (Performable m)
       , PerformEvent t m
       , MonadJSM (Performable m)
       , MonadHold t m
       , DomBuilderSpace m ~ GhcjsDomSpace
       )
    => Dynamic t State -> m (Event t Message)
nzmap state = mdo

  let page = getPage <$> state
      zoomed = hasAdapter Mapzoom <$> state  
      regD = page >>= \case
            Summary (r:_) -> return $ areaId r
            _             -> "new-zealand"
      subareaD = (fmap areaId . getSubArea) <$> state  

      get field = join . fmap (fmap slugify . field)
      attrD = do 
        reg <- regD
        subarea <- subareaD
        zoom <- zoomed
        let regselected = Just (reg <> "-selected")
            subareaselected = (<>"-selected") <$> subarea
        let zoomclass = if zoom then Just "zoom" else Just "not-zoom"

        mouse_over_reg <- get areaRegion <$> mouseOverD
        mouse_over_ta  <- get areaTa <$> mouseOverD
        mouse_over_wrd <- get areaWard <$> mouseOverD

        let classes = nub $ catMaybes [ Just "map"
                                      , regselected
                                      , subareaselected
                                      , mouse_over_reg
                                      , mouse_over_ta
                                      , mouse_over_wrd
                                      , zoomclass] 
        return $ "class" =: (Text.intercalate " " classes)

  (mapContainer, _) <-  elDynAttr' "div" attrD $ return ()

  ready <- getPostBuild
  let req = xhrRequest "GET" "/assets/map.svg" def
  reqE <- performRequestAsync (req <$ ready)
  let svgE = ffor reqE _xhrResponse_responseText
  performEvent_ $ fforMaybe svgE $ (fmap $ DOM.setInnerHTML (_element_raw mapContainer))

  let divElement = DOM.uncheckedCastTo DOM.HTMLElement (_element_raw mapContainer)
  moveE :: Event t (Maybe AreaInfo)
     <- wrapDomEvent divElement (`DOM.on` DOM.mouseMove) getAreaInfoFromSvg
  clickE :: Event t (Maybe AreaInfo)
     <- wrapDomEvent divElement (`DOM.on` DOM.click) getAreaInfoFromSvg

  mouseOverD :: Dynamic t (Maybe AreaInfo) <- holdDyn Nothing moveE

  -- The click event depends on the state
  let makeMessages :: State -> AreaInfo -> Maybe Message
      makeMessages st ai =
        let region = slugify <$> areaRegion ai
            ta = slugify <$> areaTa ai
            ward = slugify <$> areaWard ai
            currentRegion = areaId <$> getRegion st
            currentSubarea = areaId <$> getSubArea st
            iszoomed = hasAdapter Mapzoom st
            auckland = "auckland" :: Text
            subarea = if region == (Just auckland) then ward else ta
        in  if 
            | currentRegion == region && not iszoomed
                -> Just ToggleZoom
            | currentRegion /= region && isJust region
                -> Just (SetRegion (fromJust region))
            | currentSubarea /= subarea && iszoomed && isJust subarea 
                -> Just (SetSubArea $ fromJust subarea)
            | region == Nothing && iszoomed
                -> Just ToggleZoom
            | otherwise
                -> Nothing
                
  return $ attachPromptlyDynWithMaybe makeMessages state $ fmapMaybe id clickE

data AreaInfo
  = AreaInfo
    { areaRegion :: Maybe Text
    , areaTa     :: Maybe Text
    , areaWard   :: Maybe Text
    } deriving (Show, Eq)

getAreaInfoFromSvg
    :: DOM.IsEvent ev 
    => DOM.EventM e ev (Maybe AreaInfo)
getAreaInfoFromSvg = runMaybeT $ do
  target         <- MaybeT DOM.eventTarget
  svgelement     <- MaybeT (DOM.castTo DOM.SVGElement target)
  target_element <- DOM.getTagName svgelement
  parentg        <- MaybeT (DOM.getParentElement svgelement)
  MaybeT $ mkAreaInfo target_element parentg
  where
    mkAreaInfo :: Text -> DOM.Element -> DOM.EventM e ev (Maybe AreaInfo)
    mkAreaInfo target parent = do
      let getAttr :: Text -> DOM.EventM e ev (Maybe Text)
          getAttr a = do
             val <- DOM.getAttribute parent a
             if val == Just "null"
                then return Nothing
                else return val
          isPath = target == ("path" :: Text)
      reg <- getAttr (if isPath then "reg" else "reg1")
      ta  <- getAttr (if isPath then "ta" else "ta1")
      wrd <- getAttr (if isPath then "wrd" else "wrd1")
      return $ Just $ AreaInfo reg ta wrd
