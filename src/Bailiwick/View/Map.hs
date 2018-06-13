{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE TupleSections       #-}
module Bailiwick.View.Map
where

import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Fix
import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.Maybe
       (isNothing, catMaybes, isJust, fromJust)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Ordered as OM (lookup)

import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.MouseEvent (IsMouseEvent)
import GHCJS.DOM.EventM (mouseClientXY, mouseOffsetXY)
import GHCJS.DOM.HTMLElement (getOffsetTop, getOffsetLeft)

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
    :: forall m t.
       ( Monad m
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
    => Areas
    -> Dynamic t State
    -> m (Event t Message)
nzmap areas state = mdo

  let page = getPage <$> state
      zoomed = hasAdapter Mapzoom <$> state
      regD = page >>= \case
            Summary (r:_) -> return $ areaId r
            _             -> "new-zealand"
      subareaD = fmap areaId . getSubArea <$> state

      get field = ((fmap slugify . field) =<<)
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
        return $ "class" =: Text.intercalate " " classes

  (mapContainer, _) <-  elDynAttr' "div" attrD $ return ()
  let tooltipArea :: State -> Maybe (AreaInfo, (Int, Int)) -> Maybe ((AreaInfo, (Int, Int)), Area)
      tooltipArea s Nothing = Nothing
      tooltipArea s (Just (ai, xy)) =
        let maybeAreaId =
              if hasAdapter Mapzoom s
                then (areaWard ai) <|> (areaTa ai)
                else areaRegion ai
        in ((ai, xy),) <$> (((`OM.lookup` areas) . slugify) =<< maybeAreaId)
      tooltipAreaD :: Dynamic t (Maybe ((AreaInfo, (Int, Int)), Area))
      tooltipAreaD = tooltipArea <$> state <*> mouseOverFullD
      showStyle Nothing = "visibility:hidden;"
      showStyle (Just ((_, (x,y)), _)) = Text.pack $
          "visibility:visible; left:" <> show (x + 8) <> "px; top:" <> show (y + 8) <> "px;"
  elDynAttr "div" (("class" =: "tooltip" <>) . ("style" =:) . showStyle <$> tooltipAreaD) $
    el "p" $ dynText $ maybe "" (areaName . snd) <$> tooltipAreaD

  ready <- getPostBuild
  let req = xhrRequest "GET" "/assets/map.svg" def
  reqE <- performRequestAsync (req <$ ready)
  let svgE = ffor reqE _xhrResponse_responseText
  performEvent_ $ fforMaybe svgE (fmap $ DOM.setInnerHTML (_element_raw mapContainer))

  let divElement = DOM.uncheckedCastTo DOM.HTMLElement (_element_raw mapContainer)
  moveE :: Event t (Maybe (AreaInfo, (Int, Int)))
     <- wrapDomEvent divElement (`DOM.on` DOM.mouseMove) $ getAreaInfoFromSvg divElement
  clickE :: Event t (Maybe AreaInfo)
     <- fmap (fmap fst) <$> wrapDomEvent divElement (`DOM.on` DOM.click) (getAreaInfoFromSvg divElement)

  mouseOverFullD :: Dynamic t (Maybe (AreaInfo, (Int, Int))) <- holdDyn Nothing moveE
  mouseOverD :: Dynamic t (Maybe AreaInfo) <- holdUniqDyn $ fmap fst <$> mouseOverFullD

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
            subarea = if region == Just auckland then ward else ta
        in  if
            | currentRegion == region && not iszoomed
                -> Just ToggleZoom
            | currentRegion /= region && isJust region
                -> Just (SetRegion (fromJust region))
            | currentSubarea /= subarea && iszoomed && isJust subarea
                -> Just (SetSubArea $ fromJust subarea)
            | isNothing region && iszoomed
                -> Just ToggleZoom
            | otherwise
                -> Nothing

  return $ attachPromptlyDynWithMaybe makeMessages state $ fmapMaybe id clickE

data AreaInfo
  = AreaInfo
    { areaRegion  :: Maybe Text
    , areaTa      :: Maybe Text
    , areaWard    :: Maybe Text
    } deriving (Show, Eq)

getAreaInfoFromSvg
    :: DOM.IsMouseEvent ev
    => DOM.HTMLElement
    -> DOM.EventM e ev (Maybe (AreaInfo, (Int, Int)))
getAreaInfoFromSvg divElement = runMaybeT $ do
  target         <- MaybeT DOM.eventTarget
  svgelement     <- MaybeT (DOM.castTo DOM.SVGElement target)
  target_element <- DOM.getTagName svgelement
  parentg        <- MaybeT (DOM.getParentElement svgelement)
  MaybeT $ mkAreaInfo target_element parentg
  where
    mkAreaInfo
      :: IsMouseEvent ev
      => Text
      -> DOM.Element
      -> DOM.EventM e ev (Maybe (AreaInfo, (Int, Int)))
    mkAreaInfo target parent = do
      let getAttr :: Text -> DOM.EventM e ev (Maybe Text)
          getAttr = DOM.getAttribute parent
          isPath = target == ("path" :: Text)
      left <- getOffsetLeft divElement
      top <- getOffsetTop divElement
      reg <- getAttr (if isPath then "reg" else "reg1")
      ta  <- getAttr (if isPath then "ta" else "ta1")
      wrd <- getAttr (if isPath then "wrd" else "wrd1")
      (x, y) <- mouseOffsetXY
      return $ Just (AreaInfo reg ta wrd, (round left + x, round top + y))

