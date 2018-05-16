{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
module Bailiwick.View.Map
where


import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad (mzero)
import Control.Applicative ((<|>))
import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
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

      attrD = do 
        reg <- regD
        zoom <- zoomed
        let zoomregclass = if zoom then Just (reg <> "-zoom") else Nothing
        let zoomclass = if zoom then Just "zoom" else Nothing
        mouse_over_reg <- (fmap slugify) <$> mouseOverRegD
        let classes = nub $ catMaybes [ Just "map", Just reg, mouse_over_reg
                                      , zoomregclass, zoomclass] 
        return $ "class" =: (Text.intercalate " " classes)

  (mapContainer, _) <-  elDynAttr' "div" attrD $ return ()

  ready <- getPostBuild
  let req = xhrRequest "GET" "/assets/map.svg" def
  reqE <- performRequestAsync (req <$ ready)
  let svgE = ffor reqE _xhrResponse_responseText
  performEvent_ $ fforMaybe svgE $ (fmap $ DOM.setInnerHTML (_element_raw mapContainer))

  let divElement = DOM.uncheckedCastTo DOM.HTMLElement (_element_raw mapContainer)
  moveE :: Event t (Maybe Text) 
    <- wrapDomEvent divElement (`DOM.on` DOM.mouseMove) getRegionFromSvg

  clickE :: Event t (Maybe Text) 
    <- wrapDomEvent divElement (`DOM.on` DOM.click) getRegionFromSvg

  mouseOverRegD <- holdDyn Nothing moveE
  return $ fmapCheap (SetRegion . maybe "new-zealand" slugify) clickE


getRegionFromSvg :: DOM.IsEvent ev => DOM.EventM e ev (Maybe Text)
getRegionFromSvg = runMaybeT $ do
  target     <- MaybeT DOM.eventTarget
  svgelement <- MaybeT (DOM.castTo DOM.SVGElement target)
  parentg    <- MaybeT (DOM.getParentElement svgelement)
  let getAttr a = do
         val <- MaybeT (DOM.getAttribute parentg a)
         if val == "null"
            then mzero
            else return val
  getAttr ("reg" :: Text) <|> getAttr "reg1"
