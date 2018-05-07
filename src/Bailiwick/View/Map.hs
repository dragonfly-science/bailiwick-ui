{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
module Bailiwick.View.Map
where

import Control.Monad.IO.Class (MonadIO)
import Data.Monoid ((<>))

import qualified GHCJS.DOM.Element as DOM
import Language.Javascript.JSaddle.Types (MonadJSM)
import Reflex.Dom.Core

import Bailiwick.State (State(..), Message(..))


nzmap
    :: ( Monad m
       , DomBuilder t m
       , PostBuild t m
       , TriggerEvent t m
       , HasJSContext (Performable m)
       , PerformEvent t m
       , MonadJSM (Performable m)
       , MonadIO m
       , DOM.IsElement (RawElement (DomBuilderSpace m))
       , MonadHold t m
       )
    => Dynamic t State -> m (Event t Message)
nzmap _state = do
  let attrD = constDyn ("class" =: "map" <> "id" =: "auckland" )
  (mapContainer, _) <-  elDynAttr' "div" attrD $ return ()

  ready <- getPostBuild
  let req = xhrRequest "GET" "/assets/map.svg" def
  --svgE :: Event t (Maybe String) <- getAndDecode ("/assets/map.svg" <$ ready)
  reqE :: Event t XhrResponse <- performRequestAsync (req <$ ready)
  let svgE = ffor reqE _xhrResponse_responseText
                
  performEvent_ $ fforMaybe svgE $ (fmap $ DOM.setInnerHTML (_element_raw mapContainer))

--  clickE <- domEvent Click el  

  return never
