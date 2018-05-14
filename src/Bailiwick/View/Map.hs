{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
module Bailiwick.View.Map
where

import Control.Monad.Fix
import Control.Applicative
import Data.Monoid ((<>), mempty)
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

import Bailiwick.State (State(..), Message(..))


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
nzmap stateD = mdo

  let attrD = do 
        state <- stateD
        mouseReg <- maybe mempty (("id" =:) . slugify) <$> mouseOverRegD
        return $ "class" =: "map" <> mouseReg <>  
            case state of
                Summary r -> "rid" =: r
                _         -> "rid" =: "new-zealand"
  (mapContainer, _) <-  elDynAttr' "div" attrD $ return ()

  display attrD

  ready <- getPostBuild
  let req = xhrRequest "GET" "/assets/map.svg" def
  reqE <- performRequestAsync (req <$ ready)
  let svgE = ffor reqE _xhrResponse_responseText
  performEvent_ $ fforMaybe svgE $ (fmap $ DOM.setInnerHTML (_element_raw mapContainer))

  let divElement = DOM.uncheckedCastTo DOM.HTMLElement (_element_raw mapContainer)
  moveE :: Event t (Maybe Text)
    <- wrapDomEvent divElement (`DOM.on` DOM.mouseMove) $ do
                    DOM.eventTarget >>= \case
                      Nothing -> return Nothing
                      Just t ->
                        DOM.castTo DOM.SVGElement t >>= \case
                            Nothing -> return $ Nothing
                            Just sel -> do
                              mp <- DOM.getParentElement sel   
                              case mp of
                                Nothing -> return Nothing
                                Just p -> do
                                  mreg <- DOM.getAttribute p ("reg"::Text)
                                  mreg1 <- DOM.getAttribute p ("reg1"::Text)
                                  return (mreg <|> mreg1)  


  mouseOverRegD <- holdDyn Nothing moveE

  return never


--                    
