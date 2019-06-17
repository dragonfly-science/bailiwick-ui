module Bailiwick.Javascript
  ( clickEvents, makeJSObject )
where

import Control.Monad (forM_)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Text (Text)

import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Types as DOM

import Language.Javascript.JSaddle (MonadJSM, obj, setProp, toJSVal, JSM, JSString, Object)
import Reflex.Dom.Core

clickEvents
  :: ( TriggerEvent t m
     , MonadJSM m
     )
  => Element EventResult GhcjsDomSpace t
  -> (DOM.Element -> DOM.EventM DOM.HTMLElement DOM.MouseEvent (Maybe a))
  -> m (Event t (Maybe a))
clickEvents e handler =
  let htmlelement = DOM.uncheckedCastTo DOM.HTMLElement (_element_raw e)
      doHandler = runMaybeT $ do
        target      <- MaybeT DOM.eventTarget
        svgelement  <- MaybeT (return . Just $ DOM.uncheckedCastTo DOM.Element target)
        MaybeT $ handler svgelement
  in  wrapDomEvent htmlelement (`DOM.on` DOM.click) doHandler



makeJSObject :: [(JSString, Maybe Text)] -> JSM Object
makeJSObject assocs = do
    res <- obj
    let build (key, val) = do
            valJS <- toJSVal val
            setProp key valJS res
    forM_ assocs build
    return res
