{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Bailiwick.Javascript
  ( clickEvents
  , makeJSObject
  , elDynHtmlAttr'
  , switchDynM
  , toJSValDynHold
  )
where

import Control.Monad.Fix (MonadFix)
import Control.Monad (forM_)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Text (Text)
import Data.Map (Map)

import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Types as DOM

import Language.Javascript.JSaddle hiding (val)
import Reflex.Dom.Core hiding (elDynHtmlAttr')

import Bailiwick.Types (Loadable(..))

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


toJSValDynHold
  :: ( Eq val
     , Show val
     , ToJSVal val
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , MonadJSM m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t (Loadable val)
  -> m (Dynamic t (Loadable JSVal))
toJSValDynHold valD = do
  valDU <- holdUniqDyn valD
  valE <- performEvent $ ffor (updated valDU) $ \case
              Loading -> return Loading
              Missing -> return Missing
              Loaded x -> do
                val <- liftJSM $ toJSVal x
                return (Loaded val)
  holdDyn Loading valE


makeJSObject :: [(JSString, Maybe Text)] -> JSM Object
makeJSObject assocs = do
    res <- obj
    let build (key, val) = do
            valJS <- toJSVal val
            setProp key valJS res
    forM_ assocs build
    return res

elDynHtmlAttr'
  :: ( Monad m
     , DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => Text
  -> Dynamic t (Map Text Text)
  -> Dynamic t Text
  -> m (Element EventResult (DomBuilderSpace m) t)
elDynHtmlAttr' elementTag attrs html = do
  (e, _) <- elDynAttr' elementTag attrs $ return ()
  postBuild <- getPostBuild
  performEvent_ $ liftJSM . DOM.setInnerHTML (_element_raw e) <$> leftmost [updated html, tag (current html) postBuild]
  return e

switchDynM
 :: (MonadHold t m, DomBuilder t m, PostBuild t m)
 => Dynamic t (m (Event t a)) -> m (Event t a)
switchDynM = (switchHold never =<<) . dyn


