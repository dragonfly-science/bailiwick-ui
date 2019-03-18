{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
module Bailiwick (
    ui
) where

import Control.Monad.Fix

import Reflex.Dom.Core

import qualified Bailiwick.Store as Store
import qualified Bailiwick.State as State
import Bailiwick.View (view)

ui  :: ( Monad m
       , MonadFix m
       , MonadWidget t m
      )  => m ()
ui = mdo
  stateD <- State.runState messagesE
  storeD <- Store.runStore asksE
  (messagesE, asksE) <- fanEither <$> view storeD stateD
  return ()

