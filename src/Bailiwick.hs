{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TupleSections       #-}
module Bailiwick (
    ui
) where

import Reflex.Dom.Core
import Reflex.Dom.Routing.Nested
       (runRouteWithPathInFragment)
import Reflex.Dom.Routing.Writer (runRouteWriterT)


ui  :: forall t m. 
       ( MonadWidget t m )
    => m ()
ui =
  runRouteWithPathInFragment $ fmap snd $ runRouteWriterT $ mdo

    el "h1" (text "hello")

    return ()


