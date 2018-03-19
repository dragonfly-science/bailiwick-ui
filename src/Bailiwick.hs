{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Bailiwick (
    ui
) where

import Control.Monad (void)

import Reflex.Dom.Core
import Reflex.Dom.Routing.Nested 
    (runRouteWithPathInFragment, withRoute)
import Reflex.Dom.Routing.Writer (runRouteWriterT)

-- A single store object stores all data.
-- Anything not yet collected results in a fetch to the API.
-- The store provides a dynamic variable to demonstrate ready or not.

-- The application has two parts of the state:
--   1. The selected indicator, area, year, feature etc.,
--   2. The selected presentation.
-- The state is held in a single dynamic at the top level.
import Bailiwick.State (update)

-- Routes are defined by a bijection between url and the state.
import Bailiwick.Route (route)

-- Display is handled by a single function that takes the state
-- and presents the application.
import Bailiwick.View

ui  :: MonadWidget t m => m ()
ui = 
  runRouteWithPathInFragment $ fmap snd $ runRouteWriterT $ do
    void $ withRoute $ \url -> mdo
        let parsed = maybe "summary/new-zealand" id url
        state <- foldDyn update (route parsed) events
        events <- fmap switch $ (hold never =<<) $ dyn ( view <$> state)
        return ()



