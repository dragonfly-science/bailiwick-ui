{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
module Bailiwick.Store
    ( getAreas
    , getAreaSummaries
    , getThemes
    , getIndicators
    , getAreaTrees
    , getFeatures
    )
where

import Data.Proxy
import Data.Text

import Servant.API
import Servant.Reflex
import Reflex.Dom.Core

import Bailiwick.Types
import Bailiwick.AreaTrees


type GetAreas = "data" :> "areas-11d88bc13.json" :> Get '[JSON] [Area]
type GetAreaSummaries = "data" :> "areaSummaries-11d88bc13.json" :> Get '[JSON] [AreaSummary]
type GetThemes = "data" :> "themes-11d88bc13.json" :> Get '[JSON] [Theme]
type GetIndicators = "data" :> "indicators-11d88bc13.json" :> Get '[JSON] [Indicator]
type GetAreaTrees = "data" :> "areaTrees-11d88bc13.json" :> Get '[JSON] [AreaTree]
type GetFeatures = "data" :> "features-11d88bc13.json" :> Get '[JSON] [Feature]
type GetMapSummaries = "data" :> Capture "filename" Text :> Get '[JSON] MapSummary

getAreas
    :: forall t m . SupportsServantReflex t m => Client t m GetAreas ()
getAreas
    = client (Proxy :: Proxy GetAreas) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

getAreaSummaries
    :: forall t m . SupportsServantReflex t m => Client t m GetAreaSummaries ()
getAreaSummaries
    = client (Proxy :: Proxy GetAreaSummaries) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))
        
getMapSummaries
    :: forall t m . SupportsServantReflex t m => Client t m GetMapSummaries ()
getMapSummaries
    = client (Proxy :: Proxy GetMapSummaries) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

getThemes
    :: forall t m . SupportsServantReflex t m => Client t m GetThemes ()
getThemes
    = client (Proxy :: Proxy GetThemes) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

getIndicators
    :: forall t m . SupportsServantReflex t m => Client t m GetIndicators ()
getIndicators
    = client (Proxy :: Proxy GetIndicators) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

getAreaTrees
    :: forall t m . SupportsServantReflex t m => Client t m GetAreaTrees ()
getAreaTrees
    = client (Proxy :: Proxy GetAreaTrees) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

getFeatures
    :: forall t m . SupportsServantReflex t m => Client t m GetFeatures ()
getFeatures
    = client (Proxy :: Proxy GetFeatures) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))


