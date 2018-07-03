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
    )
where

import Data.Proxy

import Servant.API
import Servant.Reflex
import Reflex.Dom.Core

import Bailiwick.Types


type GetAreas = "data" :> "areas-1b7549470.json" :> Get '[JSON] [Area]
type GetAreaSummaries = "data" :> "areaSummaries-1b7549470.json" :> Get '[JSON] [AreaSummary]
type GetThemes = "data" :> "themes-dev.json" :> Get '[JSON] [Theme]
type GetIndicators = "data" :> "indicators-dev.json" :> Get '[JSON] [Indicator]



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


