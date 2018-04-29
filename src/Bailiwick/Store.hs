{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
module Bailiwick.Store
    ( getAreas
    )
where

import Data.Proxy

import Servant.API
import Servant.Reflex
import Reflex.Dom.Core

import Bailiwick.Types


type GetAreas = "data" :> "areas-1b7549470.json" :> Get '[JSON] [Area]



getAreas
    :: forall t m . SupportsServantReflex t m => Client t m GetAreas ()
getAreas
    = client (Proxy :: Proxy GetAreas) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))


