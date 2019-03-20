{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
module Bailiwick.Store
where

import Control.Monad.Fix (MonadFix)
import Data.Proxy
import Data.Text hiding (empty)

import Servant.API
import Servant.Reflex
import Reflex.Dom.Core
import Language.Javascript.JSaddle.Types (MonadJSM)

import Bailiwick.Types
import Bailiwick.Route (Message(..))
import Bailiwick.AreaTrees

data Store
  = Empty
  | LoadAreas
    { areas :: Areas
    }
    deriving (Show, Eq)

empty :: Store
empty = Empty

run
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadFix m
     , MonadHold t m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     )
  => Event t Message
  -> m (Dynamic t Store)
run messagesE = do

  -- Create API request, and capture the response
  responseE <- makeRequest messagesE

  -- Pull them back together to create the dynamic store
  foldDyn ($) empty responseE


makeRequest
  :: ( TriggerEvent t m
     , PerformEvent t m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     )
  => Event t Message
  -> m (Event t (Store -> Store))
makeRequest messageE = do
  areasE <- apiGetAreas (() <$ ffilter (==Ready) messageE)
  return $ fmap (flip $ const LoadAreas) $ fmapMaybe reqSuccess areasE








getChartData
  :: ( MonadHold t m
     , Reflex t
     , SupportsServantReflex t m
     , HasJSContext (Performable m)
     )
  =>  Dynamic t Text -> m (Dynamic t (Maybe ChartData))
getChartData filenameD = do

--   updated filenameD ...
--       lookInMemory ... <|>
--       lookInLocalStorate ... <|>
--       lookOnDisk ... <|>
--       fetchUsing servant <|>

  chartDataE <- apiGetChartData (Right <$> filenameD) (() <$ updated filenameD)

  holdDyn Nothing $ fmap reqSuccess chartDataE



type GetAreas = "data" :> "areas-11d88bc13.json" :> Get '[JSON] Areas
type GetAreaSummaries = "data" :> "areaSummaries-11d88bc13.json" :> Get '[JSON] [AreaSummary]
type GetThemes = "data" :> "themes-11d88bc13.json" :> Get '[JSON] [Theme]
type GetIndicators = "data" :> "indicators-11d88bc13.json" :> Get '[JSON] [Indicator]
type GetAreaTrees = "data" :> "areaTrees-11d88bc13.json" :> Get '[JSON] [AreaTree]
type GetFeatures = "data" :> "features-11d88bc13.json" :> Get '[JSON] [Feature]

type GetMapSummaries = "data" :> Capture "filename" Text :> Get '[JSON] MapSummary
type GetChartData = "chartdata" :> Capture "filename" Text :> Get '[JSON] ChartData

apiGetAreas
    :: forall t m . SupportsServantReflex t m => Client t m GetAreas ()
apiGetAreas
    = client (Proxy :: Proxy GetAreas) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

apiGetAreaSummaries
    :: forall t m . SupportsServantReflex t m => Client t m GetAreaSummaries ()
apiGetAreaSummaries
    = client (Proxy :: Proxy GetAreaSummaries) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

_apiGetMapSummaries
    :: forall t m . SupportsServantReflex t m => Client t m GetMapSummaries ()
_apiGetMapSummaries
    = client (Proxy :: Proxy GetMapSummaries) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

apiGetThemes
    :: forall t m . SupportsServantReflex t m => Client t m GetThemes ()
apiGetThemes
    = client (Proxy :: Proxy GetThemes) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

apiGetIndicators
    :: forall t m . SupportsServantReflex t m => Client t m GetIndicators ()
apiGetIndicators
    = client (Proxy :: Proxy GetIndicators) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

apiGetAreaTrees
    :: forall t m . SupportsServantReflex t m => Client t m GetAreaTrees ()
apiGetAreaTrees
    = client (Proxy :: Proxy GetAreaTrees) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

apiGetFeatures
    :: forall t m . SupportsServantReflex t m => Client t m GetFeatures ()
apiGetFeatures
    = client (Proxy :: Proxy GetFeatures) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

apiGetChartData
    :: forall t m . SupportsServantReflex t m => Client t m GetChartData ()
apiGetChartData
    = client (Proxy :: Proxy GetChartData) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/data"))


