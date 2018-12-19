{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
module Bailiwick.Store
    ( initialise
    , Store(..)
    , getChartData
    )
where

import Data.Proxy
import Data.Text

import Servant.API
import Servant.Reflex
import Reflex.Dom.Core

import Bailiwick.Types
import Bailiwick.AreaTrees


data Store
  = Store
    { getAreas          :: Areas
    , getAreaSummaries  :: AreaSummaries
    , getThemes         :: Themes
    , getIndicators     :: Indicators
    , getAreaTrees      :: AreaTrees
    , getFeatures       :: Features
    }
    deriving (Show, Eq)

initialise
  :: ( MonadHold t m
     , Reflex t
     , SupportsServantReflex t m
     , HasJSContext (Performable m)
     )
  => Event t () -> m (Dynamic t Store)
initialise ready = do
  let maybeGetList = holdDyn [] . fmapMaybe reqSuccess
  areasD <- fmap (fmap mkAreas) $ maybeGetList =<< apiGetAreas ready
  areaSummariesD <- fmap (fmap mkAreaSummaries) $ maybeGetList =<< apiGetAreaSummaries ready
  themesD <- fmap (fmap mkThemes) $ maybeGetList =<< apiGetThemes ready
  indicatorsD <- fmap (fmap mkIndicators) $ maybeGetList =<< apiGetIndicators ready
  areaTreesD <- fmap (fmap mkAreaTrees) $ maybeGetList =<< apiGetAreaTrees ready
  featuresD <- fmap (fmap mkFeatures) $ maybeGetList =<< apiGetFeatures ready
  return $
      Store <$> areasD
            <*> areaSummariesD
            <*> themesD
            <*> indicatorsD
            <*> areaTreesD
            <*> featuresD


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

  holdDyn Nothing $ fmap reqSuccess $ traceEventWith (Prelude.take 100 . show . reqFailure) $ chartDataE






type GetAreas = "data" :> "areas-11d88bc13.json" :> Get '[JSON] [Area]
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


