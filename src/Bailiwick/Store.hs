{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE RecordWildCards         #-}
module Bailiwick.Store
  ( Store(..)
  , run
  )
where

import Control.Monad ((>=>))
import Control.Monad.Fix (MonadFix)
import Data.Proxy
import Data.Text hiding (empty, foldr1)

import Servant.API
import Servant.Reflex
import Reflex.Dom.Core
import Language.Javascript.JSaddle.Types (MonadJSM)

import Bailiwick.Types
import Bailiwick.Route (Message(..))
import Bailiwick.AreaTrees

data Store t
  = Store 
    { storeAreasD         :: Dynamic t (Maybe Areas)
    , storeThemesD        :: Dynamic t (Maybe [Theme])
    , storeSummariesD     :: Dynamic t (Maybe AreaSummaries)
    , storeSummaryNumbers :: SummaryNumbers
    }

empty :: Reflex t => Store t
empty
  = Store
    { storeAreasD          = constDyn Nothing
    , storeThemesD         = constDyn Nothing
    , storeSummariesD      = constDyn Nothing
    , storeSummaryNumbers  = emptySummaryNumbers
    }

run
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadFix m
     , MonadHold t m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     )
  => Event t Message
  -> m (Store t)
run messagesE =
  let watchers
       = [ initialData messagesE
         , summaryNumbers messagesE
         ]

  in foldr1 (>=>) watchers empty

initialData
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     )
  => Event t Message
  -> Store t
  -> m (Store t)
initialData messagesE store = do
  let triggerE = () <$ ffilter (== Ready) messagesE
      runApi msg eveE
        = let tracedEventE = traceEventWith (showReqResult msg) eveE
          in  fmap reqSuccess tracedEventE

  areasE <- apiGetAreas triggerE
  themesE <- apiGetThemes triggerE
  summariesE <- apiGetAreaSummaries triggerE

  areasD <- holdDyn Nothing $ runApi "getAreas" areasE
  themesD <- holdDyn Nothing $ runApi "getThemes" themesE
  summariesD <- holdDyn Nothing $ runApi "getAreaSummaries" summariesE

  return $
    store 
      { storeAreasD     = areasD
      , storeThemesD    = themesD
      , storeSummariesD = summariesD
      }

summaryNumbers
  :: ( TriggerEvent t m
     , PerformEvent t m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     )
  => Event t Message
  -> Store t
  -> m (Store t)
summaryNumbers messageE store@Store{..} = do
  return store

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



type GetAreas = "db" :> "dev" :> "areas.json" :> Get '[JSON] Areas
type GetThemes = "db" :> "dev" :> "themes.json" :> Get '[JSON] [Theme]
type GetAreaSummaries = "db" :> "dev" :> "area-summaries.json" :> Get '[JSON] AreaSummaries
type GetAreaTrees = "data" :> "areaTrees-11d88bc13.json" :> Get '[JSON] [AreaTree]
type GetFeatures = "data" :> "features-11d88bc13.json" :> Get '[JSON] [Feature]

type GetMapSummaries = "data" :> Capture "filename" Text :> Get '[JSON] MapSummary
type GetChartData = "chartdata" :> Capture "filename" Text :> Get '[JSON] ChartData
type GetIndicatorSummaryNumbers = "db" :> "dev" :> Capture "<indicator>.json" Text :> Get '[JSON] IndicatorSummary

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


apiGetIndicatorSummaryNumbers
    :: forall t m . SupportsServantReflex t m => Client t m GetIndicatorSummaryNumbers ()
apiGetIndicatorSummaryNumbers
    = client (Proxy :: Proxy GetIndicatorSummaryNumbers) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))


showReqResult :: String -> ReqResult t a -> String
showReqResult apiPrefix rr = (apiPrefix ++) . unpack $
    case rr of
        ResponseSuccess _ _ _   -> "Success"
        ResponseFailure _ msg _ -> "Response failure: " <> msg
        RequestFailure _ msg    -> "Request failure: " <> msg

