{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE TupleSections           #-}
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
import qualified Data.HashMap.Strict.InsOrd as OM

import Bailiwick.Types
import Bailiwick.Route
import Bailiwick.AreaTrees

data Store t
  = Store 
    { storeAreasD          :: Dynamic t (Maybe Areas)
    , storeThemesD         :: Dynamic t (Maybe [Theme])
    , storeSummariesD      :: Dynamic t (Maybe AreaSummaries)
    , storeSummaryNumbersD :: Dynamic t SummaryNumbers
    }

empty :: Reflex t => Store t
empty
  = Store
    { storeAreasD          = constDyn Nothing
    , storeThemesD         = constDyn Nothing
    , storeSummariesD      = constDyn Nothing
    , storeSummaryNumbersD = constDyn emptySummaryNumbers
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

  areasE     <- apiGetAreas triggerE
  themesE    <- apiGetThemes triggerE
  summariesE <- apiGetAreaSummaries triggerE

  areasD     <- holdDyn Nothing $ catchApi "getAreas" areasE
  themesD    <- holdDyn Nothing $ catchApi "getThemes" themesE
  summariesD <- holdDyn Nothing $ catchApi "getAreaSummaries" summariesE

  return $
    store 
      { storeAreasD     = areasD
      , storeThemesD    = themesD
      , storeSummariesD = summariesD
      }

summaryNumbers
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , MonadFix m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     )
  => Event t Message
  -> Store t
  -> m (Store t)
summaryNumbers messageE store@Store{..} = do

  let test numbers message =
        case getIndicatorId message of
          Nothing -> Nothing
          Just indid ->
            case OM.lookup indid numbers of
              Just _ -> Nothing
              Nothing -> Just indid
  let indicatorE = attachPromptlyDynWithMaybe test storeSummaryNumbersD messageE

  indicatorD <- holdDyn Nothing (Just <$> indicatorE)

  numbersE <- do
    let jsonfileD
          = ffor indicatorD $ \case
                Nothing -> Left "indicator-not-defined"        
                Just (IndicatorId ind) -> Right (ind <> ".json")
    apiGetIndicatorSummaryNumbers jsonfileD (() <$ indicatorE)

  numbersD <-
    foldDyn (uncurry OM.insert) OM.empty
                 $ attachPromptlyDynWithMaybe (\mind i -> (,i) <$> mind) indicatorD
                 $ fmapMaybe id
                 $ catchApi "getIndicatorSummaryNumbers" numbersE

  return $ store { storeSummaryNumbersD = numbersD }

-- getChartData
--   :: ( MonadHold t m
--      , Reflex t
--      , SupportsServantReflex t m
--      , HasJSContext (Performable m)
--      )
--   =>  Dynamic t Text -> m (Dynamic t (Maybe ChartData))
-- getChartData filenameD = do
-- 
-- --   updated filenameD ...
-- --       lookInMemory ... <|>
-- --       lookInLocalStorate ... <|>
-- --       lookOnDisk ... <|>
-- --       fetchUsing servant <|>
-- 
--   chartDataE <- apiGetChartData (Right <$> filenameD) (() <$ updated filenameD)
-- 
--   holdDyn Nothing $ fmap reqSuccess chartDataE


catchApi
  :: Reflex t
  => String
  -> Event t (ReqResult () a)
  -> Event t (Maybe a)
catchApi msg eveE =
  let tracedEventE = traceEventWith (showReqResult msg) eveE
  in  fmap reqSuccess tracedEventE

showReqResult :: String -> ReqResult t a -> String
showReqResult apiPrefix rr = (apiPrefix ++) . unpack $
    case rr of
        ResponseSuccess _ _ _   -> "Success"
        ResponseFailure _ msg _ -> "Response failure: " <> msg
        RequestFailure _ msg    -> "Request failure: " <> msg


type GetAreas = "db" :> "dev" :> "areas.json" :> Get '[JSON] Areas
type GetThemes = "db" :> "dev" :> "themes.json" :> Get '[JSON] [Theme]
type GetAreaSummaries = "db" :> "dev" :> "area-summaries.json" :> Get '[JSON] AreaSummaries
type GetAreaTrees = "data" :> "areaTrees-11d88bc13.json" :> Get '[JSON] [AreaTree]
type GetFeatures = "data" :> "features-11d88bc13.json" :> Get '[JSON] [Feature]

type GetMapSummaries = "data" :> Capture "filename" Text :> Get '[JSON] MapSummary
type GetChartData = "chartdata" :> Capture "filename" Text :> Get '[JSON] ChartData
type GetIndicatorSummaryNumbers
  = "db" :> "dev" :> Capture "<indicator>.json" Text
                  :> Get '[JSON] IndicatorSummary

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

_apiGetAreaTrees
    :: forall t m . SupportsServantReflex t m => Client t m GetAreaTrees ()
_apiGetAreaTrees
    = client (Proxy :: Proxy GetAreaTrees) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

_apiGetFeatures
    :: forall t m . SupportsServantReflex t m => Client t m GetFeatures ()
_apiGetFeatures
    = client (Proxy :: Proxy GetFeatures) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

_apiGetChartData
    :: forall t m . SupportsServantReflex t m => Client t m GetChartData ()
_apiGetChartData
    = client (Proxy :: Proxy GetChartData) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/data"))


apiGetIndicatorSummaryNumbers
    :: forall t m . SupportsServantReflex t m => Client t m GetIndicatorSummaryNumbers ()
apiGetIndicatorSummaryNumbers
    = client (Proxy :: Proxy GetIndicatorSummaryNumbers) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

