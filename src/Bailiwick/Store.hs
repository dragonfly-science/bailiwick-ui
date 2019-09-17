{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Bailiwick.Store
  ( Store(..)
  , run
  )
where

import Control.Monad.Fix (MonadFix)
import Data.Proxy
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Data.Text (Text)

import Servant.API
import Servant.Reflex
import Reflex.Dom.Core
import Language.Javascript.JSaddle.Types (MonadJSM)
import qualified Data.HashMap.Strict.InsOrd as OM

import Bailiwick.Types
import Bailiwick.Route

data Store t
  = Store
    { storeAreasD          :: Dynamic t (Loadable Areas)
    , storeThemesD         :: Dynamic t (Loadable [Theme])
    , storeSummariesD      :: Dynamic t (Loadable AreaSummaries)
    , storeIndicatorsDataD :: Dynamic t (OM.InsOrdHashMap IndicatorId (Loadable IndicatorData))
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
run messageE = do

  let isReady (Ready _) = True
      isReady _ = False
      triggerE = () <$ ffilter isReady messageE

  areasE     <- apiGetAreas triggerE
  areasD     <- holdDyn Loading $ catchApi "getAreas" areasE
  themesE    <- apiGetThemes triggerE
  themesD    <- holdDyn Loading $ catchApi "getThemes" themesE
  summariesE <- apiGetAreaSummaries triggerE
  summariesD <- holdDyn Loading $ catchApi "getAreaSummaries" summariesE

  indicatorsDataD <- summaryNumbers messageE

  return $
    Store
      { storeAreasD          = areasD
      , storeThemesD         = themesD
      , storeSummariesD      = summariesD
      , storeIndicatorsDataD = indicatorsDataD
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
  -> m (Dynamic t (OM.InsOrdHashMap IndicatorId (Loadable IndicatorData)))
summaryNumbers messageE = mdo

  let test numbers message =
        case getIndicatorId message of
          Nothing -> Nothing
          Just indid ->
            case OM.lookup indid numbers of
              Just _ -> Nothing
              Nothing -> Just indid

      newIndicatorE = (,Loading) <$> attachWithMaybe test (current dataD) messageE

      findLoading = listToMaybe . OM.keys . OM.filter (== Loading)
      mkPath (Just (IndicatorId ind)) = Right (ind <> ".json")
      mkPath Nothing = Left "nothing-to-do"
      loadingD = findLoading <$> dataD

  let unpack (Loaded indata) = Just (indicatorIdent indata, Loaded indata)
      unpack _ = Nothing

      numbersLoadedE = fmapMaybe unpack $ catchApi "getIndicatorData" numbersE

  numbersE <- apiGetIndicatorData
                  (mkPath <$> loadingD)
                  (() <$ fmapMaybe id (updated loadingD))


  dataD <- foldDyn (uncurry OM.insert) OM.empty $
                 leftmost [ newIndicatorE , numbersLoadedE ]
  return dataD

catchApi
  :: Reflex t
  => String
  -> Event t (ReqResult () a)
  -> Event t (Loadable a)
catchApi msg eveE =
  let tracedEventE = traceEventWith (showReqResult msg) eveE
      success = \case
        ResponseSuccess _ x _ -> Loaded x
        _                     -> Missing
  in  fmap success tracedEventE

showReqResult :: String -> ReqResult t a -> String
showReqResult apiPrefix rr = (apiPrefix ++) . Text.unpack $
    case rr of
        ResponseSuccess _ _ _   -> ": Success"
        ResponseFailure _ msg _ -> ": Response failure: " <> msg
        RequestFailure _ msg    -> ": Request failure: " <> msg


type GetAreas = "db" :> "dev" :> "areas.json" :> Get '[JSON] Areas
type GetThemes = "db" :> "dev" :> "themes.json" :> Get '[JSON] [Theme]
type GetAreaSummaries = "db" :> "dev" :> "area-summaries.json" :> Get '[JSON] AreaSummaries
type GetIndicatorData = "db" :> "dev" :> Capture "<indicator>.json" Text :> Get '[JSON] IndicatorData

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

apiGetThemes
    :: forall t m . SupportsServantReflex t m => Client t m GetThemes ()
apiGetThemes
    = client (Proxy :: Proxy GetThemes) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

apiGetIndicatorData
    :: forall t m . SupportsServantReflex t m => Client t m GetIndicatorData ()
apiGetIndicatorData
    = client (Proxy :: Proxy GetIndicatorData) (Proxy :: Proxy m)
        (Proxy :: Proxy ()) (constDyn (BasePath "/"))

