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
--  ( Store(..)
--  , run
--  , empty
--  )
where

import Control.Monad ((>=>), join)
import Control.Applicative (liftA)
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

empty :: Reflex t => Store t
empty
  = Store
    { storeAreasD          = constDyn Loading
    , storeThemesD         = constDyn Loading
    , storeSummariesD      = constDyn Loading
    , storeIndicatorsDataD = constDyn OM.empty
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
       = concat
         [ [ initialData messagesE ]
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
  let isReady (Ready _) = True
      isReady _ = False
  let triggerE = () <$ ffilter isReady messagesE

  areasE     <- apiGetAreas triggerE
  themesE    <- apiGetThemes triggerE
  summariesE <- apiGetAreaSummaries triggerE

  areasD     <- holdDyn Loading $ catchApi "getAreas" areasE
  themesD    <- holdDyn Loading $ catchApi "getThemes" themesE
  summariesD <- holdDyn Loading $ catchApi "getAreaSummaries" summariesE

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
  -> [ Store t -> m (Store t) ]
summaryNumbers messageE =

  let updateNumbers numsD eventE =
         join <$> foldDyn (liftA . (uncurry OM.insert)) numsD eventE

      markLoading store = do
        let oldNumbersD = storeIndicatorsDataD store
            test numbers message =
              case getIndicatorId message of
                Nothing -> Nothing
                Just indid ->
                  case OM.lookup indid numbers of
                    Just _ -> Nothing
                    Nothing -> Just indid
            indicatorE = attachWithMaybe test (current oldNumbersD) messageE

        newNumbersD <-
             updateNumbers
                 oldNumbersD
                 ((, Loading) <$> indicatorE)
        return $ store { storeIndicatorsDataD = newNumbersD }

      doCall store = do
        let oldNumbersD = storeIndicatorsDataD store
            findLoading = listToMaybe . OM.keys . OM.filter (== Loading)
            mkPath (Just (IndicatorId ind)) = Right (ind <> ".json")
            mkPath Nothing = Left "nothing-to-do"
            loadingD = findLoading <$> storeIndicatorsDataD store

        numbersE <- apiGetIndicatorData
                        (mkPath <$> loadingD)
                        (() <$ fmapMaybe id (updated loadingD))

        let unpack :: Loadable IndicatorData -> Maybe (IndicatorId, Loadable IndicatorData)
            unpack (Loaded indata) = Just (indicatorIdent indata, Loaded indata)
            unpack _ = Nothing

        newNumbersD <-
            updateNumbers
                 oldNumbersD
                 $ fmapMaybe unpack
                 $ catchApi "getIndicatorData" numbersE

        return $ store { storeIndicatorsDataD = newNumbersD }

  in [ markLoading, doCall ]


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
        ResponseSuccess _ _ _   -> "Success"
        ResponseFailure _ msg _ -> "Response failure: " <> msg
        RequestFailure _ msg    -> "Request failure: " <> msg


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

