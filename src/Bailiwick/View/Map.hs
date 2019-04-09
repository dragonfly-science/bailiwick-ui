{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE RankNTypes          #-}


module Bailiwick.View.Map
  ( nzmap
  , MapState(..)
  )
where

import Control.Monad ((>=>), (<=<), forever, void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent
import Control.Applicative (liftA2, Alternative(..))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isNothing, isJust, fromJust)
import Data.Foldable (Foldable(..), forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict.InsOrd as OM (lookup, elems)

import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Performance as Performance (now)
import qualified GHCJS.DOM.MediaQueryList as MQ (removeListener, addListener)
import GHCJS.DOM.MouseEvent (IsMouseEvent)
import GHCJS.DOM.EventM (mouseClientXY)
import GHCJS.DOM.Types
       (MediaQueryList(..), NodeList(..), DOMHighResTimeStamp, IsElement,
        HTMLElement(..), HTMLObjectElement(..), uncheckedCastTo)
#ifdef ghcjs_HOST_OS
import GHCJS.DOM (currentWindowUnchecked, waitForAnimationFrame)
#else
import GHCJS.DOM (currentWindowUnchecked)
#endif
import GHCJS.DOM.ParentNode (querySelector, querySelectorUnsafe, querySelectorAll)
import GHCJS.DOM.Element (setAttribute)
import GHCJS.DOM.HTMLObjectElement (getContentDocument)
import GHCJS.DOM.GlobalPerformance (getPerformance)
import GHCJS.DOM.Window (matchMedia)
import GHCJS.DOM.MediaQueryList (getMatches)
import GHCJS.DOM.MediaQueryListListener (newMediaQueryListListenerAsync)

import Language.Javascript.JSaddle.Types (MonadJSM)
import Language.Javascript.JSaddle
       (call, eval, ToJSString, runJSM, askJSM, liftJSM)
import Reflex.Dom.Core
import Reflex.Dom.Builder.Immediate (wrapDomEvent)

import Bailiwick.Route
import Bailiwick.Types

data MapState t
  = MapState
    { routeD             :: Dynamic t Route
    , regionD            :: Dynamic t (Maybe Area)
    , subareaD           :: Dynamic t (Maybe Area)
    , areasD             :: Dynamic t (Maybe Areas)
    , indicatorSummaryD  :: Dynamic t IndicatorSummary
    }

switchDynM
 :: (MonadHold t m, DomBuilder t m, PostBuild t m)
 => Dynamic t (m (Event t a)) -> m (Event t a)
switchDynM = (switchHold never =<<) . dyn


slugify :: Text -> Text
slugify = Text.replace "'" ""
        . Text.replace " " "-"
        . Text.toLower

now :: MonadJSM m => m DOMHighResTimeStamp
now = currentWindowUnchecked >>= getPerformance >>= Performance.now

attachTime
  :: ( PerformEvent t m
     , MonadJSM (Performable m))
  => Event t a
  -> m (Event t (DOMHighResTimeStamp, a))
attachTime e = performEvent $ ffor e $ \a -> (,a) <$> now

animationFrame
  :: ( Monad m
     , MonadJSM m
     , Reflex t
     , TriggerEvent t m
     , MonadHold t m
     , MonadFix m
     , PerformEvent t m
     , MonadJSM (Performable m))
  => Dynamic t Bool
  -> m (Event t DOMHighResTimeStamp)
animationFrame sendEvents = do
  (e, send) <- newTriggerEvent
  stateChange <- updated <$> holdUniqDyn sendEvents
  sendEventsMVar <- liftIO newEmptyMVar
  ctx <- liftJSM askJSM
  _ <- liftIO . forkIO $ forever $ do
#ifdef ghcjs_HOST_OS
    takeMVar sendEventsMVar
    runJSM waitForAnimationFrame ctx >>= send
#else
    threadDelay 100000
    takeMVar sendEventsMVar
    runJSM now ctx >>= send
#endif
  performEvent_ $ ffor
    (leftmost [stateChange, tag (current sendEvents) e]) $ \enabled ->
      when enabled . void . liftIO $ tryPutMVar sendEventsMVar ()
  return e

data TransitionState = TransitionState
  { startTime    :: DOMHighResTimeStamp
  , active       :: Bool
  , currentValue :: Double
  , startValue   :: Double
  , targetValue  :: Double
  }

transition
  :: ( Monad m
     , MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , PostBuild t m
     )
  => Event t DOMHighResTimeStamp           -- Animation frame
  -> DOMHighResTimeStamp                   -- Duration
  -> Dynamic t Double                      -- Input value
  -> m (Dynamic t Bool, Dynamic t Double)  -- (active?, value)
transition frame duration input = do
  postBuild <- getPostBuild
  uniqInput <- holdUniqDyn input
  changes <-
    attachTime $ leftmost [ updated uniqInput
                          , tagPromptlyDyn uniqInput postBuild ]
  let f (Right (tNew, vNew)) Nothing
        = Just (TransitionState tNew False vNew vNew vNew)
      f (Right (tNew, vNew)) (Just TransitionState{..})
        = Just (TransitionState tNew True currentValue currentValue vNew)
      f (Left _) Nothing
        = Nothing
      f (Left tNew) (Just ts@TransitionState{..}) =
        let stillActive = tNew - startTime <= duration
        in Just ts
          { active = stillActive
          , currentValue =
             if stillActive
                then startValue + ((targetValue - startValue) *
                                   g (max 0 (tNew - startTime) / duration))
                else targetValue
          }
      g :: Double -> Double
      g x = let x2 = x * x
                x3 = x2 * x
            in 3 * x2 - 2 * x3

  transitionState :: Dynamic t (Maybe TransitionState) <-
    foldDyn f Nothing $ leftmost [Right <$> changes, Left <$> frame]
  return ( maybe False active <$> transitionState
         , fromMaybe <$> input <*> (fmap currentValue <$> transitionState))

class CommutesWithFunctor f where
  commuteWith :: Functor d => d (f a) -> f (d a)

transitions
  :: forall t m f.
     ( Monad m
     , MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , PostBuild t m
     , Traversable f
     , CommutesWithFunctor f
     )
  => Event t DOMHighResTimeStamp
  -> DOMHighResTimeStamp
  -> Dynamic t (f Double)
  -> m (Dynamic t Bool, Dynamic t (f Double))
transitions frame duration input = do
  x :: f (Dynamic t Bool, Dynamic t Double)
    <- mapM (transition frame duration) (commuteWith input)
  let activeD = foldl (liftA2 (||)) (constDyn False) $ fst <$> x
  return (activeD, sequenceA (snd <$> x))

attachPrevious
  :: ( Monad m, Reflex t, MonadHold t m )
  => Event t a
  -> m (Event t (Maybe a, a))
attachPrevious e = do
  d <- holdDyn Nothing (Just <$> e)
  return $ attach (current d) e

data RGB a = RGB
  { _red   :: a
  , _green :: a
  , _blue :: a
  } deriving (Show, Functor, Foldable, Traversable, Eq)

instance CommutesWithFunctor RGB where
  commuteWith input = RGB (_red <$> input) (_green <$> input) (_blue <$> input)

data Translate a = Translate
  { _x :: a
  , _y :: a
  } deriving (Show, Functor, Foldable, Traversable, Eq)

translateString :: Translate Double -> Text
translateString Translate{..}
  = "translate(" <> Text.pack (show _x) <> "," <> Text.pack (show _y) <> ")"

instance CommutesWithFunctor Translate where
  commuteWith input = Translate (_x <$> input) (_y <$> input)

data Scale a = Scale
  { _xScale :: a
  , _yScale :: a
  } deriving (Show, Functor, Foldable, Traversable, Eq)

scaleString :: Scale Double -> Text
scaleString Scale{..}
  = "scale(" <> Text.pack (show _xScale) <> "," <> Text.pack (show _yScale) <> ")"

transformString :: Translate Double -> Scale Double -> Text
transformString t s = translateString t <> " " <> scaleString s

instance CommutesWithFunctor Scale where
  commuteWith input = Scale (_xScale <$> input) (_yScale <$> input)

rgbString :: RGB Double -> Text
rgbString RGB{..}
  = "rgb(" <> str _red <> "," <> str _green <> "," <> str _blue <> ")"
    where str n = Text.pack $ show (round n :: Int)

data ZoomState a = ZoomState
  { _defaultBackground         :: RGB a
  , _selectedRegionBackground  :: RGB a
  , _defaultOutline            :: RGB a
  , _sameRegionOutline         :: RGB a
  , _coastline                 :: RGB a
  , _translate                 :: Translate a
  , _scale                     :: Scale a
  , _strokeWidth               :: a
  } deriving (Show, Functor, Foldable, Traversable, Eq)

instance CommutesWithFunctor ZoomState where
  commuteWith i
   = ZoomState
     (commuteWith $ _defaultBackground <$> i)
     (commuteWith $ _selectedRegionBackground <$> i)
     (commuteWith $ _defaultOutline <$> i)
     (commuteWith $ _sameRegionOutline <$> i)
     (commuteWith $ _coastline <$> i)
     (commuteWith $ _translate <$> i)
     (commuteWith $ _scale <$> i)
     (_strokeWidth <$> i)

zoomState :: Bool -> Bool -> Text -> ZoomState Double
zoomState wide z selectedArea
  = ZoomState
    { _defaultBackground        = if z then RGB 223 241 252 else RGB  41 101 117
    , _selectedRegionBackground = if z then RGB  41 101 117 else RGB   0 189 233
    , _defaultOutline           = if z then RGB 255 255 255 else RGB 255 255 255
    , _sameRegionOutline        = if z then RGB 223 241 252 else RGB  41 101 117
    , _coastline                = if z then RGB 255 255 255 else RGB  41 101 117
    , _translate                = translate
    , _scale                    = scale
    , _strokeWidth              = strokeWidth
    }
  where
    (translate, scale, strokeWidth)
        = if z
            then regionTransform $ slugify selectedArea
            else transform 0 727.5 1 (-1) 0.8

    transform :: Double -> Double -> Double -> Double -> Double
              -> (Translate Double, Scale Double, Double)
    transform x y sx sy _sw
         = ( Translate (x + if wide then 113 else 0) y
           , Scale sx sy
           , if z then 1.2/sx else 0.6
           )
    regionTransform "auckland"          = transform (-3080) 6370 10   (-10)   0.3
    regionTransform "bay-of-plenty"     = transform (-2050) 3240  5.5 ( -5.5) 0.3
    regionTransform "canterbury"        = transform ( -350) 1000  3   ( -3)   0.55
    regionTransform "gisborne"          = transform (-2506) 3400  6   ( -6)   0.45
    regionTransform "hawkes-bay"        = transform (-1800) 2550  5   ( -5)   0.4
    regionTransform "manawatu-wanganui" = transform (-1250) 2330  4.5 ( -4.5) 0.45
    regionTransform "marlborough"       = transform (-1400) 2348  6   ( -6)   0.45
    regionTransform "nelson"            = transform (-1900) 3200  8   ( -8)   0.35
    regionTransform "northland"         = transform (-1070) 3670  5   ( -5)   0.4
    regionTransform "otago"             = transform ( -160) 828   4   ( -4)   0.53
    regionTransform "southland"         = transform    100  700   4   ( -4)   0.53
    regionTransform "taranaki"          = transform (-2260) 4000  8   ( -8)   0.35
    regionTransform "tasman"            = transform (-1080) 2346  6   ( -6)   0.45
    regionTransform "waikato"           = transform (-1100) 2480  4   ( -4)   0.53
    regionTransform "wellington"        = transform (-2250) 2900  7   ( -7)   0.35
    regionTransform "west-coast"        = transform ( -210) 1170  3   ( -3)   0.6
    regionTransform _                   = transform      0  727.5 1   ( -1)   0.8

data Map
  = Map
    { _zoom           :: Bool
    , _zoomState      :: ZoomState Double
    , _mouseAreaInfo  :: Maybe AreaInfo
    , _region         :: Maybe Text
    , _subarea        :: Maybe Text
    , _regionChildren :: [Text]
    , _areas          :: [(Text, Text)]
    , _areaType       :: Maybe Text
    , _feature        :: Maybe FeatureId
    , _year           :: Maybe Year
    , _numbers        :: IndicatorSummary
    }
   deriving (Show, Eq)

_zoomRegion :: Map -> Maybe Text
_zoomRegion Map{ _zoom = False } = Nothing
_zoomRegion Map{ _region = r } = r

_selectedSubareaClass :: Map -> Maybe Text
_selectedSubareaClass Map{..}
  = (<> if _region == Just "auckland" then "-ward" else "-ta") <$> _subarea

aiRegionClass :: AreaInfo -> Maybe Text
aiRegionClass = fmap (slugify . (<> "-region")) . areaRegion

mouseOverRegionClass :: Map -> Maybe Text
mouseOverRegionClass = (>>= aiRegionClass) . _mouseAreaInfo

aiSubareaClass :: AreaInfo -> Maybe Text
aiSubareaClass AreaInfo{..}
  = (slugify . (<> "-ward") <$> areaWard) <|> (slugify . (<> "-ta") <$> areaTa)

mouseOverSubareaClass :: Map -> Maybe Text
mouseOverSubareaClass Map{_mouseAreaInfo = ai} = ai >>= aiSubareaClass

forNodesSetAttribute
  :: ( MonadJSM m
     , ToJSString name
     , ToJSString value)
  => NodeList
  -> name
  -> value
  -> m ()
forNodesSetAttribute nodeList name val = liftJSM $ do
  f <- eval (  "(function(list, name, value) {"
            <> "    for (var i = 0; i < list.length; i++) {"
            <> "        list[i].setAttribute(name, value);"
            <> "    }"
            <> "})" :: Text)
  void $ call f f (nodeList, name, val)

mediaQueryChange
  :: ( Monad m
     , MonadJSM m
     , TriggerEvent t m)
  => MediaQueryList
  -> m (Event t Bool)
mediaQueryChange mediaQueryList = do
  ctx <- askJSM
  newEventWithLazyTriggerWithOnComplete $ \trigger
    -> (`runJSM` ctx) $ do
      listener <- newMediaQueryListListenerAsync . mapM_ $ getMatches >=> \m -> liftIO $ trigger m (return ())
      MQ.addListener mediaQueryList (Just listener)
      return . (`runJSM` ctx) $ MQ.removeListener mediaQueryList (Just listener)

mediaQueryDyn
  :: ( Monad m
     , MonadJSM m
     , TriggerEvent t m
     , MonadHold t m
     , DOM.ToJSString queryString)
  => queryString
  -> m (Dynamic t Bool)
mediaQueryDyn queryString = do
  window <- currentWindowUnchecked
  mediaQueryList <- matchMedia window queryString
  initiallyMatches <- getMatches mediaQueryList
  holdDyn initiallyMatches =<< mediaQueryChange mediaQueryList


nzmap
    :: forall m t.
       ( Monad m
       , MonadJSM m
       , MonadFix m
       , DomBuilder t m
       , PostBuild t m
       , TriggerEvent t m
       , HasJSContext (Performable m)
       , PerformEvent t m
       , MonadJSM (Performable m)
       , MonadHold t m
       , DomBuilderSpace m ~ GhcjsDomSpace
       )
    => MapState t
    -> m (Event t Message)
nzmap MapState{..} = mdo

  zoomD <- holdUniqDyn ( hasAdapter Mapzoom <$> routeD)
  let areaD = zipDynWith (<|>) subareaD regionD

  -- Show the svg when it is loaded
  svgVisibilityD
    <- holdDyn ("style" =: "visibility: hidden;") $ mempty <$ loadedSvg
  let svgObjectAttrD
         = (( "type"  =: "image/svg+xml"
           <> "data"  =:" /assets/map.svg"
           <> "class" =: "map"
           ) <>) <$> svgVisibilityD

  -- Create the object for holding the svg.
  svgObject
    <- uncheckedCastTo HTMLObjectElement . _element_raw . fst <$>
           elDynAttr' "object" svgObjectAttrD (return ())

  svgBodyE :: Event t DOM.HTMLElement
     <- wrapDomEvent svgObject (`DOM.on` DOM.load) $ do
          svgDoc <- getContentDocument svgObject
          uncheckedCastTo HTMLElement <$>
              querySelectorUnsafe svgDoc ("svg" :: Text)

  -- Setup the zoom state dyanmic, and transitions
  wide <- mediaQueryDyn ("(min-width: 1025px)" :: Text)
  let duration = 500
  frame <- animationFrame zoomAnimating
  let zoomStateD = zoomState <$> wide <*> zoomD <*> (maybe "nz" areaId <$> regionD)
  (zoomAnimating, zoomStateT) <- transitions frame duration zoomStateD

  let level2type a "reg" = (a, "region")
      level2type a "ward" = ("auckland--" <> a, "ward")
      level2type a lvl = (a, lvl)
      areaAreaTypes :: Areas -> [(Text, Text)]
      areaAreaTypes (Areas as) = [ level2type areaId areaLevel
                                 | Area{..} <- OM.elems as ]
  let mapD
        = Map <$> zoomD
              <*> zoomStateT
              <*> mouseOverD
              <*> (fmap areaId <$> regionD)
              <*> (fmap areaId <$> subareaD)
              <*> (maybe [] areaChildren <$> areaD)
              <*> (maybe [] areaAreaTypes <$> areasD)
              <*> ((fmap themePageAreaType . getThemePage) <$> routeD)
              <*> ((themePageFeatureId <=< getThemePage) <$> routeD)
              <*> (fmap themePageYear . getThemePage <$> routeD)
              <*> indicatorSummaryD

  let isSummaryD = (Summary ==) . routePage <$> routeD
  svgBodyD <- holdDyn Nothing (Just <$> svgBodyE)
  loadedSvg <- switchDynM $ ffor ((,) <$> isSummaryD <*> svgBodyD) $ \case
    (_, Nothing)          -> return never
    (True, Just svgBody)  -> updateMapSummary svgBody mapD
    (False, Just svgBody) -> updateMapIndicator svgBody mapD


  let tooltipArea
         :: Maybe Areas
         -> Route
         -> Maybe (AreaInfo, (Int, Int))
         -> Maybe ((AreaInfo, (Int, Int)), Area)
      tooltipArea _ _ Nothing = Nothing
      tooltipArea Nothing _ _ = Nothing
      tooltipArea (Just areas) route (Just (ai, xy)) =
        let maybeAreaId =
              if hasAdapter Mapzoom route
                then areaWard ai <|> areaTa ai
                else areaRegion ai
        in ((ai, xy),) <$> (((`OM.lookup` (unAreas areas)) . slugify) =<< maybeAreaId)
      tooltipAreaD :: Dynamic t (Maybe ((AreaInfo, (Int, Int)), Area))
      tooltipAreaD = tooltipArea <$> areasD <*> routeD <*> mouseOverFullD
      showStyle Nothing = "visibility:hidden;"
      showStyle (Just ((_, (x,y)), _)) = Text.pack $
          "visibility:visible; left:" <> show (x + 8) <> "px; top:" <> show (y + 8) <> "px;"
  elDynAttr "div" (("class" =: "tooltip" <>) . ("style" =:) . showStyle <$> tooltipAreaD) $ do
    el "p" $ dynText $ maybe "" (areaName . snd) <$> tooltipAreaD
    --elDynAttr "p" (("class" =: ) <$> (fromMaybe "" <$> transformD) <> " number") $ text "Val"

  moveE
    :: Event t (Maybe (AreaInfo, (Int, Int)))
    <- switchDynM $ ffor svgBodyD $ \case
         Just divElement ->
           wrapDomEvent divElement (`DOM.onSync` DOM.mouseMove) getAreaInfoFromSvg
         _ -> return never
  clickE
    :: Event t (Maybe AreaInfo)
    <- switchDynM $ ffor svgBodyD $ \case
         Just divElement ->
           fmap (fmap fst) <$> wrapDomEvent divElement (`DOM.on` DOM.click) getAreaInfoFromSvg
         _ -> return never
  leaveE
    <- switchDynM $ ffor svgBodyD $ \case
         Just divElement ->
           wrapDomEvent divElement (`DOM.onSync` DOM.mouseLeave) $ return ()
         _ -> return never

  mouseOverFullD
    :: Dynamic t (Maybe (AreaInfo, (Int, Int)))
    <- holdDyn Nothing $
         leftmost [ moveE
                  , Nothing <$ clickE
                  , Nothing <$ leaveE
                  ]
  mouseOverD
     :: Dynamic t (Maybe AreaInfo)
     <- holdUniqDyn $ fmap fst <$> mouseOverFullD

  -- The click event depends on the state
  let makeMessages
        :: (Maybe Area, Maybe Area, Route)
        -> Maybe AreaInfo
        -> Maybe Message
      makeMessages (mregion, msubarea, route) ai =
        let region         = slugify <$> (areaRegion =<< ai)
            ta             = slugify <$> (areaTa =<< ai)
            ward           = slugify <$> (areaWard =<< ai)
            currentRegion  = areaId <$> mregion
            currentSubarea = areaId <$> msubarea
            iszoomed       = hasAdapter Mapzoom route
            auckland       = "auckland" :: Text
            subarea        = if region == Just auckland then ward else ta
        in  if
            | currentRegion == region &&
              not iszoomed
                -> Just ZoomIn
            | currentRegion /= region &&
              isJust region
                -> Just (SetRegion (fromJust region))
            | currentRegion /= region &&
              isJust currentRegion &&
              isJust currentSubarea
                -> Just (SetRegion (fromJust currentRegion))
            | currentSubarea /= subarea &&
              iszoomed &&
              isJust subarea
                -> Just (SetSubArea $ fromJust subarea)
            | isNothing region &&
              iszoomed
                -> Just (ZoomOut region)
            | otherwise
                -> Nothing

      combinedD = do
        region  <- regionD
        subarea <- subareaD
        route   <- routeD
        return (region, subarea, route)
  return $ attachPromptlyDynWithMaybe makeMessages combinedD clickE



updateMapSummary
  :: ( PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , IsElement self
     , MonadHold t m
     )
  => self -> Dynamic t Map -> m (Event t ())
updateMapSummary svgBody mapD = do
  let setAttr
         :: (MonadJSM m0)
         => Text -> Text -> Text -> m0 ()
      setAttr q name val = do
        nodeList <- querySelectorAll svgBody q
        forNodesSetAttribute nodeList name val

      setAttrs :: (MonadJSM m0) => Text -> [(Text, Text)] -> m0 ()
      setAttrs q = mapM_ (uncurry $ setAttr q)

      set :: (MonadJSM m0, IsElement e) => Text -> Text -> e -> m0 ()
      set a v e = setAttribute e a v


  postBuild <- getPostBuild
  performEvent_ $ postBuild $> do
    set "height" "550px" svgBody
    setAttr "path" "stroke" "none"
    setAttr "path" "stroke-width" "0.001"
    setAttr "path" "cursor" "pointer"
    setAttr "polyline" "stroke-linecap" "butt"
    setAttr "polyline" "cursor" "pointer"

  mapE <- attachPrevious $
           leftmost [ updated mapD
                    , tagPromptlyDyn mapD postBuild
                    ]

  -- Main update function
  performEvent . ffor mapE $ \(old, new) -> do

    -- Update the transform, but only if it has changed
    when ((_zoomState <$> old) /= Just (_zoomState new)) $ do
      let transform = transformString (_translate $ _zoomState new)
                                          (_scale $ _zoomState new)
      querySelector svgBody ("g" :: Text) >>= mapM_
        (set "transform" transform)

    let bg   = rgbString . _defaultBackground $ _zoomState new
        srbg = rgbString . _selectedRegionBackground $ _zoomState new
        ol   = rgbString . _defaultOutline $ _zoomState new
        srol = rgbString . _sameRegionOutline $ _zoomState new
        cl   = rgbString . _coastline $ _zoomState new
        sw   = _strokeWidth $ _zoomState new
        changed = if (_zoomState <$> old) /= Just (_zoomState new)  ||
                     (_region    <$> old) /= Just (_region new)     ||
                     (_subarea   <$> old) /= Just (_subarea new)
                    then ""
                    else maybe "" ("." <>) $ mouseOverRegionClass =<< old

    -- Reset the properties of the changed elements
    setAttr ("g" <> changed <> " > polyline") "stroke-width" (Text.pack . show $ sw * 2)
    if _zoom new
      then
        forM_ (_region new) $ \r -> do
          let subareaType = if r == "auckland" then "ward" else "ta"
          setAttr ("g" <> changed <> "[same_reg=TRUE] > polyline")
                  "stroke-width" (Text.pack . show $ sw)
          setAttr ("g" <> changed <> ".inbound[same_reg=FALSE] > polyline")
                  "stroke" ol
          setAttr ("g" <> changed <> ".inbound[same_reg=TRUE] > polyline")
                  "stroke" srol
          setAttr ("g" <> changed <> ".inbound[same_reg=TRUE][same_"
                                  <> subareaType <> "=TRUE] > polyline")
                  "stroke-width" "1.0"
      else do
        setAttr ("g" <> changed <> ".inbound[same_reg=FALSE] > polyline")
                "stroke" ol
        setAttrs ("g" <> changed <> ".inbound[same_reg=TRUE] > polyline")
                 [ ("stroke", srol)
                 , ("stroke-width", "1.5")]
    setAttr ("g" <> changed <> ".coastline > polyline")
            "stroke" cl
    setAttr ("g" <> changed <> " > path")
            "fill" bg

    if _zoom new
      then do
        -- Clear old mouse over subarea that is outside the selected region
        --  (and so will not be included when we clear the selected region)
        forM_ (_region =<< old) $ \r -> do
          let regionClass = slugify r <> "-region"
          case _mouseAreaInfo =<< old of
            Just AreaInfo{..} | (slugify <$> areaRegion) == Just r ->
              forM_ (mouseOverSubareaClass =<< old) $ \cssClass -> do
                setAttr ("g:not(." <> regionClass <> ")." <> cssClass <> " > path")
                        "fill" bg
                setAttr ("g:not(." <> regionClass <> ")." <> cssClass <> " > polyline")
                        "stroke" ol
                setAttr ("g:not(." <> regionClass <> ")." <> cssClass
                                   <> "[same_reg=TRUE] > polyline")
                        "stroke" bg
            _ -> return ()

        forM_ (_region new) $ \r -> do
          let regionClass = slugify r <> "-region"
              subareaType = if r == "auckland" then "ward" else "ta"

          -- Selected region
          setAttr ("g." <> regionClass <> " > path") "fill" srbg
          setAttr ("g.coastline." <> regionClass <> " > polyline")
                  "stroke" "none"
          setAttr ("g.inbound." <> regionClass <> "[same_"
                                <> subareaType <> "=TRUE] > polyline")
                  "stroke" srbg
          setAttr ("g.inbound." <> regionClass <> "[same_reg=TRUE][same_"
                                <> subareaType <> "=FALSE] > polyline")
                  "stroke" "rgb(106,142,156)"
          setAttr ("g.inbound." <> regionClass <> "[same_reg=FALSE][same_"
                                <> subareaType <> "=FALSE] > polyline")
                  "stroke" "none"

          -- Remainder of subareas with some part the seleced region
          forM_ (_regionChildren new) $ \child -> do
            setAttr ("g:not(." <> regionClass <> ")."
                         <> slugify child <> "-" <> subareaType <> " > path")
                    "fill" "rgb(181,209, 223)"
            setAttr ("g:not(." <> regionClass <> ")."
                         <> slugify child <> "-" <> subareaType <> "[same_"
                         <> subareaType <> "=TRUE][same_reg=TRUE] > polyline")
                    "stroke" "rgb(181,209, 223)"
            setAttr ("g:not(." <> regionClass <> ")."
                         <> slugify child <> "-" <> subareaType <> "[same_"
                         <> subareaType <> "=TRUE][same_reg=FALSE] > polyline")
                    "stroke" ol

          -- New mouse over region
          forM_ (mouseOverRegionClass new) $ \cssClass -> when (cssClass /= regionClass) $ do
            let mouseOverZoomBackground = "rgb(174, 227, 248)"
            setAttr ("g." <> cssClass <> " > path") "fill" mouseOverZoomBackground
            setAttr ("g.inbound." <> cssClass <> "[same_reg=TRUE] > polyline")
                    "stroke" "rgb(160,214,236)"
            setAttr ("g.inbound." <> cssClass <> "[same_reg=TRUE][same_"
                                  <> subareaType <> "=TRUE] > polyline")
                    "stroke" mouseOverZoomBackground

          -- Mouse over subarea
          case _mouseAreaInfo new of
            Just AreaInfo{..} | (slugify <$> areaRegion) == Just r ->
              forM_ (mouseOverSubareaClass new) $ \cssClass -> do
                setAttr ("g." <> regionClass <> "." <> cssClass <> " > path")
                        "fill" "rgb(0, 189, 233)"
                setAttr ("g.inbound." <> regionClass <> "."
                             <> cssClass <> "[same_"
                             <> subareaType <> "=TRUE] > polyline")
                        "stroke" "rgb(0, 189, 233)"
                setAttr ("g:not(." <> regionClass <> ")."
                             <> cssClass <> " > path")
                        "fill" "rgb(174, 227, 248)"
                setAttr ("g.inbound:not(." <> regionClass <> ")."
                             <> cssClass <> "[same_"
                             <> subareaType
                             <> "=TRUE][same_reg=TRUE] > polyline")
                        "stroke" "rgb(174, 227, 248)"
                setAttr ("g.inbound:not(." <> regionClass <> ")."
                             <> cssClass <> "[same_"
                             <> subareaType <> "=TRUE][same_reg=FALSE] > polyline")
                        "stroke" "rgb(0, 189, 233)"
            _ -> return ()

          -- Selected subarea
          forM_ (_selectedSubareaClass new) $ \cssClass -> do
            setAttr ("g." <> regionClass <> "." <> cssClass <> " > path")
                    "fill" "rgb(0, 189, 233)"
            setAttr ("g.inbound." <> regionClass <> "."
                         <> cssClass <> "[same_"
                         <> subareaType <> "=TRUE] > polyline")
                    "stroke" "rgb(0, 189, 233)"
            setAttr ("g:not(." <> regionClass <> ")."
                         <> cssClass <> " > path")
                    "fill" "rgb(174, 227, 248)"
            setAttr ("g.inbound:not(." <> regionClass <> ")."
                         <> cssClass <> "[same_"
                         <> subareaType <> "=TRUE][same_reg=TRUE] > polyline")
                    "stroke" "rgb(174, 227, 248)"
            setAttr ("g.inbound:not(." <> regionClass <> ")."
                         <> cssClass <> "[same_"
                         <> subareaType <> "=TRUE][same_reg=FALSE] > polyline")
                    "stroke" "rgb(0, 189, 233)"
      else do
        forM_ (_region new) $ \r -> do
          setAttr ("g." <> slugify r <> "-region > path")
                  "fill" srbg
          setAttrs ("g." <> slugify r <> "-region[same_reg=TRUE] > polyline")
                   [ ("stroke", srbg)
                   , ("stroke-width", "3.0")]
          setAttr ("g." <> slugify r <> "-region.coastline > polyline")
                  "stroke" srbg
        forM_ (mouseOverRegionClass new) $ \cssClass -> do
          setAttr ("g." <> cssClass <> " > path")
                  "fill" "rgb(0, 189, 233)"
          setAttrs ("g." <> cssClass <> "[same_reg=TRUE] > polyline")
                   [ ("stroke", "rgb(0, 189, 233)")
                   , ("stroke-width", "3.0")]
          setAttr ("g." <> cssClass <> ".coastline > polyline")
                  "stroke" "rgb(0, 189, 233)"

updateMapIndicator
  :: ( PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , IsElement self
     , MonadHold t m
     )
  => self -> Dynamic t Map -> m (Event t ())
updateMapIndicator svgBody mapD = do
  let setAttr
         :: (MonadJSM m0)
         => Text -> Text -> Text -> m0 ()
      setAttr q name val = do
        nodeList <- querySelectorAll svgBody q
        forNodesSetAttribute nodeList name val

      set :: (MonadJSM m0, IsElement e) => Text -> Text -> e -> m0 ()
      set a v e = setAttribute e a v


  postBuild <- getPostBuild
  performEvent_ $ postBuild $> do
    set "height" "550px" svgBody
    setAttr "path" "stroke" "none"
    setAttr "path" "stroke-width" "0.001"
    setAttr "path" "cursor" "pointer"
    setAttr "polyline" "stroke-linecap" "butt"
    setAttr "polyline" "cursor" "pointer"

  mapE <- attachPrevious $
           leftmost [ updated mapD
                    , tagPromptlyDyn mapD postBuild
                    ]

  -- Main update function
  performEvent . ffor mapE $ \(old, new) -> do

    -- Update the transform, but only if it has changed
    when ((_zoomState <$> old) /= Just (_zoomState new)) $ do
      let transform = transformString (_translate $ _zoomState new)
                                          (_scale $ _zoomState new)
      querySelector svgBody ("g" :: Text) >>= mapM_
        (set "transform" transform)

    let ol   = rgbString . _defaultOutline $ _zoomState new
        sw   = _strokeWidth $ _zoomState new
        changed = (_feature <$> old) /= Just (_feature new) ||
                  (_year    <$> old) /= Just (_year new) ||
                  (_areaType <$> old) /= Just (_areaType new)

        getColour area = fromMaybe "#000000" $ do
            year <- _year new
            let IndicatorSummary ismap = _numbers new
            nums <- OM.lookup (area, year, _feature new) ismap
            return (colourNum nums)

        areas = if _areaType new == Just "reg"
                    then [ a | a <- _areas new, snd a == "region" ]
                    else [ a | a <- _areas new, snd a /= "region" ]

    -- Reset the properties of the changed elements
    if changed
      then do
        forM_ areas $ \(area, areatype) -> do
          let colour = getColour area
              sel = "g." <> area <> "-" <> areatype
          setAttr (sel <> ".inbound[same_reg=FALSE] > polyline")
                  "stroke" ol
          setAttr (sel <> ".inbound[same_reg=TRUE] > polyline")
                  "stroke" colour
          setAttr (sel <> ".coastline > polyline")
                  "stroke" colour
          setAttr (sel <> "[same_reg=TRUE] > polyline")
                  "stroke-width" (Text.pack . show $ sw)
          setAttr (sel <> " > path")
                  "fill" colour
      else
        return ()



data AreaInfo
  = AreaInfo
    { areaRegion  :: Maybe Text
    , areaTa      :: Maybe Text
    , areaWard    :: Maybe Text
    } deriving (Show, Eq)

getAreaInfoFromSvg
    :: DOM.IsMouseEvent ev
    => DOM.EventM e ev (Maybe (AreaInfo, (Int, Int)))
getAreaInfoFromSvg = runMaybeT $ do
  target         <- MaybeT DOM.eventTarget
  svgelement     <- MaybeT (return . Just $ DOM.uncheckedCastTo DOM.Element target)
  target_element <- DOM.getTagName svgelement
  parentg        <- MaybeT (fmap (uncheckedCastTo DOM.Element) <$> DOM.getParentNode svgelement) -- >>= MaybeT . DOM.castTo DOM.Element
  MaybeT $ mkAreaInfo target_element parentg
  where
    mkAreaInfo
      :: IsMouseEvent ev
      => Text
      -> DOM.Element
      -> DOM.EventM e ev (Maybe (AreaInfo, (Int, Int)))
    mkAreaInfo target parent = do
      let getAttr :: Text -> DOM.EventM e ev (Maybe Text)
          getAttr = DOM.getAttribute parent
          isPath = target == ("path" :: Text)
      if target == "svg"
        then return Nothing
        else do
          reg <- getAttr (if isPath then "reg" else "reg1")
          ta  <- getAttr (if isPath then "ta"  else  "ta1")
          wrd <- getAttr (if isPath then "wrd" else "wrd1")
          (x, y) <- mouseClientXY
          return $ Just (AreaInfo reg ta wrd, (x, y))

