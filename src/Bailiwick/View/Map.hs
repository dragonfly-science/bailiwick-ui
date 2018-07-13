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
module Bailiwick.View.Map
where

import Control.Applicative (liftA2, Alternative(..))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Fix
import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.Maybe
       (fromMaybe, isNothing, catMaybes, isJust, fromJust)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict.InsOrd as OM (lookup)

import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.MouseEvent (IsMouseEvent)
import GHCJS.DOM.EventM (mouseClientXY, mouseOffsetXY)
import GHCJS.DOM.HTMLElement
       (getOffsetHeight, getOffsetTop, getOffsetLeft)

import Language.Javascript.JSaddle.Types (MonadJSM)
import Reflex.Dom.Core
import Reflex.Dom.Builder.Immediate (wrapDomEvent)

import Bailiwick.State
import Bailiwick.Types
import Control.Monad.IO.Class (MonadIO(..))
import GHCJS.DOM.CSSStyleSheet (deleteRule, insertRule_)
import GHCJS.DOM.Types
       (HTMLStyleElement(..), CSSStyleSheet(..), uncheckedCastTo)
import GHCJS.DOM.HTMLStyleElement (getSheet)
import Control.Monad ((>=>), foldM, forever, void, unless, when)
import Control.Lens ((^.))
import Language.Javascript.JSaddle
       (call, eval, ToJSString, runJSM, askJSM, waitForAnimationFrame,
        liftJSM, js2)
import GHCJS.DOM.Types
       (MediaQueryList(..), NodeList(..), DOMHighResTimeStamp, IsElement,
        IsParentNode, HTMLElement(..), HTMLObjectElement(..),
        SVGElement(..), ToJSVal(..))
import GHCJS.DOM
       (currentWindowUnchecked, inAnimationFrame',
        currentDocumentUnchecked)
import GHCJS.DOM.ParentNode
       (querySelector, querySelectorUnsafe, querySelectorAll)
import qualified GHCJS.DOM.NodeList as NodeList
       (itemUnchecked, getLength)
import Data.Traversable (forM)
import GHCJS.DOM.Element (setAttribute)
import Data.Foldable (Foldable(..), forM_)
import GHCJS.DOM.HTMLObjectElement (getContentDocument)
import Data.Functor (($>))
import Data.Time (getCurrentTime)
import qualified GHCJS.DOM.Performance as Performance (now)
import GHCJS.DOM.GlobalPerformance (getPerformance)
import Control.Concurrent
       (threadDelay, tryPutMVar, takeMVar, forkIO, newEmptyMVar)
import qualified GHCJS.DOM.MediaQueryList as MQ
       (removeListener, addListener)
import GHCJS.DOM.Window (matchMedia)
import GHCJS.DOM.MediaQueryList (getMatches, removeListener)
import GHCJS.DOM.MediaQueryListListener
       (newMediaQueryListListenerAsync)

slugify :: Text -> Text
slugify = Text.replace "'" ""
        . Text.replace " " "-"
        . Text.toLower

now :: MonadJSM m => m DOMHighResTimeStamp
now = currentWindowUnchecked >>= getPerformance >>= Performance.now

attachTime :: (PerformEvent t m, MonadJSM (Performable m)) => Event t a -> m (Event t (DOMHighResTimeStamp, a))
attachTime e = performEvent $ ffor e $ \a -> (,a) <$> now

animationFrame
  :: (Monad m, MonadJSM m, Reflex t, TriggerEvent t m, MonadHold t m, MonadFix m, PerformEvent t m, MonadJSM (Performable m))
  => Dynamic t Bool
  -> m (Event t DOMHighResTimeStamp)
animationFrame sendEvents = do
  (e, send) <- newTriggerEvent
  stateChange <- updated <$> holdUniqDyn sendEvents
  sendEventsMVar <- liftIO newEmptyMVar
  ctx <- liftJSM askJSM
  _ <- liftIO . forkIO $ forever $ do
    takeMVar sendEventsMVar
    runJSM waitForAnimationFrame ctx >>= send
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
  => Event t DOMHighResTimeStamp
  -> DOMHighResTimeStamp
  -> Dynamic t Double
  -> m (Dynamic t Bool, Dynamic t Double)
transition frame duration input = do
  postBuild <- getPostBuild
  uniqInput <- holdUniqDyn input
  changes <- attachTime $ leftmost [ updated uniqInput, tagPromptlyDyn uniqInput postBuild ]
  let f (Right (tNew, vNew)) Nothing                       = Just (TransitionState tNew False vNew vNew vNew)
      f (Right (tNew, vNew)) (Just TransitionState{..})    = Just (TransitionState tNew True currentValue currentValue vNew)
      f (Left _)             Nothing                       = Nothing
      f (Left tNew)          (Just ts@TransitionState{..}) =
        let stillActive = tNew - startTime <= duration
        in Just ts
          { active = stillActive
          , currentValue = if stillActive
                then startValue + ((targetValue - startValue) * g (max 0 (tNew - startTime) / duration))
                else targetValue
          }
      g :: Double -> Double
      g x = let x2 = x * x
                x3 = x2 * x
            in 3 * x2 - 2 * x3

  transitionState :: Dynamic t (Maybe TransitionState) <- foldDyn f Nothing $ leftmost [Right <$> changes, Left <$> frame]
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
  x :: f (Dynamic t Bool, Dynamic t Double) <- mapM (transition frame duration) (commuteWith input)
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
-- makeLenses ''RGB

instance CommutesWithFunctor RGB where
  commuteWith input = RGB (_red <$> input) (_green <$> input) (_blue <$> input)

data Translate a = Translate
  { _x :: a
  , _y :: a
  } deriving (Show, Functor, Foldable, Traversable, Eq)

translateString :: Translate Double -> Text
translateString Translate{..} = "translate(" <> Text.pack (show _x) <> "," <> Text.pack (show _y) <> ")"

instance CommutesWithFunctor Translate where
  commuteWith input = Translate (_x <$> input) (_y <$> input)

data Scale a = Scale
  { _xScale :: a
  , _yScale :: a
  } deriving (Show, Functor, Foldable, Traversable, Eq)

scaleString :: Scale Double -> Text
scaleString Scale{..} = "scale(" <> Text.pack (show _xScale) <> "," <> Text.pack (show _yScale) <> ")"

transformString :: Translate Double -> Scale Double -> Text
transformString t s = translateString t <> " " <> scaleString s

instance CommutesWithFunctor Scale where
  commuteWith input = Scale (_xScale <$> input) (_yScale <$> input)

rgbString :: RGB Double -> Text
rgbString RGB{..} = "rgb(" <> str _red <> "," <> str _green <> "," <> str _blue <> ")"
  where str n = Text.pack $ show (round n :: Int)

data ZoomState a = ZoomState
  { _defaultBackground :: RGB a
  , _selectedRegionBackground :: RGB a
  , _defaultOutline :: RGB a
  , _sameRegionOutline :: RGB a
  , _coastline :: RGB a
  , _translate :: Translate a
  , _scale     :: Scale a
  , _strokeWidth :: a
  } deriving (Show, Functor, Foldable, Traversable, Eq)

instance CommutesWithFunctor ZoomState where
  commuteWith i = ZoomState
                    (commuteWith $ _defaultBackground <$> i)
                    (commuteWith $ _selectedRegionBackground <$> i)
                    (commuteWith $ _defaultOutline <$> i)
                    (commuteWith $ _sameRegionOutline <$> i)
                    (commuteWith $ _coastline <$> i)
                    (commuteWith $ _translate <$> i)
                    (commuteWith $ _scale <$> i)
                    (_strokeWidth <$> i)

zoomState :: Bool -> Bool -> Maybe Text -> ZoomState Double
zoomState wide z selectedRegion = ZoomState
    { _defaultBackground = if z then RGB 223 241 252 else RGB 41 101 117
    , _selectedRegionBackground = if z then RGB 41 101 117 else RGB 0 189 233
    , _defaultOutline    = if z then RGB 255 255 255 else RGB 255 255 255
    , _sameRegionOutline = if z then RGB 223 241 252 else RGB 41 101 117
    , _coastline         = if z then RGB 255 255 255 else RGB 41 101 117
    , _translate = translate
    , _scale = scale
    , _strokeWidth = strokeWidth
    }
  where
    (translate, scale, strokeWidth) =
      case selectedRegion of
        Just r | z -> regionTransform $ slugify r
        _ -> transform 0 727.5 1 (-1) 0.8
    transform x y sx sy sw = (Translate (x + if wide then 113 else 0) y, Scale sx sy, if z then 1.2/sx else 0.6)
    regionTransform "auckland"          = transform (-3080) 6370 10 (-10) 0.3
    regionTransform "bay-of-plenty"     = transform (-2050) 3240 5.5 (-5.5) 0.3
    regionTransform "canterbury"        = transform (-350) 1000 3 (-3) 0.55
    regionTransform "gisborne"          = transform (-2506) 3400 6 (-6) 0.45
    regionTransform "hawkes-bay"        = transform (-1800) 2550 5 (-5) 0.4
    regionTransform "manawatu-wanganui" = transform (-1250) 2330 4.5 (-4.5) 0.45
    regionTransform "marlborough"       = transform (-1400) 2348 6 (-6) 0.45
    regionTransform "nelson"            = transform (-1900) 3200 8 (-8) 0.35
    regionTransform "northland"         = transform (-1070) 3670 5 (-5) 0.4
    regionTransform "otago"             = transform (-160) 828 4 (-4) 0.53
    regionTransform "southland"         = transform 100 700 4 (-4) 0.53
    regionTransform "taranaki"          = transform (-2260) 4000 8 (-8) 0.35
    regionTransform "tasman"            = transform (-1080) 2346 6 (-6) 0.45
    regionTransform "waikato"           = transform (-1100) 2480 4 (-4) 0.53
    regionTransform "wellington"        = transform (-2250) 2900 7 (-7) 0.35
    regionTransform "west-coast"        = transform (-210) 1170 3 (-3) 0.6
    regionTransform _                   = transform 0 727.5 1 (-1) 0.8

data MapState = MapState
  { _zoom           :: Bool
  , _zoomState      :: ZoomState Double
  , _mouseAreaInfo  :: Maybe AreaInfo
  , _region         :: Maybe Text
  , _subarea        :: Maybe Text
  , _regionChildren :: [Text]
  } deriving (Show, Eq)

_zoomRegion :: MapState -> Maybe Text
_zoomRegion MapState{ _zoom = False } = Nothing
_zoomRegion MapState{ _region = r } = r

_selectedSubareaClass :: MapState -> Maybe Text
_selectedSubareaClass MapState{..} = (<> if _region == Just "auckland" then "-ward" else "-ta") <$> _subarea

aiRegionClass :: AreaInfo -> Maybe Text
aiRegionClass = fmap (slugify . (<> "-region")) . areaRegion

mouseOverRegionClass :: MapState -> Maybe Text
mouseOverRegionClass = (>>= aiRegionClass) . _mouseAreaInfo

aiSubareaClass :: AreaInfo -> Maybe Text
aiSubareaClass AreaInfo{..} = (slugify . (<> "-ward") <$> areaWard) <|> (slugify . (<> "-ta") <$> areaTa)

mouseOverSubareaClass :: MapState -> Maybe Text
mouseOverSubareaClass MapState{_mouseAreaInfo = ai} = ai >>= aiSubareaClass

forNodesSetAttribute :: (MonadJSM m, ToJSString name, ToJSString value) => NodeList -> name -> value -> m ()
forNodesSetAttribute nodeList name val = liftJSM $ do
  f <- eval ("(function(list, name, value) { for (var i = 0; i < list.length; i++) { list[i].setAttribute(name, value); }})" :: Text)
  void $ call f f (nodeList, name, val)

mediaQueryChange
  :: (Monad m, MonadJSM m, TriggerEvent t m)
  => MediaQueryList
  -> m (Event t Bool)
mediaQueryChange mediaQueryList = do
  ctx <- askJSM
  newEventWithLazyTriggerWithOnComplete $ \trigger -> (`runJSM` ctx) $ do
    listener <- newMediaQueryListListenerAsync . mapM_ $ getMatches >=> \m -> liftIO $ trigger m (return ())
    MQ.addListener mediaQueryList (Just listener)
    return . (`runJSM` ctx) $ MQ.removeListener mediaQueryList (Just listener)

mediaQueryDyn
  :: (Monad m, MonadJSM m, TriggerEvent t m, MonadHold t m, DOM.ToJSString queryString)
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
    => Areas
    -> Dynamic t State
    -> m (Event t Message)
nzmap areas state = mdo

  zoomD <- holdUniqDyn $ hasAdapter Mapzoom <$> state
  let selectedArea = stateArea <$> state

      regD = selectedArea >>= \case
            (r:_) -> return . Just $ areaId r
            _             -> return Nothing

      regChildrenD = selectedArea >>= \case
            (r:_) -> return $ areaChildren r
            _             -> return []
      subareaD = fmap areaId . getSubArea <$> state

  svgVisibilityD <- holdDyn ("style"=:"visibility: hidden;") $ mempty <$ loadedSvg
  let svgObjectAttrD = (("type"=:"image/svg+xml" <> "data"=:"/assets/map.svg" <> "class" =: "map") <>) <$> svgVisibilityD

  svgObject <- uncheckedCastTo HTMLObjectElement . _element_raw . fst <$> elDynAttr' "object" svgObjectAttrD (return ())
  svgBodyE :: Event t DOM.HTMLElement
     <- wrapDomEvent svgObject (`DOM.on` DOM.load) $ do
      svgDoc <- getContentDocument svgObject
      uncheckedCastTo HTMLElement <$> querySelectorUnsafe svgDoc ("svg" :: Text)

  wide <- mediaQueryDyn ("(min-width: 1025px)" :: Text)

  let duration = 500
  frame <- animationFrame zoomAnimating
  let zoomStateD = zoomState <$> wide <*> zoomD <*> regD
  (zoomAnimating, zoomStateT) <- transitions frame duration zoomStateD

  svgBodyD <- holdDyn Nothing (Just <$> svgBodyE)
  loadedSvg <- switchHold never =<< dyn (ffor svgBodyD $ \case
    Nothing -> return never
    Just svgBody -> do
      let forSelectionSetAttribute :: (MonadJSM m0) => Text -> Text -> Text -> m0 ()
          forSelectionSetAttribute q name value = do
            nodeList <- querySelectorAll svgBody q
            forNodesSetAttribute nodeList name value
          forSelectionSetAttributes :: (MonadJSM m0) => Text -> [(Text, Text)] -> m0 ()
          forSelectionSetAttributes q = mapM_ (uncurry $ forSelectionSetAttribute q)
          set :: (MonadJSM m0, IsElement e) => Text -> Text -> e -> m0 ()
          set a v e = setAttribute e a v
          setColour :: (MonadJSM m0) => Text -> Text -> m0 ()
          setColour cssClass colour = liftJSM $ do
            forSelectionSetAttribute ("g." <> cssClass <> " > path") "fill" colour
            forSelectionSetAttribute ("g." <> cssClass <> " > polyline") "stroke" colour
          mapStateD = MapState <$> zoomD <*> zoomStateT <*> mouseOverD <*> regD <*> subareaD <*> regChildrenD

      postBuild <- getPostBuild
      performEvent_ $ postBuild $> do
        set "height" "550px" svgBody
        forSelectionSetAttribute "path" "stroke" "none"
        forSelectionSetAttribute "path" "stroke-width" "0.001"
        forSelectionSetAttribute "path" "cursor" "pointer"
        forSelectionSetAttribute "polyline" "stroke-linecap" "butt"
        forSelectionSetAttribute "polyline" "cursor" "pointer"

      mapStateE <- attachPrevious (leftmost [updated mapStateD, tagPromptlyDyn mapStateD postBuild])
      performEvent . ffor mapStateE $ \(old, new) -> do
        -- Update the transform, but only if it has changed
        when ((_zoomState <$> old) /= Just (_zoomState new)) $ do
          let transform = transformString (_translate $ _zoomState new)
                                              (_scale $ _zoomState new)
          querySelector svgBody ("g" :: Text) >>= mapM_
            (set "transform" transform)

        let bg = rgbString . _defaultBackground $ _zoomState new
            srbg = rgbString . _selectedRegionBackground $ _zoomState new
            ol = rgbString . _defaultOutline $ _zoomState new
            srol = rgbString . _sameRegionOutline $ _zoomState new
            cl = rgbString . _coastline $ _zoomState new
            sw = _strokeWidth $ _zoomState new
            changed = if (_zoomState <$> old) /= Just (_zoomState new) || (_region <$> old) /= Just (_region new) || (_subarea <$> old) /= Just (_subarea new)
                        then ""
                        else maybe "" ("." <>) $ mouseOverRegionClass =<< old

        -- Reset the properties of the changed elements
        forSelectionSetAttribute ("g" <> changed <> " > polyline") "stroke-width" (Text.pack . show $ sw * 2)
        if _zoom new
          then
            forM_ (_region new) $ \r -> do
              let subareaType = if r == "auckland" then "ward" else "ta"
              forSelectionSetAttribute ("g" <> changed <> "[same_reg=TRUE] > polyline") "stroke-width" (Text.pack . show $ sw)
              forSelectionSetAttribute ("g" <> changed <> ".inbound[same_reg=FALSE] > polyline") "stroke" ol
              forSelectionSetAttribute ("g" <> changed <> ".inbound[same_reg=TRUE] > polyline") "stroke" srol
              forSelectionSetAttribute ("g" <> changed <> ".inbound[same_reg=TRUE][same_" <> subareaType <> "=TRUE] > polyline") "stroke-width" "1.0"
          else do
            forSelectionSetAttribute ("g" <> changed <> ".inbound[same_reg=FALSE] > polyline") "stroke" ol
            forSelectionSetAttributes ("g" <> changed <> ".inbound[same_reg=TRUE] > polyline") [("stroke", srol), ("stroke-width", "1.5")]
        forSelectionSetAttribute ("g" <> changed <> ".coastline > polyline") "stroke" cl
        forSelectionSetAttribute ("g" <> changed <> " > path") "fill" bg

        if _zoom new
          then do
            -- Clear old mouse over subarea that is outside the selected region (and so will not be included when we clear the selected region)
            forM_ (_region =<< old) $ \r -> do
              let regionClass = slugify r <> "-region"
              case _mouseAreaInfo =<< old of
                Just AreaInfo{..} | (slugify <$> areaRegion) == Just r ->
                  forM_ (mouseOverSubareaClass =<< old) $ \cssClass -> do
                    forSelectionSetAttribute ("g:not(." <> regionClass <> ")." <> cssClass <> " > path") "fill" bg
                    forSelectionSetAttribute ("g:not(." <> regionClass <> ")." <> cssClass <> " > polyline") "stroke" ol
                    forSelectionSetAttribute ("g:not(." <> regionClass <> ")." <> cssClass <> "[same_reg=TRUE] > polyline") "stroke" bg
                _ -> return ()

            forM_ (_region new) $ \r -> do
              let regionClass = slugify r <> "-region"
                  subareaType = if r == "auckland" then "ward" else "ta"

              -- Selected region
              forSelectionSetAttribute ("g." <> regionClass <> " > path") "fill" srbg
              forSelectionSetAttribute ("g.coastline." <> regionClass <> " > polyline") "stroke" "none"
              forSelectionSetAttribute ("g.inbound." <> regionClass <> "[same_" <> subareaType <> "=TRUE] > polyline") "stroke" srbg
              forSelectionSetAttribute ("g.inbound." <> regionClass <> "[same_reg=TRUE][same_" <> subareaType <> "=FALSE] > polyline") "stroke" "rgb(106,142,156)"
              forSelectionSetAttribute ("g.inbound." <> regionClass <> "[same_reg=FALSE][same_" <> subareaType <> "=FALSE] > polyline") "stroke" "none"

              -- Remainder of subareas with some part the seleced region
              forM_ (_regionChildren new) $ \child -> do
                forSelectionSetAttribute ("g:not(." <> regionClass <> ")." <> slugify child <> "-" <> subareaType <> " > path") "fill" "rgb(181,209, 223)"
                forSelectionSetAttribute ("g:not(." <> regionClass <> ")." <> slugify child <> "-" <> subareaType <> "[same_" <> subareaType <> "=TRUE][same_reg=TRUE] > polyline") "stroke" "rgb(181,209, 223)"
                forSelectionSetAttribute ("g:not(." <> regionClass <> ")." <> slugify child <> "-" <> subareaType <> "[same_" <> subareaType <> "=TRUE][same_reg=FALSE] > polyline") "stroke" ol

              -- New mouse over region
              forM_ (mouseOverRegionClass new) $ \cssClass -> when (cssClass /= regionClass) $ do
                let mouseOverZoomBackground = "rgb(174, 227, 248)"
                forSelectionSetAttribute ("g." <> cssClass <> " > path") "fill" mouseOverZoomBackground
                forSelectionSetAttribute ("g.inbound." <> cssClass <> "[same_reg=TRUE] > polyline") "stroke" "rgb(160,214,236)"
                forSelectionSetAttribute ("g.inbound." <> cssClass <> "[same_reg=TRUE][same_" <> subareaType <> "=TRUE] > polyline") "stroke" mouseOverZoomBackground

              -- Mouse over subarea
              case _mouseAreaInfo new of
                Just AreaInfo{..} | (slugify <$> areaRegion) == Just r ->
                  forM_ (mouseOverSubareaClass new) $ \cssClass -> do
                    forSelectionSetAttribute ("g." <> regionClass <> "." <> cssClass <> " > path") "fill" "rgb(0, 189, 233)"
                    forSelectionSetAttribute ("g.inbound." <> regionClass <> "." <> cssClass <> "[same_" <> subareaType <> "=TRUE] > polyline") "stroke" "rgb(0, 189, 233)"
                    forSelectionSetAttribute ("g:not(." <> regionClass <> ")." <> cssClass <> " > path") "fill" "rgb(174, 227, 248)"
                    forSelectionSetAttribute ("g.inbound:not(." <> regionClass <> ")." <> cssClass <> "[same_" <> subareaType <> "=TRUE][same_reg=TRUE] > polyline") "stroke" "rgb(174, 227, 248)"
                    forSelectionSetAttribute ("g.inbound:not(." <> regionClass <> ")." <> cssClass <> "[same_" <> subareaType <> "=TRUE][same_reg=FALSE] > polyline") "stroke" "rgb(0, 189, 233)"
                _ -> return ()

              -- Selected subarea
              forM_ (_selectedSubareaClass new) $ \cssClass -> do
                forSelectionSetAttribute ("g." <> regionClass <> "." <> cssClass <> " > path") "fill" "rgb(0, 189, 233)"
                forSelectionSetAttribute ("g.inbound." <> regionClass <> "." <> cssClass <> "[same_" <> subareaType <> "=TRUE] > polyline") "stroke" "rgb(0, 189, 233)"
                forSelectionSetAttribute ("g:not(." <> regionClass <> ")." <> cssClass <> " > path") "fill" "rgb(174, 227, 248)"
                forSelectionSetAttribute ("g.inbound:not(." <> regionClass <> ")." <> cssClass <> "[same_" <> subareaType <> "=TRUE][same_reg=TRUE] > polyline") "stroke" "rgb(174, 227, 248)"
                forSelectionSetAttribute ("g.inbound:not(." <> regionClass <> ")." <> cssClass <> "[same_" <> subareaType <> "=TRUE][same_reg=FALSE] > polyline") "stroke" "rgb(0, 189, 233)"
          else do
            forM_ (_region new) $ \r -> do
              forSelectionSetAttribute ("g." <> slugify r <> "-region > path") "fill" srbg
              forSelectionSetAttributes ("g." <> slugify r <> "-region[same_reg=TRUE] > polyline") [("stroke", srbg), ("stroke-width", "3.0")]
              forSelectionSetAttribute ("g." <> slugify r <> "-region.coastline > polyline") "stroke" srbg
            forM_ (mouseOverRegionClass new) $ \cssClass -> do
              forSelectionSetAttribute ("g." <> cssClass <> " > path") "fill" "rgb(0, 189, 233)"
              forSelectionSetAttributes ("g." <> cssClass <> "[same_reg=TRUE] > polyline") [("stroke", "rgb(0, 189, 233)"), ("stroke-width", "3.0")]
              forSelectionSetAttribute ("g." <> cssClass <> ".coastline > polyline") "stroke" "rgb(0, 189, 233)")

  let tooltipArea :: State -> Maybe (AreaInfo, (Int, Int)) -> Maybe ((AreaInfo, (Int, Int)), Area)
      tooltipArea s Nothing = Nothing
      tooltipArea s (Just (ai, xy)) =
        let maybeAreaId =
              if hasAdapter Mapzoom s
                then areaWard ai <|> areaTa ai
                else areaRegion ai
        in ((ai, xy),) <$> (((`OM.lookup` areas) . slugify) =<< maybeAreaId)
      tooltipAreaD :: Dynamic t (Maybe ((AreaInfo, (Int, Int)), Area))
      tooltipAreaD = tooltipArea <$> state <*> mouseOverFullD
      showStyle Nothing = "visibility:hidden;"
      showStyle (Just ((_, (x,y)), _)) = Text.pack $
          "visibility:visible; left:" <> show (x + 8) <> "px; top:" <> show (y + 8) <> "px;"
  elDynAttr "div" (("class" =: "tooltip" <>) . ("style" =:) . showStyle <$> tooltipAreaD) $
    el "p" $ dynText $ maybe "" (areaName . snd) <$> tooltipAreaD

  moveE :: Event t (Maybe (AreaInfo, (Int, Int))) <- switchHold never =<< dyn (ffor svgBodyD $ \case
    Just divElement ->
      wrapDomEvent divElement (`DOM.onSync` DOM.mouseMove) getAreaInfoFromSvg
    _ -> return never)
  clickE :: Event t (Maybe AreaInfo) <- switchHold never =<< dyn (ffor svgBodyD $ \case
    Just divElement ->
      fmap (fmap fst) <$> wrapDomEvent divElement (`DOM.on` DOM.click) getAreaInfoFromSvg
    _ -> return never)
  leaveE <- switchHold never =<< dyn (ffor svgBodyD $ \case
    Just divElement ->
      wrapDomEvent divElement (`DOM.onSync` DOM.mouseLeave) $ return ()
    _ -> return never)

  mouseOverFullD :: Dynamic t (Maybe (AreaInfo, (Int, Int))) <- holdDyn Nothing $ leftmost [moveE , Nothing <$ clickE, Nothing <$ leaveE]
  mouseOverD :: Dynamic t (Maybe AreaInfo) <- holdUniqDyn $ fmap fst <$> mouseOverFullD

  -- The click event depends on the state
  let makeMessages :: State -> Maybe AreaInfo -> Maybe Message
      makeMessages st ai =
        let region = slugify <$> (areaRegion =<< ai)
            ta = slugify <$> (areaTa =<< ai)
            ward = slugify <$> (areaWard =<< ai)
            currentRegion = areaId <$> getRegion st
            currentSubarea = areaId <$> getSubArea st
            iszoomed = hasAdapter Mapzoom st
            auckland = "auckland" :: Text
            subarea = if region == Just auckland then ward else ta
        in  if
            | currentRegion == region && not iszoomed
                -> Just ZoomIn
            | currentRegion /= region && isJust region
                -> Just (SetRegion (fromJust region))
            | currentRegion /= region && isJust currentRegion && isJust currentSubarea
                -> Just (SetRegion (fromJust currentRegion))
            | currentSubarea /= subarea && iszoomed && isJust subarea
                -> Just (SetSubArea $ fromJust subarea)
            | isNothing region && iszoomed
                -> Just (ZoomOut region)
            | otherwise
                -> Nothing

  return $ attachPromptlyDynWithMaybe makeMessages state clickE

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
          ta  <- getAttr (if isPath then "ta" else "ta1")
          wrd <- getAttr (if isPath then "wrd" else "wrd1")
          (x, y) <- mouseClientXY
          return $ Just (AreaInfo reg ta wrd, (x, y))

