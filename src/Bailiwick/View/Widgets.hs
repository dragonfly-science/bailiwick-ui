{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
module Bailiwick.View.Widgets
where

import Control.Monad (guard)
import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Read as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap


import Reflex
import Reflex.Dom.Core



selectWidget
    :: forall k t m. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k)
    => k -> Dynamic t (Map k Text)
    -> Dynamic t (Map Text Text)
    -> Event t k
    -> m (Dynamic t k, Event t k)
selectWidget k0 options attrs setK = do

  optionsWithAddedKeys :: Dynamic t (Map k Text)
    <- fmap (zipDynWith Map.union options)
        $ foldDyn Map.union (k0 =: "") $ fmap (=: "") setK

  defaultKey :: Dynamic t k <- holdDyn k0 setK

  let indexedOptions :: Dynamic t (Map (Int, k) Text)
      ixKeys :: Dynamic t (Bimap Int k)
      (indexedOptions, ixKeys) = splitDynPure $ ffor optionsWithAddedKeys $ \os ->
        let xs = fmap (\(ix, (k, v)) -> ((ix, k), ((ix, k), v))) $ zip [0::Int ..] $ Map.toList os
        in (Map.fromList $ map snd xs, Bimap.fromList $ map fst xs)
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = def
        & selectElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
        & selectElementConfig_setValue .~ fmap (T.pack . show) (attachPromptlyDynWithMaybe (flip Bimap.lookupR) ixKeys setK)
  (eRaw, _) <- selectElement cfg $ listWithKey indexedOptions $ \(ix, k) v -> do
    let optionAttrs = fmap (\dk -> "value" =: T.pack (show ix) <> if dk == k then "selected" =: "selected" else mempty) defaultKey
    elDynAttr "option" optionAttrs $ dynText v
  let lookupSelected ks v = do
        key <- T.readMaybe $ T.unpack v
        Bimap.lookup key ks
  let eChange = attachPromptlyDynWith lookupSelected ixKeys $ _selectElement_change eRaw
  let readKey keys mk = fromMaybe k0 $ do
        k <- mk
        guard $ Bimap.memberR k keys
        return k
  dValue <- fmap (zipDynWith readKey ixKeys) $ holdDyn (Just k0) $ leftmost [eChange, fmap Just setK]
  return (dValue, attachPromptlyDynWith readKey ixKeys eChange)

