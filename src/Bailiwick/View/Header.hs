{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
module Bailiwick.View.Header
where

import Control.Monad.Fix
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, listToMaybe)

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Servant.Reflex
import Reflex.Dom.Core

import Bailiwick.State (State(..), Message(..))
import Bailiwick.Types
import Bailiwick.Store (getAreas)


header 
    :: ( Monad m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       , SupportsServantReflex t m
       )
    => Dynamic t State -> m (Event t Message)
header state = mdo

  ready <- getPostBuild
  areasE <- getAreas ready
  areasD <- holdDyn [] $ fmap areas $ fmapMaybe reqSuccess areasE

  let regionsD = mkRegions <$> areasD
      urlRegion = getRegion <$> state

      background = do
        reg <- urlRegion
        return $ (  "class" =: "title" <> "data-region" =: reg)

      dispRegion = do 
        this <- urlRegion
        regions <- regionsD
        return $ fromMaybe "New Zealand" $ Map.lookup this regions

  region <- 
    elDynAttr "div" background $
      divClass "content" $ mdo
        divClass "left" $ do
          elClass "span" "block-label context-text" $ text "You're looking at"
          divClass "page-header summary-page-header" $ do
            el "div" $ dynText dispRegion
            el "div" $ return ()
        divClass "right" $ do
          divClass "title-menus" $ do
            dropdownMenu urlRegion regionsD

  return $ SetRegion <$> updated region

  where

    mkRegions :: [Area] -> Map Text Text
    mkRegions as = Map.fromList [ (Bailiwick.Types.id a, name a) 
                                | a <- as
                                , level a == "reg" ]  

    getRegion :: State -> Text
    getRegion (Summary reg) = reg
    getRegion _ = "new-zealand"
      
                

dropdownMenu
    :: ( Monad m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => Dynamic t Text -> Dynamic t (Map Text Text) -> m (Dynamic t Text)
dropdownMenu urlRegion regionsD = do
  divClass "dropdown" $ do
    divClass "dropdown-container" $ mdo
    
      currentValue :: Dynamic t Text
        <- holdDyn "new-zealand" (firstKey <$> selectedRegion)

      open :: Dynamic t Bool
        <- holdDyn False $
            leftmost [ (not <$> tag (current open) (domEvent Click p))
                     , False <$ selectedRegion ]
   
      let label = do 
            val <- urlRegion
            if val == "new-zealand"
              then return "Select a region"
              else do
                regions <- regionsD
                return $ fromMaybe "not found ?" $ Map.lookup val regions

          ulClass = ffor open $ \isOpen ->
              if isOpen then "dropdown-menu dropdown-select show-menu"
                        else "dropdown-menu dropdown-select"

      (p, _) <-  elClass' "p" "dropdown-button" $ dynText label
      selectedRegion :: Event t (Map Text Text)
        <- elDynClass "ul" ulClass $ do
             listViewWithKey regionsD $ \_k v -> do
               (li, _) <- el' "li" $ dynText v
               return (tag (current v) (domEvent Click li))

      return currentValue

  where
    firstKey :: Map Text Text -> Text
    firstKey = fromMaybe "" . fmap fst . listToMaybe . Map.toList


