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
    => State -> m (Event t Message)
header state = do

  ready <- getPostBuild
  areasE <- getAreas ready
  areasD <- holdDyn [] $ fmap areas $ fmapMaybe reqSuccess areasE

  let mkRegions :: [Area] -> Map Text Text
      mkRegions as = Map.fromList [ (Bailiwick.Types.id a, name a) 
                                  | a <- as
                                  , level a == "reg" ]  
  let regionsD = mkRegions <$>  areasD

  let initialRegion = case state of 
         Summary reg -> Just reg
         _           -> Nothing
      lookupReg :: Map Text Text -> Text
      lookupReg regs = fromMaybe "New Zealand" $ do
                         ini <- initialRegion
                         Map.lookup ini regs

  elAttr "div" (  "class" =: "title" 
               <> "data-region" =: fromMaybe "new-zealand" initialRegion) $
    divClass "content" $ mdo
      divClass "left" $ do
        elClass "span" "block-label context-text" $ text "You're looking at"
        divClass "page-header summary-page-header" $ do
          el "div" $ dynText (lookupReg <$> regionsD)
          el "div" $ return ()
      region
        <- divClass "right" $ do
             divClass "title-menus" $ do
               dropdownMenu initialRegion  regionsD
      return $ SetRegion <$> updated region
                

firstKey :: Map Text Text -> Text
firstKey = fromMaybe "" . fmap fst . listToMaybe . Map.toList

dropdownMenu
    :: ( Monad m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , DomBuilder t m
       )
    => Maybe Text -> Dynamic t (Map Text Text) -> m (Dynamic t Text)
dropdownMenu start regionsD = do
  divClass "dropdown" $ do
    divClass "dropdown-container" $ mdo
    
      currentValue :: Dynamic t Text
        <- holdDyn (fromMaybe "new-zealand" start) (firstKey <$> selectedRegion)

      open :: Dynamic t Bool
        <- holdDyn False $
            leftmost [ (not <$> tag (current open) (domEvent Click p))
                     , False <$ selectedRegion ]
   
      let label = do 
            val <- currentValue
            if val == "new-zealand"
                 then return "Select a region"
                 else do
                     regions <- regionsD
                     return $ fromMaybe "not found ?" $ Map.lookup val regions

      (p, _) <-  elClass' "p" "dropdown-button" $ dynText label

      let ulClass = ffor open $ \isOpen ->
              if isOpen then "dropdown-menu dropdown-select show-menu"
                        else "dropdown-menu dropdown-select"
      selectedRegion :: Event t (Map Text Text)
        <- elDynClass "ul" ulClass $ do
             listViewWithKey regionsD $ \_k v -> do
               (li, _) <- el' "li" $ dynText v
               return (tag (current v) (domEvent Click li))

      return currentValue


