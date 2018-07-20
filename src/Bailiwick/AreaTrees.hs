{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bailiwick.AreaTrees (
   FT(..)
 , AreaTree(..)
 , AreaTrees
 , mkAreaTrees
) where

import Data.Aeson
       ((.:), withObject, FromJSON(..), Options(..), defaultOptions,
        genericParseJSON, Value(..))
import GHC.Generics (Generic)
import Data.Text (Text)
import Bailiwick.Types (IndicatorId(..))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OMap (fromList)
import qualified Data.Vector as V (toList)

data FT = FB { name :: Text, children :: [FT] }
        | FL { name      :: Text
             , slug      :: Text
             , value     :: Double
             , dispValue :: Text }
        deriving(Show, Eq, Generic)

ftOptions :: Options
ftOptions = defaultOptions
    { omitNothingFields = True }

instance FromJSON FT where
    parseJSON = genericParseJSON ftOptions

data AreaTree = AreaTree
    { indicator :: IndicatorId
    , year      :: Text
    , areas     :: FT
    } deriving(Show, Eq, Generic)

instance FromJSON AreaTree where
    parseJSON = genericParseJSON defaultOptions
    parseJSONList = withObject "AreaTrees" $ \v -> do
      Array as <- v .: "areaTrees"
      mapM parseJSON $ V.toList as

type AreaTrees = InsOrdHashMap IndicatorId AreaTree

mkAreaTrees :: [ AreaTree ] -> AreaTrees
mkAreaTrees areaTrees = OMap.fromList [(indicator i, i) | i <- areaTrees]
