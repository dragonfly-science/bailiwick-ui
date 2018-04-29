{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE FlexibleContexts    #-}
module Bailiwick.View.Map
where

import Data.Monoid ((<>))

import Reflex.Dom.Core

import Bailiwick.State (State(..), Message(..))


nzmap
    :: ( Monad m
       , DomBuilder t m
       )
    => Dynamic t State -> m (Event t Message)
nzmap _state = do
  elAttr "object" ( "type" =: "image/svg+xml" <> "data" =: "/assets/map.svg" ) $
    text ""
  return never
