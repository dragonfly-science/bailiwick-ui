{-# LANGUAGE OverloadedStrings   #-}

module Bailiwick.View
where

import Reflex.Dom.Core

import Bailiwick.State (State, Message)

view :: ( Monad m
        , DomBuilder t m
        )
     => State -> m (Event t Message)
view _state = do
    el "h1" (text "hello world")
    return never

