{-# LANGUAGE OverloadedStrings   #-}

module Bailiwick.View
where

import Data.Text.Encoding (decodeUtf8)

import Reflex.Dom.Core

import Bailiwick.State (State(..), Message(..))

view :: ( Monad m
        , DomBuilder t m
        )
     => State -> m (Event t Message)
view (State path) = do
    el "h1" (text "hello world")
    el "span" (text (decodeUtf8 path))
    el "br" $ return ()
    (e, _) <- el' "button" $ text "Click"
    return $ SetPath "/lets/go/here" <$ domEvent Click e
    
