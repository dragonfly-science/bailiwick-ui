{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
) where

#ifdef MIN_VERSION_jsaddle_wkwebview
import Data.Default(Default(..))
import qualified Data.ByteString.Lazy as LBS (toStrict)
import Language.Javascript.JSaddle.WKWebView (run, runFile, runHTMLWithBaseURL)
#else
import Language.Javascript.JSaddle.Warp (run)
#endif
import Reflex.Dom.Core (mainWidget)

import Bailiwick (ui)

main :: IO ()
main = do
#ifdef MIN_VERSION_jsaddle_wkwebview
  runHTMLWithBaseURL (LBS.toStrict html) "http://localhost:3701/" def $
#else
  run 3704 $
#endif
    mainWidget ui

