{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
) where

#ifdef MIN_VERSION_jsaddle_wkwebview
import Language.Javascript.JSaddle.WKWebView (run, runFile, runHTMLWithBaseURL)
#else
import Language.Javascript.JSaddle.Warp (run)
#endif
import Reflex.Dom.Core (mainWidget)

import Bailiwick (ui)
import Data.Default (Default(..))
import qualified Data.ByteString as BS (pack)
import System.Directory (getCurrentDirectory)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text as T (pack)
import Data.Monoid ((<>))
import Bailiwick.Application (indexHtml)
import qualified Data.ByteString.Lazy as LBS (toStrict)

main :: IO ()
main = do
  dir <- getCurrentDirectory
  html <- indexHtml []
#ifdef MIN_VERSION_jsaddle_wkwebview
  runHTMLWithBaseURL (LBS.toStrict html) "http://localhost:3701/" def $
#else
  run 3704 $
#endif
    mainWidget ui

