{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bailiwick.Devel (
    test
  , debug
) where

------------------------------------------------------------------------------
import Data.Monoid ((<>))
import Reflex.Dom.Core
------------------------------------------------------------------------------

import Bailiwick (ui)

#ifndef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Warp (jsaddleOr, debugWrapper, jsaddleJs)
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.Wai.Middleware.RequestLogger
import Network.WebSockets (defaultConnectionOptions)
import Language.Javascript.JSaddle (syncPoint, JSM)
import qualified Network.Wai as W
       (responseLBS, pathInfo, requestMethod)
import qualified Network.HTTP.Types as H (status200)

import Bailiwick.Application (application)


debug :: Int -> JSM () -> IO ()
debug prt f = do
  app <- application  "/jsaddle.js"
  debugWrapper $ \withRefresh registerContext ->
    runSettings (setPort prt (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions 
                (registerContext >> f >> syncPoint) 
                (withRefresh $ \req sendResponse -> 
          case (W.requestMethod req, W.pathInfo req) of
              ("GET", ["jsaddle.js"]) ->
                       sendResponse
                      $ W.responseLBS H.status200 
                          [("Content-Type", "application/javascript")] 
                      $ jsaddleJs True
              _ -> logStdoutDev app req sendResponse)
  putStrLn $ "<a href=\"http://localhost:" <> show prt <> "\">run</a>"
#else
import Language.Javascript.JSaddle (JSM)

debug :: Int -> JSM () -> IO ()
debug _ = id
#endif


test :: JSM ()
test = mainWidget ui

-- > System.Process.system "xyleap-start-tempdb"
-- > System.Process.system "../db/get-database-from-kahu.sh"
-- > XYLeap.Devel.debug 3706 XYLeap.Devel.test
-- try putting this into the repl to force reload of app
-- :def! reload (const $ return "::reload\nBailiwick.Devel.debug 3705 test")


