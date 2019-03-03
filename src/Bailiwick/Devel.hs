{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings   #-}
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
       (responseLBS, pathInfo, requestMethod, requestHeaderHost)
import qualified Network.HTTP.Types as H (status200)

import Bailiwick.Application (application, indexHtml)
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Network.Wai.Application.Static
       (defaultFileServerSettings, staticApp)
import System.Process (system)
import System.Directory (copyFile)

#if MIN_VERSION_ghcjs_dom(0,9,4)
import GHCJS.DOM.Debug (addDebugMenu)
#else
addDebugMenu :: JSM ()
addDebugMenu = return ()
#endif

staticfiles :: FilePath
staticfiles = "dist-ghcjs/build/x86_64-linux/ghcjs-8.4.0.1/bailiwick-0.1.0.0/x/bailiwick/build/bailiwick/bailiwick.jsexe/"

debug :: Int -> JSM () -> IO ()
debug prt f = do
  _ <- system "nix-build ./css"
  copyFile "result/bailiwick-pre.css" "static/teal-skua.css"
  app <- application "/jsaddle.js"
  let ghcjsFiles = ["rts.js", "lib.js", "out.js", "runmain.js"]
  debugWrapper $ \withRefresh registerContext ->
    runSettings (setPort prt (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions
                (registerContext >> addDebugMenu >> f >> syncPoint)
                (withRefresh $ \req sendResponse ->
          case (W.requestMethod req, W.pathInfo req) of
              ("GET", ["jsaddle.js"]) ->
                  sendResponse
                      $ W.responseLBS H.status200
                          [("Content-Type", "application/javascript")]
                      $ jsaddleJs True
              ("GET", [ghcjsFile]) |
                  ghcjsFile `elem` ghcjsFiles ->
                    staticApp (defaultFileServerSettings staticfiles)
                      req sendResponse
              ("GET", x) | (null x || head x `elem` ["summary", "theme"])
                         && W.requestHeaderHost req /=
                             Just ("jsaddle.localhost:"
                                  <> T.encodeUtf8 (T.pack $ show prt)) ->
                  sendResponse . W.responseLBS H.status200
                                 [("Content-Type", "text/html")]
                  =<< indexHtml (map ((LBS.fromStrict . T.encodeUtf8) . ("/" <>)) 
                                 ghcjsFiles)
              _ -> logStdoutDev app req sendResponse)
  putStrLn $ "<a href=\"http://localhost:" <> show prt <> "\">run</a>"
#else
import Language.Javascript.JSaddle (JSM)

debug :: Int -> JSM () -> IO ()
debug _ = id
#endif

test :: JSM ()
test = mainWidget ui

-- > Bailiwick.Devel.debug 3706 Bailiwick.Devel.test
-- try putting this into the repl to force reload of app
-- :def! reload (const $ return "::reload\nBailiwick.Devel.debug 3705 test")


