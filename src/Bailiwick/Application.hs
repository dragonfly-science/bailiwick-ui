{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Bailiwick.Application
  ( application
  , indexHtml
  )
where

import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as LB
       (fromStrict, ByteString)
import Data.String.Interpolation

#ifndef ghcjs_HOST_OS
import qualified Network.Wai as W
       (responseLBS, pathInfo, requestMethod, Application, Request)
import qualified Network.HTTP.Types as H (status200)
import Network.Wai.Application.Static
       (staticApp, defaultWebAppSettings)
#endif

import Reflex.Dom.Core

uiStatic
    :: ( DomBuilder t m
       , PostBuild t m)
    => m ()
uiStatic =
  divClass "section columns" $
    elAttr "div" ( "class" =: "container box column home-page is-half"
                 <> "id" =: "content-wrapper") $
      text "Loading..."

indexHtml :: [LB.ByteString] -> IO LB.ByteString
indexHtml jss = do
  body <- LB.fromStrict . snd <$> renderStatic uiStatic
  return [str|
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <title>Regional economic activity report</title>
    <meta name="description" content="">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <link rel="stylesheet" href="/vendor.css">
    <link rel="stylesheet" href="/teal-skua.css">

    <script src="/modernizr.custom.30140.js"></script>
    <script src="/iframeResizer.contentWindow.min.js"></script>
    <script src="https://d3js.org/d3.v3.min.js"></script>
    <script src="/bailiwick.js"></script>
    <script src="/bailiwick.bundle.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/lodash@4.17.11/lodash.min.js"></script>
    <script src="/perfect-scrollbar.js"></script>
  </head>
  <body>
    $body$
    $ mconcat (map (\js -> "<script src=\"" <> js <> "\"></script>") jss) $
  </body>
  </html>

|]


#ifndef ghcjs_HOST_OS

checkPath :: W.Request -> Bool
checkPath req =
    let path = W.pathInfo req
    in case path of
        []            -> True
        ("summary":_) -> True
        ("theme":_)   -> True
        _             -> False



application :: LB.ByteString -> IO W.Application
application js = do
    loadingPage <- indexHtml [js]
    return $ \req sendResponse
       -> case (W.requestMethod req, checkPath req) of
            ("GET", True) ->
                sendResponse $
                    W.responseLBS H.status200 [("Content-Type", "text/html")]
                    loadingPage
            _ -> staticApp (defaultWebAppSettings "static") req sendResponse

#else
application :: String
application = "Not defined"

#endif


