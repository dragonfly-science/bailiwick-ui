{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Bailiwick.Application
  ( application
  )
where

import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as LB
       (fromStrict, ByteString)

import qualified Network.Wai as W
       (responseLBS, pathInfo, requestMethod, Application)
import qualified Network.HTTP.Types as H (status200)

import Reflex.Dom.Core

uiStatic
    :: ( DomBuilder t m
       , PostBuild t m)
    => m ()
uiStatic = do
  divClass "section columns" $
    elAttr "div" ( "class" =: "container box column home-page is-half" 
                 <> "id" =: "content-wrapper") $
      text "Loading..."

indexHtml :: LB.ByteString -> IO LB.ByteString
indexHtml js = do
  body <- LB.fromStrict . snd <$> renderStatic uiStatic
  return $ mconcat
      [ "<!DOCTYPE html>"
      , "<html lang=\"en\" class=\"has-navbar-fixed-top\">"

      , "<head>"
      ,   "<meta charset=\"utf-8\">"
      ,   "<title>Bailiwick</title>"
      ,   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
      , "</head>"

      , "<body id=\"body\">"
      ,   body
      ,   "<script src='" <> js <> "'></script>"
      , "</body>"

      , "</html>"
      ]

application :: LB.ByteString -> IO W.Application
application js = do
    loadingPage <- indexHtml js
    return $ \req sendResponse ->
                case (W.requestMethod req, W.pathInfo req) of
                    ("GET", _) -> sendResponse $ W.responseLBS H.status200 [("Content-Type", "text/html")] loadingPage


