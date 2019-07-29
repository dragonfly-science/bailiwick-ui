{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Bailiwick.View.ExportMenu
  ( exportMenu
  )
where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)

import Language.Javascript.JSaddle (liftJSM, MonadJSM, eval)
import Reflex.Dom.Core

import Bailiwick.Route


exportMenu
  :: ( MonadFix m
     , MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => Event t Modal
  -> m (Event t Message)
exportMenu openE = mdo

  let clickE = fmap (domEvent Click . fst)

  modeD <- holdDyn Nothing $
                leftmost [ Just    <$> openE
                         , Nothing <$  closeE
                         , Just    <$> selectE
                         ]

  let showCssD css = do
        mode <- modeD
        case mode of
          Just _  -> return $ css <> ("style" =: "display:flex")
          Nothing -> return $ css <> ("style" =: "display:none")

      selectClassD clss = do
        mode <- modeD
        case mode of
          Just Embed    -> return $ clss <> " embed-menu"
          Just Download -> return $ clss <> " download-menu"
          Just Share    -> return $ clss <> " share-menu"
          Nothing       -> return $ clss

  (exportE, closeE, selectE) <-
    elDynAttr "div" (showCssD ("class" =: "export-dialog")) $ do
      (closeE', selectE') <-
        divClass "export-dialog-main" $ do
          (closeE'', selectE'') <-
            divClass "export-header" $ do
              selectE''' <-
                divClass "export-menu" $ do
                  selectShareE <- clickE $
                    elClass' "span" "export-type" $ text "Share"
--                  selectEmbedE <- clickE $
--                    elClass' "span" "export-type" $ text "Embed"
--                  selectDownloadE <- clickE $
--                    elClass' "span" "export-type" $ text "Download"
                  return $ leftmost [ Share    <$ selectShareE]
--                                    , Embed    <$ selectEmbedE
--                                    , Download <$ selectDownloadE
--                                    ]
              closeE''' <- clickE $
                elClass' "span" "export-close" $ return ()
              return (closeE''', selectE''')
          elDynClass "div" (selectClassD "export-body") $ do
            shareMenu    (maybe False (== Share) <$> modeD)
            embedMenu    (maybe False (== Embed) <$> modeD)
            downloadMenu (maybe False (== Download) <$> modeD)
          return (closeE'', selectE'')

      return (never, closeE', selectE')

  return $ exportE


shareMenu
  :: ( MonadFix m
     , MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => Dynamic t Bool
  -> m ()
shareMenu showD = do
  let clickE = fmap (domEvent Click . fst)

      showCssD css = ffor showD $ \case
          True  -> css <> ("style" =: "display:flex")
          False -> css <> ("style" =: "display:none")

  elDynAttr "div" (showCssD ("class" =: "extra-wrapper")) $ do
    divClass "export-detail" $
      divClass "share-text" $ return ()
    divClass "export-controls" $
      divClass "social-share" $ do
        text "Share this on:"
        -- TODO: Needs a share event - opens twitter share URL.
        divClass "share-twitter" $ do
          el "i" $ return ()
          text "Twitter"
          return ()
        -- TODO: Needs an event - opens modal to share in FB.
        divClass "share-facebook" $ do
          el "i" $ return ()
          text "Facebook"
          return ()
  elDynAttr "div" (showCssD ("class" =: "extra-wrapper")) $ mdo

    currentUrlE <-
      performEvent ((liftJSM $ getLocationUrl) <$ ffilter (==True) (updated showD))
    currentUrlD <- holdDyn "" currentUrlE

    performEvent_ $ ffor (tagPromptlyDyn currentUrlD copyE) $ \url -> do
      void $ liftJSM $ eval $ "clipboard.copy('" <> url <> "')"

    divClass "export-detail" $ do
      let attrD = do
            currentUrl <- traceDyn "currentUrlD" currentUrlD
            return ("value" =: currentUrl <> "class" =: "share-url")
      elDynAttr "input" attrD $ return ()
    copyE <- clickE $
      divClass "export-controls" $
        -- TODO: Needs event to copy URL
        elClass' "div" "share-copy" $
          text "Copy this link to clipboard"
    return ()


embedMenu
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t Bool
  -> m ()
embedMenu showD = do
  let showCssD css = ffor showD $ \case
          True  -> css <> ("style" =: "display:flex")
          False -> css <> ("style" =: "display:none")

  elDynAttr "div" (showCssD ("class" =: "embed-output embed-preview export-detail")) $ return ()
    -- TODO: this needs to resize I think on window resize
    -- events.
--        elAttr "iframe" (
--          "src" =: "/theme/tourism-spend/map/treemap/2018/new-zealand/northland/?embed=dynamic&amp;left-zoom=1&amp;right-transform=absolute&amp;preview=600" <>
--          "frameborder" =: "0" <>
--          "scrolling" =: "no" <>
--          "marginheight" =: "0" <>
--          "marginwidth" =: "0" <>
--          "width" =: "600" <>
--          "id" =: "iFrameResizer1" <>
--          "style" =: "overflow: hidden; height: 1671px; transform: translateX(106px) scale(0.239378); visibility: inherit;"
--        ) $ return ()
      -- return ()
  elDynAttr "div" (showCssD ("class" =: "embed-controls export-controls")) $ do
    el "div" $ do
      divClass "embed-labels" $ text "Customise the view"
      divClass "embed-components" $ do
        -- TODO: emebed-component requires an event to check/uncheck the input field.
        divClass "embed-component" $ do
          elAttr "input" ("type" =: "checkbox") $ return ()
          el "div" $ do
            el "i" $ return ()
            text "Show map"
        divClass "embed-component" $ do
          elAttr "input" ("type" =: "checkbox") $ return ()
          el "div" $ do
            el "i" $ return ()
            text "Show chart"
        divClass "embed-component" $ do
          elAttr "input" ("type" =: "checkbox") $ return ()
          el "div" $ do
            el "i" $ return ()
            text "Show numbers"
      divClass "selector" $
        divClass "selector-container" $ do
          -- TODO: requires a dynamic to change text based upon selection below & to add/remove
          -- the class "show-menu" to the ul below.
          elClass "p" "selector-button" $ text "Width: Medium"
          elClass "ul" "selector-menu selector-select" $ do
            -- TODO: li require events to change text above & change width of iframe.
            el "li" $ text "Narrow"
            el "li" $ text "Medium"
            el "li" $ text "Wide"
    el "div" $ do
      elAttr "textarea" (
          "readonly" =: "" <>
          "spellcheck" =: "false" <>
          "rows" =: "6" <>
          "class" =: "embed-snippet"
        ) $ text "&lt;iframe src=\"http://webrear.mbie.govt.nz/theme/tourism-spend/map/treemap/2018/new-zealand/northland/?embed=dynamic&amp;left-zoom=1&amp;right-transform=absolute\" frameborder=\"0\" scrolling=\"no\" marginheight=\"0\" marginwidth=\"0\" width=\"600\" height=\"1671\"&gt;&lt;/iframe&gt;"
      divClass "snippet-copy" $ text " &lt;/&gt; Copy html to clipboard"

downloadMenu
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t Bool
  -> m ()
downloadMenu showD = do
  let showCssD css = ffor showD $ \case
          True  -> css <> ("style" =: "display:flex")
          False -> css <> ("style" =: "display:none")

  elDynAttr "div" (showCssD ("class" =: "extra-wrapper")) $ do
    divClass "export-detail" $ do
      divClass "download-text" $ do
        el "p" $ do
          text "For the selected indicator "
          el "span" $ text "tourism spend"
          text " and the selected area "
          el "span" $ text "New Zealand"
          text " there are 4 data downloads available."
        el "p" $ do
          el "i" $ text "Tourism spend"
          text "is all the original data and the derived values used to create visualisations for the tourism spend indicator."
    divClass "export-controls download-controls" $ do
      divClass "data-labels" $ text "Select the data"
      divClass "embed-components" $ do
        -- TODO: need events to check the input
        divClass "embed-component" $ do
          elAttr "input" ("type" =: "radio" <> "value" =: "indicator") $ return ()
          el "div" $ do
            el "i" $ return ()
            text "Tourism Spend"
          el "div" $ text "813 kB"
        divClass "embed-component" $ do
          elAttr "input" ("type" =: "radio" <> "value" =: "indicator") $ return ()
          el "div" $ do
            el "i" $ return ()
            text "New Zealand"
          el "div" $ text "147 kB"
        divClass "embed-component" $ do
          elAttr "input" ("type" =: "radio" <> "value" =: "indicator") $ return ()
          el "div" $ do
            el "i" $ return ()
            text "All original data"
          el "div" $ text "2,371 kB"
        divClass "embed-component" $ do
          elAttr "input" ("type" =: "radio" <> "value" =: "indicator") $ return ()
          el "div" $ do
            el "i" $ return ()
            text "All data"
          el "div" $ text "10,443 kB"
  elDynAttr "div" (showCssD ("class" =: "extra-wrapper")) $ do
    divClass "export-detail" $ do
      divClass "copyright" $ do
        divClass "icons" $ do
          divClass "cc-cc" $ return ()
          divClass "cc-by" $ return ()
        elAttr "a" (
          "target" =: "_blank" <>
          "href" =: "http://creativecommons.org/licenses/by/4.0/") $ text "Creative Commons Attribution 4.0 International Licence"
    divClass "export-controls download-controls" $
      -- TODO: needs event to download selected file.
      divClass "download-button" $ text "Download file to desktop"



