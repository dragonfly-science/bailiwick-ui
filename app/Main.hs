module Main (
    main
) where

import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core (mainWidget)

import Bailiwick (ui)

main :: IO ()
main = run 3704 $ mainWidget ui

