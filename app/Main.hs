module Main (main) where

import Lib (runBot)

main :: IO ()
main = runBot "app-db-config.env"
