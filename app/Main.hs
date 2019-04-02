{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Spock
import           Web.Spock.Config

type Server a = SpockM () () () a

app :: Server ()
app = get root (text "Mello")

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 9000 (spock cfg app)
