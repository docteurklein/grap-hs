{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Graphs

prop_test :: Property
prop_test = property $ do
  "Graphs" === "Graphs"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
