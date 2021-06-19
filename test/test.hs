{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Prelude
import Test.DocTest

main :: IO ()
main =
  doctest
  [
    "src/Ephemeral/Market.hs",
    "src/Ephemeral/Shekel.hs",
    "src/Ephemeral/Chart.hs",
    "src/Ephemeral/Ellipse.hs"
  ]
