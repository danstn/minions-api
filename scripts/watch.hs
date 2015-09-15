#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main :: IO ()
main = do
  stdout (runWatch $ findSrc)

findSrc :: Shell Text
findSrc = inproc "find" ["./src"] ""

runWatch :: Shell Text -> Shell Text
runWatch s = inproc "entr" ["cabal", "build"] s
