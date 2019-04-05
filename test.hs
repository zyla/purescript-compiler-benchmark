#!/usr/bin/env stack
{- stack --resolver lts-13.1 script --package turtle --package clock -}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Turtle
import qualified Turtle.Pattern as P
import qualified System.Clock as Clock

invalidateSources :: IO ()
invalidateSources = sh $ find (P.ends ".purs") ".psc-package" >>= touch

compile :: IO ()
compile = void $ shell "pulp --psc-package build" empty

timed :: IO () -> IO Clock.TimeSpec
timed action = do
  start <- Clock.getTime Clock.Monotonic
  action
  end <- Clock.getTime Clock.Monotonic
  pure $ Clock.diffTimeSpec end start

timedN n setup action = do
  replicateM n $ do
    setup
    timed action

average x = Clock.fromNanoSecs $ Clock.toNanoSecs (sum x) `div` fromIntegral (length x)

main :: IO ()
main = do
  compile
  t <- timedN 5 (pure ()) compile
  print t
  print $ average t
