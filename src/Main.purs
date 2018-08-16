module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Timer as T

import PurePitch

main :: Effect Unit
main = do
  void $ T.setTimeout 10 do
    log "timeout increment counter"
    purePitchInit
