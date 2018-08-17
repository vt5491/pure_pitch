module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Timer as T
import Data.Maybe
import Data.Array ((!!))

import PurePitch

main :: Effect Unit
main = do
  -- let _ = quicky $ Just 7
  -- quicky2 $ Just 7
  -- let a = [NoteData {freq: 440.0, durRatio: 8}]
  -- doSomething $ a !! 0
  -- doSomething $ overTheMountianTriads !! 0
  void $ T.setTimeout 10 do
    log "timeout increment counter"
    purePitchInit

quicky :: Maybe Int -> Effect Unit
quicky n = do
            log "now in quicky"
            case n of
              Nothing -> log "nutting"
              Just a -> do
                log $ "a=" <> show a

quicky2 n = case n of
              Nothing -> log "nutting"
              Just a -> do
                log $ "a2=" <> show a
