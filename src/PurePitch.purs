module PurePitch where

import Data.Functor
import Data.Maybe
import Data.String
import Prelude

import Audio.WebAudio.AudioParam (getValue, setValue, setValueAtTime)
import Audio.WebAudio.BaseAudioContext (createGain, createOscillator, currentTime, destination, newAudioContext, resume, state, suspend)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.Oscillator (OscillatorType(..), frequency, setFrequency, setOscillatorType, startOscillator, stopOscillator)
import Audio.WebAudio.Types (AudioContext, GainNode, OscillatorNode, AudioContextState(..), connect)
import Data.Array (filter, (..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Console (log)
import Effect.Timer as T
import Web.DOM (Element)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (querySelector)
import Web.Event.CustomEvent (CustomEvent)
import Web.Event.Event (EventType(..), Event)
import Web.Event.EventTarget (EventTarget, addEventListener, dispatchEvent, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode, toDocument)
import Web.HTML.HTMLElement (toElement, fromElement, className, HTMLElement)
import Web.HTML.Window (Window, document)

doSomething :: Int
doSomething = 7

purePitchInit = do
  setupPlayBtn

-- play a single tone
beep :: Number -> Effect Unit
beep freq = do
  ctx <- newAudioContext
  osc <- createOscillator ctx
  setOscillatorType Sine osc
  log "ToneDeaf: now calling startOscillator"
  startOscillator 0.0 osc
  setValue freq =<< frequency osc
  connect osc =<< destination ctx
  let duration = 0.5
  -- halfway through, double the freq
  -- void $ T.setTimeout $ duration / 2.0 do
  --   setFrequency freq  osc
  _ <- T.setTimeout 250 do
    setFrequency 1200.0  osc
  stopOscillator duration osc
  pure unit

initOscillator :: Effect Unit
initOscillator = do
  ctx <- newAudioContext
  osc <- createOscillator ctx

-- play a tone stream (e.g. a lick or little note sequence)
toneStream :: Number -> Effect Unit
toneStream freq = do

setupPlayBtn ::  Effect Unit
setupPlayBtn = do
  log "PurePitch: now in setupPlayBtn"
  doc <- map toParentNode (window >>= document)
  mbtn <- querySelector (wrap "#play-tone-btn") doc
  case mbtn of
    Nothing -> log "mbtn failed"
    Just btn -> do
      let mhtmlEl = fromElement btn
      case mhtmlEl of
        Nothing -> log "mhtml failed"
        Just htmlEl -> do
          let cn = className htmlEl
          log $ "classname below:"
          cn >>= log
          evtListener <- (eventListener playSong)
          addEventListener
            (EventType "mousedown")
            evtListener
            false
            ((toEventTarget <<< toElement) htmlEl)
          -- vtEvtListener <- (eventListener vtEvtHandler)
          -- addEventListener
          --   (EventType "vtEvt")
          --   vtEvtListener
          --   false
          --   ((toEventTarget <<< toElement) htmlEl)
          -- pure unit
      pure unit
  pure unit

playSong :: Event -> Effect Unit
playSong e = do
  log "btn pressed"
  initOscillator
  -- toneStream 880.0
  -- let r = toneDeafJsDoIt 5
  -- log $ "btnHandler: r=" <> r
  -- let custEvt = createVtEvt 1
  -- dispatchVtEvt custEvt
  pure unit
