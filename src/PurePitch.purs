module PurePitch where

import Data.Foldable
import Data.Functor
import Data.List.Types
import Data.Maybe
import Data.String
import Prelude

import Audio.WebAudio.AudioParam (getValue, setValue, setValueAtTime)
import Audio.WebAudio.BaseAudioContext (createGain, createOscillator, currentTime, destination, newAudioContext, resume, state, suspend)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.Oscillator (OscillatorType(..), frequency, setFrequency, setOscillatorType, startOscillator, stopOscillator)
import Audio.WebAudio.Types (AudioContext, GainNode, OscillatorNode, AudioContextState(..), connect)
import Data.Array (filter, (..), (!!), index, length, toUnfoldable, fromFoldable)
import Data.Int (toNumber, fromNumber, ceil)
import Data.List as L
import Data.Newtype (over, wrap)
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


chromaticMap :: ToneIndex -> Number
chromaticMap Zero  = 1.000000
chromaticMap One  = 1.059463
chromaticMap Two  = 1.122462
chromaticMap Three = 1.189207
chromaticMap Four = 1.259921
chromaticMap Five = 1.334839
chromaticMap Six = 1.414213
chromaticMap Seven = 1.498307
chromaticMap Eight = 1.587401
chromaticMap Nine = 1.681792
chromaticMap Ten = 1.781797
chromaticMap Eleven = 1.887748

data ToneIndex = Zero | One | Two | Three | Four | Five
  | Six | Seven | Eight | Nine | Ten | Eleven

derive instance eqToneIndex :: Eq ToneIndex
derive instance ordToneIndex :: Ord ToneIndex


-- chromaticIntervals :: ToneInterval -> Number
-- chromaticIntervals Tonic  = 1.000000
-- chromaticIntervals First  = 1.059463
-- chromaticIntervals Second  = 1.122462
-- chromaticIntervals MinThird = 1.189207
-- chromaticIntervals MajThird = 1.259921
-- chromaticIntervals Fourth = 1.334839
-- chromaticIntervals Diminished = 1.414213
-- chromaticIntervals Fifth = 1.498307
-- chromaticIntervals AugFifth = 1.587401
-- chromaticIntervals Sixth = 1.681792
-- chromaticIntervals MinSeventh = 1.781797
-- chromaticIntervals MajSeventh = 1.887748
-- -- data ToneInterval = 0 | 1 | 2 deriving (Enum)
-- -- data ToneInterval = Zero | One | Two deriving (Enum)
-- data ToneIndex = Zero | One | Two | Three | Four | Five
--   | Six | Seven | Eight | Nine | Ten | Eleven
--
-- derive instance eqToneIndex :: Eq ToneIndex
-- derive instance ordToneIndex :: Ord ToneIndex

wholeNoteDuration :: Number
wholeNoteDuration = 1.0

-- type Entry =
--   { firstName :: String
--   , lastName  :: String
--   , address   :: Address
--   }
type NoteType =
  { freq :: Number,
    durRatio :: Int
  }

data NoteData = NoteData
  { freq :: Number
  , durRatio :: Int
  }

showNote :: NoteType -> String
showNote n = "note.freq=" <> show n.freq

-- showNoteData :: NoteData -> String
-- showNoteData n = "note.freq=" <> show n.freq
  -- type AddressBook = List Entry

-- newtype A = A {x::Int, y::Int}
-- newtype N2 = N2 {x:: Int, y:: Int}
-- overTheMountianTriads
-- type AddressBook = List Entry
-- type Tune = Array Note
type Tune = Array NoteData

-- overTheMountianTriads :: Tune
overTheMountianTriads :: Array {freq :: Number, durRatio :: Int }
overTheMountianTriads = [
  -- NoteData {freq: 220.0, durRatio: 12},
  -- NoteData {freq: 660.0, durRatio: 12}
  {freq: 300.0, durRatio: 12},
  {freq: 660.0, durRatio: 12}
  ]

otm = overTheMountianTriads

-- emptyTune :: Tune
-- emptyTune = empty

-- showAddress :: Address -> String
-- showAddress addr = addr.street <> ", " <>
-- showNote n =  show n.freq
-- instance showRomanDigit :: Show RomanDigit where
--   -- show x = "Foo bar=" <> show x
--   show M = "M"
-- instance showNote :: Show Note where
--   show n = "note.freq=" <> n.freq
myIntList :: List Int
myIntList = (Cons 1 Nil)

myNoteDataList :: List {freq :: Number, durRatio :: Int }
myNoteDataList = (Cons {freq: 1.0, durRatio: 7}  Nil)

----------------
--
--   Functions
--
-----------------
-- doSomething :: Maybe NoteType -> Effect Unit
doSomething :: Maybe {freq :: Number, durRatio :: Int} -> Effect Unit
doSomething mn = case mn of
                  Nothing -> log "doSomething: got Nothing"
                  Just n -> do
                    log $ "freq=" <> show n


purePitchInit = do
  setupPlayBtn

-- play a single tone
beep :: Number -> Effect Unit
beep freq = do
  ctx <- newAudioContext
  osc <- createOscillator ctx
  setOscillatorType Sine osc
  -- var gainNode = audioCtx.createGain();
  -- gainNode <- createGain osc
  log "PurePitch: now calling startOscillator"
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

-- playTune :: Tune -> Effect Unit
playTune :: Array {freq :: Number, durRatio :: Int} -> Effect Unit
playTune tune = do
  ctx <- newAudioContext
  osc <- createOscillator ctx
  startOscillator 0.0 osc
  let mn = otm !! 0
  -- let abc :: Number
  -- let abc = 7.0
  let freq  =  case mn of
                Nothing -> 0
                -- Just n -> debugger 1
                -- Just n -> fromNumber $ 240
                -- Just n -> 240
                Just n -> ceil n.freq
  -- let freq = mn do
  --                 case mn of
  --                   Nothing -> 0
  --                   Just n -> n.freq
  log $ "playTune: freq=" <> show freq
  -- setValue freq =<< frequency osc
  -- connect osc =<< destination ctx
  -- let duration = 0.5
  -- -- halfway through, double the freq
  -- -- void $ T.setTimeout $ duration / 2.0 do
  -- --   setFrequency freq  osc
  -- _ <- T.setTimeout 250 do
  --   setFrequency 1200.0  osc
  -- stopOscillator duration osc
  pure unit

-- initOscillator :: Effect Unit
-- initOscillator = do
--   ctx <- newAudioContext
--   osc <- createOscillator ctx

-- play a tone stream (e.g. a lick or little note sequence)
-- toneStream :: Number -> Effect Unit
-- toneStream freq = do

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
          -- evtListener <- (eventListener playSong)
          evtListener <- (eventListener playTone)
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

-- btnHandler :: Event -> Effect Unit
-- btnHandler e = do
--   log "btn pressed"
--   beep 890.0

playTone :: Event -> Effect Unit
playTone e = do
  log "btn pressed"
  -- initOscillator
  -- beep 880.0
  playTune otm
  -- toneStream 880.0
  -- let r = toneDeafJsDoIt 5
  -- log $ "btnHandler: r=" <> r
  -- let custEvt = createVtEvt 1
  -- dispatchVtEvt custEvt
  pure unit

-- arrayToList :: forall a. Array a -> List a
-- arrayToList = ?whatGoesHere

foreign import debugger :: Int -> Int
