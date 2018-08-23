module PurePitch where

import Data.Foldable
import Data.Functor
import Data.List.Types
import Data.Maybe
import Data.String
import Prelude
import Math (pow)

import Audio.WebAudio.AudioParam (getValue, setValue, setValueAtTime)
import Audio.WebAudio.BaseAudioContext (createGain, createOscillator, currentTime, destination, newAudioContext, resume, state, suspend)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.Oscillator (OscillatorType(..), frequency, setFrequency, setOscillatorType, startOscillator, stopOscillator)
import Audio.WebAudio.Types (AudioContext, GainNode, OscillatorNode, AudioContextState(..), connect)
import Data.Array (filter, (..), (!!), index, length, toUnfoldable, fromFoldable, concat, concatMap, cons, snoc)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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

halfStepMultiple :: Number
halfStepMultiple = 1.059463

-- Assume this is relative to A (e.g. 440 hz)
chromaticLookup :: Int -> Number
chromaticLookup 0  = 1.000000
chromaticLookup 1  = 1.059463
chromaticLookup 2  = 1.122462
chromaticLookup 3 = 1.189207
chromaticLookup 4 = 1.259921
chromaticLookup 5 = 1.334839
chromaticLookup 6 = 1.414213
chromaticLookup 7 = 1.498307
chromaticLookup 8 = 1.587401
chromaticLookup 9 = 1.681792
chromaticLookup 10 = 1.781797
chromaticLookup 11 = 1.887748
chromaticLookup _ = 1.00

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

-- showNote :: NoteType -> String
showNote2 :: Note -> String
-- showNote n = "note.freq=" <> show n.freq
showNote2 (Note {freq, durRatio}) = "note.freq=" <> show freq

getFreq :: Note -> Number
-- freq (Note {freq, _}) = freq
-- freq (Note freq _) = freq
getFreq (Note {freq, durRatio}) = freq

getDurRatio :: Note -> Int
getDurRatio (Note {freq, durRatio}) = durRatio

getString :: GuitarNote -> Int
getString (GuitarNote {string, fret}) = string

newtype Note = Note { freq :: Number , durRatio :: Int }
derive instance genericNote :: Generic Note _
instance showNote :: Show Note where
  show = genericShow

newtype GuitarNote = GuitarNote {string :: Int, fret :: Int }
derive instance genericGuitarNote :: Generic GuitarNote _
instance showGuitarNote :: Show GuitarNote where
  show = genericShow

newtype Stanza = Stanza {repeat :: Int, notes :: Array GuitarNote}
derive instance genericStanza :: Generic Stanza _
instance showStanza :: Show Stanza where
  show = genericShow

getStanza :: Array Stanza -> Int -> Stanza
getStanza a n = do
  let ms = a !! n
  case ms of
    Nothing -> Stanza {repeat: 0, notes: []}
    Just s -> s

getStanzaNotes :: Stanza -> Array GuitarNote
getStanzaNotes (Stanza {repeat, notes}) = notes

getStanzaRepeat :: Stanza -> Int
getStanzaRepeat (Stanza {repeat, notes}) = repeat

-- getStanzaNotes :: Maybe Stanza -> Array GuitarNote
-- getStanzaNotes ms =  case ms of
--                       Nothing -> []
--                       Just s -> do
--                         let mNotes = s

-- newtype Song = (Array Note) | (Array GuitarNote)
-- type Song =  Note {freq :: Number, durRatio :: Int} | GuitarNote {string :: Int, fret :: Int}
-- type Song = (Array Note) | (Array GuitarNote)

-- overTheMountianTriads :: Tune
-- overTheMountianTriads :: Array {freq :: Number, durRatio :: Int }
overTheMountianTriads :: Array Note
overTheMountianTriads = [
  Note {freq: 220.0, durRatio: 12},
  -- note: parens appear to be optional
  -- (Note {freq: 660.0, durRatio: 12})
  Note {freq: 440.0, durRatio: 12},
  Note {freq: 330.0, durRatio: 12}
  -- {freq: 300.0, durRatio: 12},
  -- {freq: 660.0, durRatio: 12}
  ]

otm = overTheMountianTriads

overTheMountianTab :: Array GuitarNote
overTheMountianTab = [
  GuitarNote {string: 1, fret: 17},
  GuitarNote {string: 1, fret: 13},
  GuitarNote {string: 2, fret: 15},

  GuitarNote {string: 1, fret: 18},
  GuitarNote {string: 1, fret: 13},
  GuitarNote {string: 2, fret: 15},

  GuitarNote {string: 1, fret: 15},
  GuitarNote {string: 1, fret: 12},
  GuitarNote {string: 2, fret: 13},

  GuitarNote {string: 1, fret: 17},
  GuitarNote {string: 1, fret: 13},
  GuitarNote {string: 2, fret: 13}
  ]

otmTab = overTheMountianTab

otmTab2 :: Array Stanza
otmTab2 = [
  Stanza {repeat: 4, notes: [
    GuitarNote {string: 1, fret: 17},
    GuitarNote {string: 1, fret: 13},
    GuitarNote {string: 2, fret: 15}
  ]},
  Stanza {repeat: 4, notes: [
    GuitarNote {string: 1, fret: 18},
    GuitarNote {string: 1, fret: 13},
    GuitarNote {string: 2, fret: 15}
  ]}
  ]

-- Stanza {repeat: 3,
--   [
--     GuitarNote {string: 1, fret: 17},
--     GuitarNote {string: 1, fret: 13},
--     GuitarNote {string: 2, fret: 15}
--   ]
-- }

-- otmTab2 ::

globalSong = otmTab
globalSongLen = (length globalSong) - 0

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

-- playTune2 :: Array Note -> Effect Unit
-- playTune2 tune = do
--   ctx <- newAudioContext
--   osc <- createOscillator ctx
--   connect osc =<< destination ctx
--   startOscillator 0.0 osc
--   -- foldl
--   pure unit

playTune :: Array Note -> Effect Unit
playTune tune = do
  ctx <- newAudioContext
  osc <- createOscillator ctx
  connect osc =<< destination ctx
  startOscillator 0.0 osc
  -- playNote osc 0
  -- playNote2 osc globalSong 0
  playStanza osc $ getStanza otmTab2 1
  pure unit

playNote :: OscillatorNode ->  Int -> Effect Unit
-- playNote osc 12 = stopOscillator 0.0  osc
-- playNote osc n |songLen = stopOscillator 0.0  osc
  -- where songLen = length globalSong
  -- where songLen = 6
playNote osc i
  -- let songLen = 12
  -- in
  | i > globalSongLen = log "playNote: error"
  | i == globalSongLen = stopOscillator 0.0  osc
  | i < globalSongLen = do
    let mNote = mTabToNote $ otmTab !! i
    let freq  = case mNote of
                  Nothing -> 0.0
                  Just n ->  getFreq n
    log $ "playNote: freq=" <> show freq
    setValue freq =<< frequency  osc
    _ <- T.setTimeout 500 do
        playNote osc (i + 1)

    pure unit
playNote _ _ = log "playNote: fallthrough"

playNote2 :: OscillatorNode -> Array GuitarNote ->  Int -> Effect Unit
playNote2 osc notes i
    | i > length notes = log "playNote2: error"
    | i == (length notes) = stopOscillator 0.0  osc
    | i < (length notes) = do
      let mNote = mTabToNote $ notes !! i
      let freq  = case mNote of
                    Nothing -> 0.0
                    Just n ->  getFreq n
      log $ "playNote: freq=" <> show freq
      setValue freq =<< frequency  osc
      _ <- T.setTimeout 500 do
          playNote2 osc notes (i + 1)

      pure unit

playNote2 _ _ _ = log "playNote2: fallthrough"

playStanza :: OscillatorNode -> Stanza -> Effect Unit
playStanza o s = playNote2 o (getStanzaNotes s)  0

-- create an array of GuitarNote's from a Stanza Array
-- flattenStanzas :: Array Stanza -> Array GuitarNote
-- flattenStanzas a =

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
          evtListener <- (eventListener startTone)
          addEventListener
            (EventType "mousedown")
            evtListener
            false
            ((toEventTarget <<< toElement) htmlEl)
      pure unit
  pure unit

startTone :: Event -> Effect Unit
startTone e = do
  log "btn pressed"
  -- beep 880.0
  -- playTune otm
  playTune otm
  pure unit

fretToFreq :: GuitarNote -> Number
-- fretToFreq (GuitarNote {string, fret}) = toNumber (string + fret)
fretToFreq (GuitarNote {string, fret}) =
                      case string of
                        6 -> 82.41 * pow halfStepMultiple (toNumber fret)
                        5 -> 110.0 * pow halfStepMultiple (toNumber fret)
                        4 -> 146.83 * pow halfStepMultiple (toNumber fret)
                        3 -> 196.0 * pow halfStepMultiple (toNumber fret)
                        2 -> 246.94 * pow halfStepMultiple (toNumber fret)
                        1 -> 329.63 * pow halfStepMultiple (toNumber fret)
                        _ -> 1.0

tabToNote :: GuitarNote -> Note
tabToNote tab = Note {freq: fretToFreq tab, durRatio: 12}

mTabToNote :: Maybe GuitarNote -> Maybe Note
-- mTabToNote (GuitarNote {string, fret}) = case mtab of
mTabToNote mtab = case mtab of
                    Nothing -> Nothing
                    Just gn -> Just $ Note {freq: fretToFreq gn, durRatio: 12}

-- repeatGuitarNotes :: Array GuitarNote -> Int -> Array GuitarNote
-- repeatGuitarNotes xs 0 = xs
-- repeatGuitarNotes xs n = a

-- repeatIntArray :: Array Int -> Int -> Array Int
-- repeatIntArray xs 0 = xs
-- repeatIntArray xs n =

-- typed hole example
-- arrayToList :: forall a. Array a -> List a
-- arrayToList = ?whatGoesHere

foreign import debugger :: Int -> Int
