module Monitors.Activity (
    InactivityWindow(..),
    InactivityCallback(..),
    MonitoredEvent(..),
    start,
    reset,
    checkpointKey
) where


import Prelude

import Data.DateTime (diff)
import Data.DateTime.Instant (Instant, instant, unInstant, toDateTime)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds, Milliseconds(..))
import Effect (Effect)
import Effect.Now (now)
import Effect.Timer (setInterval)
import Global (readInt)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML.HTMLDocument as D
import Web.HTML.Window (document, localStorage)
import Web.HTML.Window as W
import Web.Storage.Storage (getItem, setItem)


-- | How long to allow an inactive session to continue.
newtype InactivityWindow = InactivityWindow Seconds

-- | The events which constitute "activity". There is a default set consisting of:
-- window.load, document.click, document.mouseMove, document.keypress
data MonitoredEvent = ME {
    name:: EventType,
    context :: EventTarget
    }

--
-- API
--

-- | A side effect performed if the activity monitor times out
newtype InactivityCallback = OnInactivity (Effect Unit)

-- | Starts a new timer with the default
start ::
    InactivityCallback
    -> InactivityWindow
    -> Effect Unit
start cb window = do
    me <- monitoredEvents
    start' cb window me

start' ::
    InactivityCallback
    -> InactivityWindow
    -> Array MonitoredEvent
    -> Effect Unit
start' (OnInactivity cb) window events = do
    bindMonitoredEvents events
    checkpoint
    void $ setInterval checkInterval callOnInactivity
    where
        callOnInactivity =
            (\b -> when b cb) =<< hasWindowExpired window

reset :: Effect Unit
reset = checkpoint

--
-- Helpers
--

checkpoint ::
    Effect Unit
checkpoint = do
    currentTime <- show <<< unInstant <$> now
    ls <- localStorage =<< window
    setItem checkpointKey currentTime ls


getLatestCheckpoint ::
    Instant
    -> Effect (Maybe Instant)
getLatestCheckpoint currentTime = do
    ls <- localStorage =<< window
    lastCheckpointTime <- getItem checkpointKey ls
    case lastCheckpointTime of
        Nothing ->
            pure $ Just currentTime
        Just ts ->
            pure <<< instant <<< Milliseconds $ readInt 10 ts

-- | Checks whether or not the . If there is an error pulling
hasWindowExpired ::
    InactivityWindow
    -> Effect Boolean -- TODO: Need more information in this result
hasWindowExpired (InactivityWindow n) = do
    currentTime <- now
    mostRecent <- getLatestCheckpoint currentTime
    case mostRecent of
        Nothing -> pure true
        Just cp -> let
            nDT = toDateTime currentTime
            cpDT = toDateTime cp
            inactivity = diff nDT cpDT
            in pure $ inactivity > n


bindMonitoredEvents ::
    Array MonitoredEvent
    -> Effect Unit
bindMonitoredEvents =
    traverse_ attatchEvent
    where
    attatchEvent (ME {name, context}) = do
        listener <- eventListener (const checkpoint)
        addEventListener name listener false context


--
-- Defaults
--

-- | The default checkpoint key for local storage
checkpointKey :: String
checkpointKey = "last_activity_checkpoint"

-- | Ten Seconds
checkInterval :: Int
checkInterval = 10000

monitoredEvents :: Effect (Array MonitoredEvent)
monitoredEvents = do
    w <-  window
    doc <- D.toEventTarget <$> document w
    let win = W.toEventTarget w
    pure [
         ME {name: EventType "click", context: doc}
        ,ME {name: EventType "mousemove", context: doc}
        ,ME {name: EventType "keypress", context: doc}
        ,ME {name: EventType "load", context: win}
        ]
