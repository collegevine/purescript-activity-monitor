module Monitors.Activity (
    InactivityWindow(..),
    InactivityCallback(..),
    MonitoredEvent(..),
    start,
    start',
    reset,
    reset'
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
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as D
import Web.HTML.Window (document, localStorage)
import Web.HTML.Window as W
import Web.Storage.Storage (getItem, setItem)


-- | A session becomes inactive once the inactivity window has elapsed.
newtype InactivityWindow = InactivityWindow Seconds

-- | The events which constitute "activity". There is a default set consisting of:
-- | window.load, document.click, document.mouseMove, document.keypress
data MonitoredEvent = ME {
    name:: EventType,
    context :: EventTarget
    }

--
-- API
--

-- | A side effect performed if/when the activity monitor times out.
newtype InactivityCallback = OnInactivity (Effect Unit)

-- | Starts a new timer with the default 'MonitoredEvent's. See above for the specifc events.
start ::
    InactivityCallback
    -> InactivityWindow
    -> Effect Unit
start cb window = do
    me <- monitoredEvents
    start' checkpointKey cb window me

-- | A fully-configurable version of 'start'. If you wanted to trigger an inactivity
-- | callback based on the last time someone liked something on Facebook, you can do that.
start' ::
    String
    -> InactivityCallback
    -> InactivityWindow
    -> Array MonitoredEvent
    -> Effect Unit
start' key (OnInactivity cb) window events = do
    bindMonitoredEvents key events
    checkpoint key
    void $ setInterval checkInterval callOnInactivity
    where
        callOnInactivity =
            (\b -> when b cb) =<< hasWindowExpired window

-- | Manually reset the logout timer.
reset :: Effect Unit
reset = reset' checkpointKey

reset' ::
    String
    -> Effect Unit
reset' = checkpoint

--
-- Helpers
--

checkpoint ::
    String
    -> Effect Unit
checkpoint key = do
    (Milliseconds currentTime) <- unInstant <$> now
    let ts = show currentTime
    ls <- localStorage =<< window
    setItem key ts ls


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
    String
    -> Array MonitoredEvent
    -> Effect Unit
bindMonitoredEvents key =
    traverse_ attatchEvent
    where
    attatchEvent (ME {name, context}) = do
        listener <- eventListener (const $ checkpoint key)
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
         ME {name: EventType "mousedown", context: doc}
        ,ME {name: EventType "mousemove", context: doc}
        ,ME {name: EventType "keypress", context: doc}
        ,ME {name: EventType "scroll", context: doc}
        ,ME {name: EventType "load", context: win}
        ]
