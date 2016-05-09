{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module is used in Model.CalendarEvents to provide additional data
-- structures needed to make a fromJSON instance for CalendarEvents.
module Model.PersistHelpers.CalendarEvents where

import ClassyPrelude.Yesod
import Import.Utilities
import Data.Aeson.TH
import Data.Time.LocalTime (ZonedTime(..))

instance Eq ZonedTime where
    (==) (ZonedTime a1 b1) (ZonedTime a2 b2) = (a1 == a2) && (b1 == b2)

data EventStart = EventStart
    { esDate :: Maybe Day
    , esDateTime :: Maybe ZonedTime
    , esTimeZone :: Maybe Text
    } deriving (Eq, Read, Show)
$(deriveFromJSON defaultOptions{fieldLabelModifier = unCapitalize . drop 2} ''EventStart)

data EventEnd = EventEnd
    { eeDate :: Maybe Day
    , eeDateTime :: Maybe ZonedTime
    , eeTimeZone :: Maybe Text
    } deriving (Eq, Read, Show)
$(deriveFromJSON defaultOptions{fieldLabelModifier = unCapitalize . drop 2} ''EventEnd)

{- We don't use this anymore. Now-redundant intermediary data structure.

-- | We don't store this structure directly on the backend. Instead, we use it
-- as an intermediary type between a JSON result and a CalendarEvent stored on
-- the backend. There's no difference between using this type and manually
-- deriving JSON instances for CalendarEvent, except for that...
data Event = Event
    { eKind :: Text -- ^ Type of the resource ("calendar#event").
    , eEtag :: Text -- ^ ETag of the resource.
    , eId :: Text -- ^ unique event identifier
    , eHtmlLink :: Text -- ^ An absolute link to this event in the Google
        -- Calendar Web UI.
    , eCreated :: UTCTime -- ^ Creation time of the event (as a RFC3339
        -- timestamp).
    , eUpdated :: UTCTime -- ^ Last modification time of the event (as a
        -- RFC3339 timestamp).
    , eSummary :: Maybe Text -- ^ Title of the event.
    , eDescription :: Maybe Text -- ^ Description of the event.
    , eLocation :: Maybe Text -- ^ Geographic location of the event as free-form
        -- text
    , eColorId :: Maybe Text -- ^ The color of the event. This is an ID
        -- referring to an entry in the event section of the colors definition
        -- (see the colors endpoint).
    , eStart :: EventStart
    , eEnd :: EventEnd
    , eICalUID :: Text -- ^ Event unique identifier as defined in RFC5545. It is
        -- used to uniquely identify events accross calendaring systems and must
        -- be supplied when importing events via the import method.
    , eSequence :: Int
    } deriving (Eq, Read, Show)
$(deriveFromJSON defaultOptions{fieldLabelModifier = unCapitalize . drop 1} ''Event)
-}
