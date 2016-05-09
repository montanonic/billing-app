{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- Persistent Fields
import Model.Currency (Currency)
import Model.PersistField.Currency()
import Data.Time.LocalTime (TimeZone)
import Model.PersistField.TimeZone()

-- Imports for creating instances for Persitent Entitites, such as FromJSON.
import Data.Aeson
import Data.Time.LocalTime (ZonedTime(..), zonedTimeToUTC)
import Model.PersistHelpers.CalendarEvents

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance FromJSON CalendarEvent where
    parseJSON (Object o) = do
        calendarEventKind <- o .: "kind"
        calendarEventEtag <- o .: "etag"
        calendarEventIdent <- o .: "id"
        calendarEventHtmlLink <- o .: "htmlLink"
        calendarEventCreated <- o .: "created"
        calendarEventUpdated <- o .: "updated"
        calendarEventSummary <- o .: "summary"
        calendarEventDescription <- o .:? "description"
        calendarEventLocation <- o .:? "location"
        calendarEventColorId <- o .:? "colorId"
        EventStart{..} <- o .: "start"
        let calendarEventStartDate = esDate
            calendarEventStartDateTime = zonedTimeToUTC <$> esDateTime
            calendarEventStartDateTimeTimeZone = zonedTimeZone <$> esDateTime
            calendarEventStartTimeZone = esTimeZone
        EventEnd{..} <- o .: "end"
        let calendarEventEndDate = eeDate
            calendarEventEndDateTime = zonedTimeToUTC <$> eeDateTime
            calendarEventEndDateTimeTimeZone = zonedTimeZone <$> eeDateTime
            calendarEventEndTimeZone = eeTimeZone
        calendarEventICalUID <- o .: "iCalUID"
        return CalendarEvent{..}

    parseJSON _ = mzero
