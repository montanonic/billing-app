-- | For prototyping Calendar-based functionality.
module Calendar where

import Import

--
-- ** Types
--

-- | The unique Id referencing a Calendar. User's may have multiple Calendars,
-- and no Calendar Id is needed to refer to their primary one.
newtype CalendarId = CalendarId { getCalendarId :: forall a. IsString a => a }

-- | The most common case is that a user only modifies their main Calendar, so
-- we use this as the default option.
defaultCalendarId :: CalendarId
defaultCalendarId = CalendarId "primary"
