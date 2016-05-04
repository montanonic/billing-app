module Calendar.Events.List
    ( EventsListResponse(..)
    , OrderBy(..)
    , setSearchTerms
    , orderListBy
    , setLatestStartTime
    , setEarliestEndTime
    , createEventsListRequest
    , defaultEventsListRequest
    ) where

import Import
import Network.HTTP.Simple
import Data.Aeson.TH

import Calendar (CalendarId(..))
import Calendar.Events
import GoogleAPI.Auth (QueryParams, bqp, addServerKey,
    AccessToken, addGoogleAPIAuthHeader)

-- See link below for API reference.
-- https://developers.google.com/google-apps/calendar/v3/reference/events/list#request

-- | When google sends us a response to an events.list query, we turn the
-- response into the following data type.
data EventsListResponse = EventsListResponse
    { elKind :: Text -- ^ Type of the collection ("calendar#events").
    , elEtag :: Text -- ^ ETag of the collection.
    , elSummary :: Text -- ^ Title of the calendar.
    , elDescription :: Maybe Text -- ^ Description of the calendar.
    , elUpdated :: UTCTime -- ^ Last modification time of the calendar (as a
        -- RFC3339 timestamp). Read-only.
    , elTimeZone :: Text -- ^ The time zone of the calendar.
    , elNextPageToken :: Maybe Text -- ^ Token used to access the next page of
        -- this result. Omitted if no further results are available, in which
        -- case nextSyncToken is provided.
    , elNextSyncToken :: Maybe Text -- ^ Token used at a later point in time to
        -- retrieve only the entries that have changed since this result was
        -- returned. Omitted if further results are available, in which case
        -- nextPageToken is provided.
    , elItems :: [Event]
    } deriving (Eq, Read, Show)
$(deriveFromJSON defaultOptions{fieldLabelModifier = unCapitalize . drop 2} ''EventsListResponse)

data EventsListQueries = EventsListQueries
    { elqMaxAttendees :: Maybe Int -- ^ max >= 1; see API reference; we force
        -- this field to equal 1 so that attendees are not included in Calendar
        -- queries, as the app currently has no use for that information.
    , elqMaxResults :: Maybe Int -- ^ defaults to 250; max value is 2500
    , elqOrderBy :: Maybe OrderBy
    , elqSearchTerms :: [ByteString] -- ^ Free text search terms to find events
        -- that match these terms in any field, except for extended properties.
        -- This field is denoted as "q" in the API reference.
    , elqSingleEvents :: Maybe Bool -- ^ whether or not to display all recurring
        -- events as individual events. We default this field to True for this
        -- app, as we're concerned with the time of events, and whether or not
        -- they recur is irrelevant except for the potential of simplifying
        -- some calculations.
    , elqTimeMax :: Maybe UTCTime
    , elqTimeMin :: Maybe UTCTime
    }

data OrderBy
    = StartTime
    | Updated
    deriving (Eq, Show)

renderOrderBy :: OrderBy -> ByteString
renderOrderBy = \case
    StartTime -> "startTime"
    Updated -> "updated"

-- | Sets default query parameters, including: omitting attendees, as that's not
-- information we'll likely ever need or want; returning all recurring events
-- as a collection of single events;
defaultEventsListQueries :: EventsListQueries
defaultEventsListQueries = EventsListQueries
    { elqMaxAttendees = Just 1
    , elqMaxResults = Just 2500
    , elqOrderBy = Nothing
    , elqSearchTerms = []
    , elqSingleEvents = Just True
    , elqTimeMax = Nothing
    , elqTimeMin = Nothing
    }

--
-- ** add/modify query parameters
--

setSearchTerms :: [ByteString] -> EventsListQueries -> EventsListQueries
setSearchTerms terms elq = elq { elqSearchTerms = terms }

orderListBy :: OrderBy -> EventsListQueries -> EventsListQueries
orderListBy ob elq = elq { elqOrderBy = Just ob }

-- | Will filter out all events with a start time after the one given.
setLatestStartTime :: UTCTime -> EventsListQueries -> EventsListQueries
setLatestStartTime time elq = elq { elqTimeMax = Just time }

-- | Will filter out all events with an end time at or before the one given.
setEarliestEndTime :: UTCTime -> EventsListQueries -> EventsListQueries
setEarliestEndTime time elq = elq { elqTimeMin = Just time }

--
-- ** Create HTTP request
--

makeQueryParams :: (EventsListQueries -> EventsListQueries) -> QueryParams
makeQueryParams f = addServerKey $ go (f defaultEventsListQueries) where
    go (EventsListQueries{..}) = (catMaybes
        [ bqp "maxAttendees" elqMaxAttendees
        , bqp "maxResults" elqMaxResults
        , (\x -> ("orderBy", Just . renderOrderBy $ x)) <$> elqOrderBy
        , bqp "singleEvents" elqSingleEvents
        , bqp "timeMax" elqTimeMax
        , bqp "timeMin" elqTimeMin
        ])
        ++ ((,) "q" . Just <$> elqSearchTerms)

createEventsListRequest :: MonadThrow m =>
       CalendarId
    -> AccessToken
    -> (EventsListQueries -> EventsListQueries)
    -> m Request
createEventsListRequest (CalendarId cid) token f = do
    initReq <- parseUrl . unpack $
        ("https://www.googleapis.com/calendar/v3/calendars/"
        ++ cid ++ "/events" :: Text)

    return $ setRequestQueryString (makeQueryParams f)
        . addGoogleAPIAuthHeader token $
        initReq { method = "GET" }

defaultEventsListRequest :: MonadThrow m =>
       CalendarId
    -> AccessToken
    -> m Request
defaultEventsListRequest cid token = createEventsListRequest cid token id
