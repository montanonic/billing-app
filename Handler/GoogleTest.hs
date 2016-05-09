module Handler.GoogleTest where

import Import

import Data.Aeson
import Network.HTTP.Simple

import GoogleAPI.Auth
import Calendar
import Calendar.Events.List

getGoogleTestR :: Handler Html
getGoogleTestR = do
    mtoken <- maybeAccessToken
    case mtoken of
        Nothing -> error "failure"
        Just token -> do
            r <- join $ httpJSON <$>
                defaultEventsListRequest defaultCalendarId token
            let body = fromJSON (getResponseBody r) :: Result EventsListResponse
            defaultLayout $ [whamlet|<p>#{show body}|]

{-      do
    mtoken <- (map AccessToken . userAccessToken) <$> (runDB $ get404 uid)
    case mtoken of
        Nothing -> error "failure"
        Just token -> do
            r <- join $ httpJSON <$>
                defaultEventsListRequest defaultCalendarId token
            let body = fromJSON (getResponseBody r) :: Result EventsListResponse
            defaultLayout $ [whamlet|<p>#{show body}|]
-}


{-
    mtoken <- maybeAccessToken
    case mtoken of
        Nothing -> error "no access token"
        Just token -> do
            r <- join $ httpJSON <$>
                createEventsListRequest defaultCalendarId token
                    (setSearchTerms ["Longo"])
            let body = fromJSON (getResponseBody r) :: Result EventsListResponse
            case body of
                Error e -> error . show $ e
                Success eList ->
                    let events = elItems eList
                        res = [ func (esDateTime eStart) (eeDateTime eEnd)
                            | Event{..} <- events]
                    in defaultLayout $ [whamlet|
<h1>Total hours you've worked on Longo case (estimate): #{show $ foldl' add (HoursMinutes (0,0)) res}

<h3>Hours for each event:
$forall r <- res
    <p>#{show r}
|]

func :: Maybe ZonedTime -> Maybe ZonedTime -> HoursMinutes Int
func (Just zt1) (Just zt2) = HoursMinutes $
    (`divMod` 60) . (`div` 60) . truncate $
        diffUTCTime (zonedTimeToUTC zt2) (zonedTimeToUTC zt1)
func _ _ = HoursMinutes (8, 0)

newtype HoursMinutes a = HoursMinutes (a, a)

add :: Integral a => HoursMinutes a -> HoursMinutes a -> HoursMinutes a
add (HoursMinutes (h1, m1)) (HoursMinutes (h2, m2)) = let
    (h', m') = divMod (m1+m2) 60
    in HoursMinutes (h1+h2+h', m')

instance (Ord a, Show a, Integral a) => Show (HoursMinutes a) where
    show (HoursMinutes (h, m)) = show h ++ " hours, " ++ show m ++ " minutes"
-}
