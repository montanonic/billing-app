-- | Contains types and functions for constructing properly authenticated
-- queries to GoogleAPI. Additionally contains utilities to help with forming
-- HTTP requests.
module GoogleAPI.Auth
    -- HTTP utilities
    ( QueryParams
    , buildQueryParam
    , bqp
    -- auth
    , addServerKey
    , AccessToken(..)
    , maybeAccessToken
    , addGoogleAPIAuthHeader
    ) where

import Import
import Network.HTTP.Simple (addRequestHeader)
import Data.Aeson
import qualified Data.ByteString as B

--
-- HTTP request builder helper functions and types
--

type QueryParams = [(B.ByteString, Maybe B.ByteString)]

-- | For ease in constructing query parameters using Network.HTTP.Simple
buildQueryParam, bqp :: (ToJSON a) => B.ByteString -> Maybe a ->
    Maybe (B.ByteString, Maybe B.ByteString)
buildQueryParam name (Just a) = Just (name, Just . toStrict . encode $ a)
buildQueryParam _ Nothing = Nothing
-- shorter, convenience alias
bqp = buildQueryParam

--
-- ** Google API authorization
--

addServerKey :: QueryParams -> QueryParams
addServerKey params = let
    keyParam = ("key", Just (serverKey :: B.ByteString))
    in keyParam : params

-- | This newtype is for added type safety. We export only the type, but not
-- the constructor, to ensure proper handling.
newtype AccessToken = AccessToken B.ByteString
    deriving Show

-- | Looks up an access token in the session if it exists, and wraps it in a
-- newtype for added safety.
maybeAccessToken :: MonadHandler m => m (Maybe AccessToken)
maybeAccessToken = do
    mtoken <- lookupSessionBS accessToken
    return $ AccessToken <$> mtoken

-- | Adds an Authorization header to a Google API request.
addGoogleAPIAuthHeader :: AccessToken -> Request -> Request
addGoogleAPIAuthHeader (AccessToken token) =
    addRequestHeader hAuthorization ("Bearer " ++ token)
