{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.PersistField.TimeZone where

import ClassyPrelude.Yesod
import Import.Utilities
import Database.Persist.Sql
import Data.Time.LocalTime (TimeZone(..))

-- | Stringly-typed storage implementation.
instance PersistField TimeZone where
    toPersistValue zone = PersistText $ showT zone
    fromPersistValue (PersistText tzone) = case readMay tzone of
        Just zone -> Right $ zone
        Nothing -> Left "TimeZone could not be read from string. `read . show`\
            \ should be id when the type is restricted, but parsing failed in\
            \ this case."
    fromPersistValue _ = Left "TimeZone value was not properly stored in\
        \ database."

instance PersistFieldSql TimeZone where
    sqlType _ = SqlString
