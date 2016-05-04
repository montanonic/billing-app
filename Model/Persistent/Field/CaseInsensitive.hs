{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Persistent.Field.CaseInsensitive where

import ClassyPrelude
import Import.Utilities

-- for making new Persistent data fields.
import Database.Persist.Types
import Database.Persist.Sql

import Data.CaseInsensitive (CI)

instance PersistField (CI Text) where
    toPersistValue ci = PersistText $ showT ci
    fromPersistValue (PersistText tci) = case readMay tci of
        Just ci -> Right ci
        Nothing -> Left "CI value could not be read from Text"
    fromPersistValue _ = Left "CI values must be converted from PersistText"

instance PersistFieldSql (CI Text) where
    sqlType _ = SqlString
