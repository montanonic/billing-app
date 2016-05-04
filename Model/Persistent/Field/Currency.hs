{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Persistent.Field.Currency where

import ClassyPrelude
import Database.Persist
import Database.Persist.Sql

import Model.Currency

instance PersistField Currency where
    toPersistValue cur = PersistInt64 $ unsafeGetCurrency cur
    fromPersistValue (PersistInt64 cents) = Right $ unsafeMakeCurrency cents
    fromPersistValue _ = Left "Currency value was not properly stored in\
        \ database."

instance PersistFieldSql Currency where
    sqlType _ = SqlInt64
