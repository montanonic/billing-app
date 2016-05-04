module Form.Field.CaseInsensitive where

import Import
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI

ciTextField :: Field Handler (CI Text)
ciTextField = convertField CI.mk CI.original textField
