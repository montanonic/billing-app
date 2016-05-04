module Form.Field.Currency where

import Import

import Model.Currency

{-
currencyField :: Field Handler Currency
currencyField = let
    uncheckedField :: Field Handler Currency
    uncheckedField = convertField
        parseCurrencyUnsafe -- value on the backend
        formatCurrency -- value to render in the field
        textField
        -- add a check to give a helpful error message if parsing fails
    in check (note msg . parseCurrency) uncheckedField
    where msg = "Rate improperly formatted. Use ',' or '.' to separate dollars\
        \ from cents, and cents should not exceed two digits." :: Text
-}

currencyField :: Field Handler Currency
currencyField = checkMap
    (note msg . parseCurrency)
    formatCurrency
    textField

    where
        msg :: Text
        msg = "Please use conventional monetary format. Example: 14.65"
