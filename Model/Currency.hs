module Model.Currency
    ( Currency
    , toCurrency
    , fromCurrency
    , formatCurrency -- pretty print
    , parseCurrency

    -- specifically for use in making Currency a Persistent Field
    , unsafeGetCurrency
    , unsafeMakeCurrency

    , parseCurrencyUnsafe
    )
    where

import ClassyPrelude
import Import.Utilities
import Control.Error (hush)

-- for parsing
import Data.Char (isDigit)
import Data.Attoparsec.Text

type Dollars = Int64
type Cents = Int64

-- | Only deals with USD for now. Converts all dollars into cents, to avoid
-- floating point errors.
newtype Currency = Currency Cents
    deriving (Eq, Show, Read)

unsafeGetCurrency :: Currency -> Cents
unsafeGetCurrency (Currency cents) = cents

unsafeMakeCurrency :: Cents -> Currency
unsafeMakeCurrency = Currency

{-
--
-- ** Persistent Interoperability
--

instance PersistField Currency where
    toPersistValue (Currency cents) = PersistInt64 cents
    fromPersistValue (PersistInt64 cents) = Right $ Currency cents
    fromPersistValue _ = Left "Currency value was not properly stored in\
        \ database."


instance PersistFieldSql Currency where
    sqlType _ = SqlInt64
-}
--------------------------------------------------------------------------------

toCurrency :: Dollars -> Cents -> Currency
toCurrency d c = Currency $ (abs d * 100) + (abs c)

fromCurrency :: Currency -> (Dollars, Cents)
fromCurrency (Currency cents) = cents `divMod` 100

-- | Convert a Currency value into a properly formatted Text. Can convert back
-- into Currency by using 'parseCurrency'. Hence:
--      parseCurrencyUnsafe . formatCurrency == id
--  ==  formatCurrency . parseCurrencyUnsafe
formatCurrency :: Currency -> Text
formatCurrency cur = showT d ++ "." ++ showCents
  where
    (d, c) = fromCurrency cur
    showCents
        | c <= 9 = "0" ++ showT c
        | otherwise = showT c

-- | Parses textual values of the following forms into Currency:
-- "12345" -> Currency (1234500)
-- "123.45" -> Currency (12345)
-- "123,45" -> Currency (12345)
-- "123,4" -> Currency (12340)
-- "123." -> Currency (12300)
-- ",4" -> Currency (40)
-- ".45" -> Currency (45)
-- Will fail on inputs formatted in another manner.
currencyParser :: Parser Currency
currencyParser = do
    dollars <- read <$> (takeWhile1 isDigit <|> return "0")
    let earlyResult = toCurrency dollars 0
    (endOfInput $> earlyResult) <|> (centsParser dollars)

  where
    centsParser dollars = do
        _ <- char '.' <|> char ',' <|> char ';'
        cents <- read <$> (count 2 $ digit <|> return '0')
        return $ toCurrency dollars cents

-- | Removes all whitespace, and then parses text into Maybe Currency.
parseCurrency :: Text -> Maybe Currency
parseCurrency = hush . parseOnly currencyParser . filter (/= ' ')

-- | Throws an error in case of failure instead of using Maybe type. Intended
-- to be used only where we can guarantee that a Currency value is properly
-- formatted as Text.
parseCurrencyUnsafe :: Text -> Currency
parseCurrencyUnsafe t = case parseOnly currencyParser $ filter (/= ' ') t of
    Right c -> c
    _ -> error "Currency did not parse properly"
