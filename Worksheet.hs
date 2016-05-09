module Worksheet where

import ClassyPrelude.Yesod hiding (Reader, ask)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap = liftM

instance Applicative (Reader r) where
    pure = return
    (<*>) = ap

instance Monad (Reader r) where
    return a = Reader $ const a
    --(>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = f <$> ask

evalReader :: r -> Reader r a -> a
evalReader = flip runReader

test = asks (+100)

test2 = do
    x <- test
    y <- (*2) <$> test
    return (x, y)

test3 = do
    x <- test
    (a, b) <- test2
    return (x+a,x+b)
