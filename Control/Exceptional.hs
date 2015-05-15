module Control.Exceptional where

import Control.Applicative
import Data.Monoid (mempty)

-- |This is basically specialized 'Either String', or 'Maybe' with error
-- messages.
data Exceptional x
  = Failure String
  | Success x
  deriving (Eq,Show,Read)

instance Functor Exceptional where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure s) = Failure s

instance Applicative Exceptional where
  pure = Success
  Success f <*> Success x = Success (f x)
  Failure s <*> _ = Failure s
  _ <*> Failure s = Failure s

instance Alternative Exceptional where
  empty = Failure mempty
  Success a <|> _ = Success a
  _ <|> Failure s = Failure s

instance Monad Exceptional where
  (>>=) (Success x) f = f x
  (>>=) (Failure s) _ = Failure s
  fail = Failure
  return = pure

-- |Convert 'Exceptional' into another 'Monad'
runExceptional :: Monad m => Exceptional x -> m x
runExceptional (Failure s) = fail s
runExceptional (Success s) = pure s

-- |Convert a 'Maybe' to an 'Exceptional'
fromMaybe :: String -> Maybe a -> Exceptional a
fromMaybe s Nothing = fail s
fromMaybe s (Just x) = pure x

-- |Convert a 'Maybe' to an 'Exceptional'
toMaybe :: Exceptional a -> Maybe a
toMaybe (Success x) = Just x
toMaybe (Failure _) = Nothing

-- |Convert an 'Either' 'String' to an 'Exceptional'
fromEither :: Either String a -> Exceptional a
fromEither (Left s) = fail s
fromEither (Right x) = pure x

-- |Convert an 'Exceptional' to an 'Either' 'String'
toEither :: Exceptional a -> Either String a
toEither (Failure s) = Left s
toEither (Success x) = Right x
