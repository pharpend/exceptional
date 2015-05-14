module Control.Exceptional where

import Control.Applicative

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
