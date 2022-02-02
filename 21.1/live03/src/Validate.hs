module Validate ( Validation
                , success
                , failure
                ) where


data Validation err a
   = Failure err
   | Success a
   deriving (Eq, Ord, Show)


success :: a -> Validation err a
success = Success

failure :: err -> Validation err a
failure = Failure

instance Functor (Validation err) where
   fmap _ (Failure err) = Failure err
   fmap f (Success x)   = Success (f x)


instance Semigroup err => Applicative (Validation err) where
  pure = Success
  Failure e1 <*> b = Failure $ case b of
    Failure e2 -> e1 <> e2
    Success _ -> e1
  Success _  <*> Failure e2 =
    Failure e2
  Success f  <*> Success a  =
    Success (f a)
