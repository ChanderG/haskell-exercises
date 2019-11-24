-- Implement a Counter that supports the inc and dec operation as a Monad.
-- Use inc/dec via the bind operator

-- Apparently, there is a State monad that can be used to model this as well.

{-# LANGUAGE FlexibleInstances #-}

-- Simple system, no monads
type Counter1 = Int

increment1 :: Counter1 -> Counter1
increment1 c = c+1

decrement1 :: Counter1 -> Counter1
decrement1 c = c-1

newCounter1 :: Counter1
newCounter1 = 0

-- Clearly not useful as you have to manually pass it in as an arg to all our functions


-- Simple Monad from scratch
-- Suprisingly a difficult exercise

data Counter a = Counter {count :: Integer, value :: a} deriving (Show)

increment :: a -> Counter a
increment v = Counter 1 v
decrement :: a -> Counter a
decrement v = Counter (-1) v

-- straight forward defintion
-- basically run the function on the value, ignoring the count itself
instance Functor Counter where
  fmap f (Counter c val) = Counter c (f val)

-- guessing this based on the requirements
-- run the function on the internal value, naively adding the counts
instance Applicative Counter where
  pure = Counter 0
  (Counter c1 f) <*> (Counter c2 x) = Counter (c1+c2) (f x)

-- Checked to satisfy the Monad Laws
instance Monad Counter where
  return val = Counter 0 val
  -- counting happens via the bind operator and the inc/dec functions
  -- for other function calls, the counter adds the count returned by that function
  -- while also using that function's return value
  (Counter c val) >>= f = Counter (c + c') val'
    where (Counter c' val') = f val
