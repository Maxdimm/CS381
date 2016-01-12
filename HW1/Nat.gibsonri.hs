module Nat where

import Prelude hiding (Enum(..), sum)


--
-- * Part 1: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 0.
zero :: Nat
zero = Zero

-- | The number 1.
one :: Nat
one = Succ zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--
--   >>> pred zero
--   Zero
--
--   >>> pred three
--   Succ (Succ Zero)
--
pred :: Nat -> Nat
pred (Succ n) = n
pred Zero = Zero

-- | True if the given value is zero.
--
--   >>> isZero zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero (Succ _) = False
isZero Zero = True

-- | Convert a natural number to an integer.
--
--   >>> toInt zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Integer
toInt = intAcc 0

-- Support function for toInt.
intAcc :: Integer -> Nat -> Integer
intAcc i (Succ n) = intAcc (i+1) n
intAcc i Zero = i

-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--
add :: Nat -> Nat -> Nat
add n Zero = n
add n (Succ m) = add (Succ n) m

-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub n Zero = n
sub Zero _ = Zero
sub (Succ n) (Succ m) = sub n m

-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
--   >>> gt Zero Zero
--   False
gt :: Nat -> Nat -> Bool
-- Why is the case gt Zero Zero = False not needed?
gt n m = sub n m /= Zero


-- | Multiply two natural numbers.
--
--   >>> mult two zero
--   Zero
--
--   >>> mult zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
-- mult m n should add m to itself n times.
-- Need an accumulator
mult :: Nat -> Nat -> Nat
mult = multAcc Zero

-- multAcc l _ Zero = l
-- multAcc Zero m (Succ n) = multAcc m m n
multAcc :: Nat -> Nat -> Nat -> Nat
multAcc m _ Zero = m
multAcc m n (Succ o) = multAcc (add m n) n o

-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--
--   >>> sum [one,zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum = foldr add Zero


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat]
odds = gen nextOdd one

-- An infinitely long "list" of successive function applications on an initial value.
gen :: (a -> a) -> a -> [a]
gen f s = s : gen f (f s)

nextOdd :: Nat -> Nat
nextOdd Zero = one
nextOdd (Succ n) = Succ (Succ (Succ n))
