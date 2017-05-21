module Lib
    ( 
      Natural,
      suc, add, mul,
      zero, one, two, three, four, five, six,
    ) where

-- we want to overload (+) and (*)
-- the trouble is that these are defined in Num along with other functions like
-- negate
-- which are problematic for natural numbers

data Natural = Zero | Succ Natural deriving(Show, Eq)

suc = Succ
add Zero n = n
add (Succ m) n = Succ (add m n)
mul Zero n = Zero
mul (Succ m) n = add n (mul m n) 

-- convenience
zero = Zero
one = suc zero
two = suc one
three = suc two
four = suc three
five = suc four
six = suc five
    
instance Ord Natural where
    Zero <= n = True
    (Succ n) <= Zero = False
    (Succ n) <= (Succ m) = n <= m
