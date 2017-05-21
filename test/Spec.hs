import Lib
import Test.QuickCheck
import Control.Monad

instance Arbitrary Natural where
    arbitrary = oneof [return zero, return one, return three, return six, liftM suc arbitrary]

type N = Natural
type BinOp = N -> N -> N

prop_succIncreasing :: N -> Bool
prop_succIncreasing n = (n < (suc n))

prop_addBiggerEq :: N -> N -> Bool
prop_addBiggerEq n m = (n `add` m) >= n

prop_Commutative :: BinOp -> N -> N -> Bool
prop_Commutative op n m = (op n m) == (op m n)

prop_Associative :: BinOp -> N -> N -> N -> Bool
prop_Associative op a b c = ((a `op` b) `op` c) == (a `op` (b `op` c))

prop_neutralElement :: BinOp -> N -> N -> Bool
prop_neutralElement op e n = (e `op` n) == n && (n `op` e) == n

prop_leftDistributive :: N -> N -> N -> Bool
prop_leftDistributive a b c = (a `add` b) `mul` c == (a `mul` c) `add` (b `mul` c)

main = do
    quickCheck prop_succIncreasing
    quickCheck (prop_Commutative mul)
    quickCheck (prop_Commutative add)
    quickCheck (prop_Associative mul)
    quickCheck (prop_Associative add)
    quickCheck (prop_neutralElement add zero)
    quickCheck (prop_neutralElement mul one)
    quickCheck prop_leftDistributive
