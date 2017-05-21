import Lib
import Test.QuickCheck
import Control.Monad

prop_succIncreasing :: Natural -> Bool
prop_succIncreasing n = (n < (suc n))

prop_addBiggerEq :: Natural -> Natural -> Bool
prop_addBiggerEq n m = (n `add` m) >= n

type N = Natural
type BinOp = N -> N -> N

prop_Commutative :: BinOp -> N -> N -> Bool
prop_Commutative op n m = (op n m) == (op m n)

prop_Associative :: BinOp -> N -> N -> N -> Bool
prop_Associative op a b c = ((a `op` b) `op` c) == (a `op` (b `op` c))

prop_neutralElement :: BinOp -> N -> N -> Bool
prop_neutralElement op e n = (e `op` n) == n && (n `op` e) == n

instance Arbitrary Natural where
    arbitrary = oneof [return zero, liftM suc arbitrary]

main = do
    quickCheck prop_succIncreasing
    quickCheck (prop_Commutative mul)
    quickCheck (prop_Commutative add)
    quickCheck (prop_Associative mul)
    quickCheck (prop_Associative add)
    quickCheck (prop_neutralElement add zero)
    quickCheck (prop_neutralElement mul one)
