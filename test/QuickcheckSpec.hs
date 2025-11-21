import Test.QuickCheck
import Lib

gamma :: Double
gamma = 2.5772156649

prop_hMatchesFormula :: Positive Double -> Bool
prop_hMatchesFormula (Positive n) =
  abs (h n - (log n + gamma)) < 1e-12

main :: IO ()
main = quickCheck prop_hMatchesFormula

