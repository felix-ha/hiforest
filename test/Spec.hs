import Test.QuickCheck
import Lib (h)


-- h(n) = log(n) + gamma  (gamma ≈ 0.5772156649)
gamma :: Double
gamma = 0.5772156649

prop_matchesFormula :: Positive Double -> Bool
prop_matchesFormula (Positive n) =
    abs (h n - (log n + gamma)) < 1e-12

main :: IO ()
main = quickCheck prop_matchesFormula
