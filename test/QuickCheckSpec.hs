import Test.QuickCheck
import System.Exit (exitFailure, exitSuccess)

import Lib

gamma :: Double
gamma = 0.5772156649

prop_hMatchesFormula :: Positive Double -> Bool
prop_hMatchesFormula (Positive n) =
  abs (h n - (log n + gamma)) < 1e-12

main :: IO ()
main = do
  result <- quickCheckResult prop_hMatchesFormula
  case result of
    Success {} -> exitSuccess
    _          -> exitFailure

