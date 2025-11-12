import Test.HUnit

data ITree = ExNode Integer | InNode ITree ITree Integer Double
  deriving (Show)

type Dataset = [[Double]]

x :: Dataset
x = [[1.2, 1.1], [1.9, 0.2], [2.2, 0.1], [1.9, 0.2]]

numberOfFeatures :: Dataset -> Int
numberOfFeatures x | length distinct_values == 1 = distinct_values !! 0
                   | otherwise = -1
   where distinct_values = distinctValues (map length x) []

distinctValues :: Eq a => [a] -> [a] -> [a]
distinctValues [] ds = ds
distinctValues (x:xs) ds | x `elem` ds = distinctValues xs ds 
                         | otherwise = distinctValues xs (x:ds) 

isValidDataset :: Dataset -> Bool
isValidDataset ds = length distinct_values == 1
   where distinct_values = distinctValues (map length x) []

testFeatures :: Test
testFeatures = TestCase (assertEqual "Test numberOfFeatures for x" 2 (numberOfFeatures x))

testisValidDataset:: Test
testisValidDataset = TestCase (assertEqual "Test isValidDataset for x" True (isValidDataset x))

tests :: Test
tests = TestList [TestLabel "testFeatures" testFeatures, 
                  TestLabel "testIsValid" testisValidDataset]

main :: IO ()
main = do
  counts <- runTestTT tests
  print counts    
