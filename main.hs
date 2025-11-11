import Test.HUnit

data ITree = ExNode Integer | InNode ITree ITree Integer Double
  deriving (Show)

type Dataset = [[Double]]

x :: Dataset
x = [[1.2, 1.1], [1.9, 0.2], [2.2, 0.1], [1.9, 0.2]]

distinctValues :: Eq a => [a] -> [a] -> [a]
distinctValues [] ds = ds
distinctValues (x:xs) ds | x `elem` ds = distinctValues xs ds 
                         | otherwise = distinctValues xs (x:ds) 

-- TODO: Return value if distinctValues equals 1
numberOfFeatures :: Dataset -> Int
numberOfFeatures x = length (distinctValues (map length x) [])


testFeatures :: Test
testFeatures = TestCase (assertEqual "Test numberOfFeatures for x" 2 (numberOfFeatures x))

tests :: Test
tests = TestList [TestLabel "testFeatures" testFeatures]

main :: IO ()
main = do
  counts <- runTestTT tests
  print counts    
