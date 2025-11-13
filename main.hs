import Data.List
import System.Random
import Test.HUnit

data ITree = ExNode Int | InNode ITree ITree Int Double
  deriving (Show)

type Dataset = [[Double]]
type Column = [Double]

x :: Dataset
x = [[1.2, 1.1], [1.9, 0.2], [2.2, 0.1], [1.9, 0.2]]

getColumn :: Dataset -> Int -> Column
getColumn x i = map (\l -> l !! i) x

initialGenerator :: StdGen
initialGenerator = mkStdGen 44

-- TODO: Test this function
splits :: Column -> [Double]
splits xs = map (\t -> (fst t + snd t) / 2) (zip ys (tail ys))
   where ys = sort xs

train :: StdGen -> Dataset -> Int -> Int -> ITree
train gen x e l | e >= l || length x <= 1  = ExNode (length x)
                | otherwise = ExNode q
    where (q, gen_new) = randomR (0 :: Int, (numberOfFeatures x) - 1:: Int) gen
          column = getColumn x q
          splitPoints = splits column
-- (p, gen_return) = randomR (minimum splitPoints :: Double, maximum splitPoints :: Double) gen_new

main = print $
     train initialGenerator x 0 10


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

-- main :: IO ()
-- main = do
--   counts <- runTestTT tests
--   print counts    
