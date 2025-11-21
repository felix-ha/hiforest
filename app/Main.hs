module Main where

import System.Random
import Lib

initialGenerator :: StdGen
initialGenerator = mkStdGen 44

x :: Dataset
x = [[1.2, 1.1], [1.9, 0.2], [2.2, 0.1], [1.9, 0.2]]

y :: Column
y = [0.01, 9.2]

iTree :: ITree
iTree = fitITree initialGenerator x 0 1

l :: Double
l = pathLength y iTree 0

-- TODO iTree looks good, pathLength is -Infinty
main :: IO ()
main = print $
     l


-- testFeatures :: Test
-- testFeatures = TestCase (assertEqual "Test numberOfFeatures for x" 2 (numberOfFeatures x))
--
-- testisValidDataset:: Test
-- testisValidDataset = TestCase (assertEqual "Test isValidDataset for x" True (isValidDataset x))
--
-- tests :: Test
-- tests = TestList [TestLabel "testFeatures" testFeatures, 
--                   TestLabel "testIsValid" testisValidDataset]

-- main :: IO ()
-- main = do
--   counts <- runTestTT tests
--   print counts    
