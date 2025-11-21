module Lib where

import Data.List
import System.Random

data ITree = ExNode Int | InNode ITree ITree Int Double
  deriving (Show)

type Dataset = [[Double]]
type Column = [Double]


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
   where distinct_values = distinctValues (map length ds) []

getColumn :: Dataset -> Int -> Column
getColumn x i = map (\l -> l !! i) x

randomFeature :: StdGen -> Dataset -> (Int, StdGen)
randomFeature gen x = randomR (0 :: Int, (numberOfFeatures x) - 1 :: Int) gen

splits :: Column -> [Double]
splits xs = map (\t -> (fst t + snd t) / 2) (zip ys (tail ys))
   where ys = sort xs

-- Probably not correct, because randomR generates a random number between the range of lo and hi. Randomly select a elemt of splitPoints may be better.
randomSplit :: StdGen -> [Double] -> (Double, StdGen)
randomSplit gen splitPoints = randomR (minimum splitPoints :: Double, maximum splitPoints :: Double) gen

fitITree :: StdGen -> Dataset -> Int -> Int -> ITree
fitITree gen x e l | e >= l || length x <= 1  = ExNode (length x)
                | otherwise = InNode (fitITree gen_return x_left (e + 1) l) (fitITree gen_return x_right (e + 1) l) q p
    where (q, gen_new) = randomFeature gen x 
          column = getColumn x q
          splitPoints = splits column
          (p, gen_return) = randomSplit gen_new splitPoints
          x_left = filter (\r -> (r !! q) < p) x
          x_right = filter (\r -> (r !! q) >= p) x

h :: Double -> Double
h n = log n + 0.5772156649

c :: Double -> Double
c n = 2 * h (n-1) - (2 * (n-1) / n)

pathLength :: Column -> ITree -> Double -> Double
pathLength _ (ExNode n) e = e + c (fromIntegral n)
pathLength x (InNode iTreeLeft iTreeRight q p) e | (x !! q) < p = pathLength x iTreeLeft (e+1)
                                                 | otherwise = pathLength x iTreeRight (e+1)

