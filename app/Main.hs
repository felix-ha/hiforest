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


