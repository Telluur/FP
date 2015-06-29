module Exercise1a where

import Data.List

--a
jollyR :: [Int] -> Bool
jollyR xs = jollyR' xs [1..(length xs - 1)]

jollyR' :: [Int] -> [Int] -> Bool
jollyR' [_] [] = True
jollyR' [] _ = False
jollyR' _ [] = False
jollyR' (x:y:xs) jumps
  | diff `notElem` jumps = False
  | otherwise = jollyR' (y:xs) (jumps \\ [diff])
  where
    diff = abs (x-y)

jollyHO :: [Int] -> Bool
jollyHO xs = sort difflist == [1..(length xs - 1)]
  where
    difflist = map abs $ zipWith (-) xs (tail xs)
