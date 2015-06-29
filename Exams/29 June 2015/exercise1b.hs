module Exercise1b where

import Data.List

powerList :: Ord a => [a] -> [[a]]
powerList list = [l:list' | l <- list, list' <- []:powerList(filter (l <) (list \\ [l]))]
