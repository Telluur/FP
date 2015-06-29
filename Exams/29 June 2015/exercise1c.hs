module Exercise1c where

palindroom :: Integral a => a -> Bool
palindroom n = n' == reverse n'
  where
    n' = digits n

digits :: Integral a => a -> [a]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]
