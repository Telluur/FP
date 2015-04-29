import Data.Char
import Data.List

--1
myfilter :: (a -> Bool) -> [a] -> [a] 
myfilter _ [] = []  
myfilter p (x:xs)   
    | p x       = x : myfilter p xs  
    | otherwise = myfilter p xs  

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f acc []     = acc
myfoldr f acc (x:xs) = f x (myfoldr f acc xs)

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f acc []     =  acc
myfoldl f acc (x:xs) =  myfoldl f (f acc x) xs

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]  
myzipWith _ [] _ = []  
myzipWith _ _ [] = []  
myzipWith f (x:xs) (y:ys) = f x y : myzipWith f xs ys 

--2
--a
db = [
	("Rick", 19, "male", "Enschede"),
	("Albert", 21, "male", "Amsterdam"),
	("Bert", 25, "male", "Rotterdam"),
	("Celine", 99, "female", "Zevenaar"),
	("Judith", 38, "female", "London"),
	("Erika", 40 , "female", "Hengelo"),
	("Frans", 32, "male", "Arnhem"),
	("Stan", 10, "male", "Nijmegen"),
	("Veronica", 35, "female", "Wehl"),
	("Michiel", 20, "male", "Badhoevedorp")
	]

--b
name :: (Num a, Ord a) => ([Char],a,[Char],[Char]) -> [Char]
name (name, age, sex, place) = name

age :: (Num a, Ord a) => ([Char],a,[Char],[Char]) -> a
age (name, age, sex, place) = age
	
sex :: (Num a, Ord a) => ([Char],a,[Char],[Char]) -> [Char]
sex (name, age, sex, place) = sex
	
place :: (Num a, Ord a) => ([Char],a,[Char],[Char]) -> [Char]
place (name, age, sex, place) = place

--c
--recursion
increaseAgeR :: (Num a) => a -> [([Char],a,[Char],[Char])] -> [([Char],a,[Char],[Char])]
increaseAgeR n [] = []
increaseAgeR n ((a,b,c,d):xs) = (a,b+n,c,d) : increaseAgeR n xs 

--list comprehension
increaseAgeC :: (Num a) => a -> [([Char],a,[Char],[Char])] -> [([Char],a,[Char],[Char])]
increaseAgeC n db = [ (a,b+n,c,d) | (a,b,c,d) <- db]

--higher order function
increaseAgeHO :: (Num a) => a -> [([Char],a,[Char],[Char])] -> [([Char],a,[Char],[Char])]
increaseAgeHO n db = map (\(a,b,c,d) -> (a,b+n,c,d)) db

--d
womenBetween30And40 :: (Num a, Ord a) => ([Char],a,[Char],[Char]) -> Bool
womenBetween30And40 (_,b,c,_) = (b > 30) && (b < 40) && (c == "female")

--recursion
womenR :: (Num a, Ord a) => [([Char],a,[Char],[Char])] -> [([Char],a,[Char],[Char])]
womenR [] = []
womenR ((a,b,c,d):xs)
	| womenBetween30And40 (a,b,c,d) = (a,b,c,d) : womenR xs
	| otherwise = womenR xs 
	
--list comprehension 
womenC :: (Num a, Ord a) => [([Char],a,[Char],[Char])] -> [([Char],a,[Char],[Char])]
womenC db = [ (a,b,c,d) | (a,b,c,d) <- db, womenBetween30And40 (a,b,c,d)]

--higher order function
womenHO :: (Num a, Ord a) => [([Char],a,[Char],[Char])] -> [([Char],a,[Char],[Char])]
womenHO db = filter (womenBetween30And40) db

--e
ageByName :: (Num a, Ord a) => [Char] -> [([Char],a,[Char],[Char])] -> a
ageByName name ((a,b,c,d):xs)
	| (map toLower name) == (map toLower a) = b
	| otherwise = ageByName name xs

--f (Not the best implementation :D) 
sortByAge :: (Num a, Ord a) => [([Char],a,[Char],[Char])] -> [([Char],a,[Char],[Char])]
sortByAge db = map swap1stAnd2nd $ sort $ map swap1stAnd2nd db

swap1stAnd2nd :: (a,b,c,d) -> (b,a,c,d)
swap1stAnd2nd (a,b,c,d) = (b,a,c,d)

--3
--a
sieve [] = []
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0]

primes :: [Integer]
primes = sieve [2..]

isPrime :: Integer -> Bool
isPrime n = null [ x | x <- [2..n - 1], n `mod`x  == 0]

firstNPrimes :: Int -> [Integer]
firstNPrimes n = take n primes

primesUnder :: Integer -> [Integer]
primesUnder n = sieve [2..n-1]

--b
dividersC :: Integer -> [Integer]
dividersC n = [ x | x <- [1..n `div` 2], (n `rem` x) == 0]

dividersHO :: Integer -> [Integer]
dividersHO n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

isPrimeAlt :: Integer -> Bool
isPrimeAlt n = 1 == (length $ dividersHO n)

--4
--a
pythDup :: Int -> [(Int,Int,Int)]
pythDup n = [ (a,b,c) | a <- [1..n] , b <- [1..n], c <- [1..n], a^2 + b^2 == c^2 ]

{-
List comprehensions in the form of [ (x,y) | x <- [1,2], y <- [1..]] yields [(1,1),(1,2),(1,3),..]
If we were to make the pyth function infinite, b and c would never inflate, thus never finding any solutions.
-}

--b
{-
When generating Pythagorean Triples, the only symmetries that can arise are in the form of (a,b,c) -> (b,a,c)
If we make sure that b is always equal or greater than a, no duplicates can exists. 
Optimization: C must always be larger than a and b. So we only generate from b to n.
Possible optimization: Remove generator for c and check if a^2 + b^2 is a natural number.
-}
pyth :: Int -> [(Int,Int,Int)]
pyth n = [ (a,b,c) | a <- [1..n] , b <- [a..n], c <- [b..n], a^2 + b^2 == c^2]

--5
--a
list5a1 = [1,3,5,7,9,11,13,15]
list5a2 = [1,3,5,3,2,3]

increasing :: (Num a, Ord a) => [a] -> Bool
increasing [x] = True
increasing (x:y:xs)
	| x < y = increasing (y:xs)
	| otherwise = False
	
--b
list5b1 = [1,2,2,3,3,4,4,5,5]
list5b2 = [1,2,3,4,3,7,8,9]

--Weird typecast in de eerste guard kan nooit de bedoeling zijn...
weaklyDecreasing :: (Ord a, Num a, Integral a) => [a] -> Bool 
weaklyDecreasing [x] = True
weaklyDecreasing list@(x:xs)
	| fromIntegral x > ((fromIntegral $ sum xs) / (fromIntegral $ length xs)) = weaklyDecreasing xs
	| otherwise = False
	
weaklyIncreasing :: (Ord a, Num a, Integral a) => [a] -> Bool
weaklyIncreasing list = weaklyDecreasing $ reverse list

--6
--a
list61 = [1,9,2,8,3,7,4,6,5]	--List
list62 = [1,9,2]				--Sublist
list63 = [7,4,6]				--Sublist
list64 = [1,4,5]				--Partial
list65 = [5,4,3]				--Neither

isSublist :: (Eq a, Num a) => [a] -> [a] -> Bool
isSublist [] _ = False
isSublist _ [] = False
isSublist  sub@(x:xs) list@(y:ys) = prec sub list|| isSublist sub ys

prec :: (Eq a, Num a) => [a] -> [a] -> Bool
prec [] [] = True
prec [] (x:xs)  = True
prec sub@(x:xs) list@(y:ys) 
	| x == y = prec xs ys
	| otherwise = False

--b
isPartialSublist :: (Eq a, Num a) => [a] -> [a] -> Bool
isPartialSublist [] [] = True
isPartialSublist [] _ = True
isPartialSublist _ [] = False
isPartialSublist sub@(x:xs) list@(y:ys)
	| x == y = isPartialSublist xs ys
	| otherwise = isPartialSublist sub ys
	
--7
--a
bubble :: (Ord a) => [a] -> [a]
bubble (x:y:xs)
    | x > y = y : bubble (x:xs)
    | otherwise = x : bubble (y:xs)
bubble (x) = (x)

--helper function for applying bubble (length n) times.
bubblehelp :: (Ord a) => [a] -> Int -> [a]
bubblehelp xs i 
    | i == (length xs) = xs
    | otherwise = bubblehelp (bubble xs) (i + 1) 
 
bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs = bubblehelp xs 0

--b
mmsort :: (Ord a) => [a] -> [a]
mmsort [] = []
mmsort [x] = [x]
mmsort list = minimum list : mmsort (list \\ [(minimum list),(maximum list)]) ++ [maximum list]

--c
ins :: (Ord a) => a -> [a] -> [a]
ins x (y:ys)
    | x > y     = y : insert x ys
    | otherwise = x : y : ys

isort :: (Ord a) => [a] -> [a]
isort = foldr insert []

--d
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
	| x < y = x : merge xs (y:ys)
	| otherwise = y : merge (x:xs) ys

msort :: (Ord a) => [a] -> [a]
msort list
	| length list > 1 = merge (msort half1) (msort half2)
	| otherwise = list
	where
	n = length list `div` 2
	half1 = take n list
	half2 = drop n list

--e
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]