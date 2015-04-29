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
increaseAge :: (Num a) => a -> ([Char],a,[Char],[Char]) -> ([Char],a,[Char],[Char])
increaseAge n (a,b,c,d) = (a,b+n,c,d)

increaseAgeHO :: (Num a) => a -> [([Char],a,[Char],[Char])] -> [([Char],a,[Char],[Char])]
increaseAgeHO n db = map (increaseAge n) db

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
sortByAge db = swap1stAnd2nd $ sort $ swap1stAnd2nd db
		
swap1stAnd2nd :: [(a,b,c,d)] -> [(b,a,c,d)]
swap1stAnd2nd [] = []
swap1stAnd2nd ((a,b,c,d):xs) = (b,a,c,d) : swap1stAnd2nd xs

--3
--a
sieve [] = []
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0]

primes :: [Integer]
primes = sieve [2..]

isPrime :: Integer -> Bool
isPrime n = null [ x | x <- [2..n - 1], n `mod`x  == 0]

firstNPrimes n = take n primes

primesUnder :: Integer -> [Integer]
primesUnder n = sieve [2..n-1]

dividersC :: Integer -> [Integer]
dividersC n = [ x | x <- [1..n `div` 2], (n `rem` x) == 0]

dividersHO :: Integer -> [Integer]
dividersHO n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

--isPrimeAlt :: Integer -> Bool
isPrimeAlt n = 1 == (length $ dividersHO n)



