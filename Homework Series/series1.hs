import Data.Char
--1
f :: Int -> Int
f x = 2*x^2 + 3*x - 5

--2
code :: Int -> Char -> Char
code n char
	| x >= 65 && x <= 90 = chr((x - 65 + n) `mod` 26 + 65)
	| x >= 97 && x <= 122 = chr((x - 97 + n) `mod` 26 + 97)
	| otherwise = char
	where
	x = ord char
--3
interest :: Int -> Float -> Float -> Float
interest n a r
	| n == 0 = a
	| otherwise = interest (n-1) (a + a / 100 * r) r

--4
root1 :: Float -> Float -> Float -> Float
root1 a b c
	| discr a b c >= 0 = ((-b) + sqrt(b ^ 2 - 4 * a * c)) / (2 * a)
	| otherwise = error "negative discriminant"

root2 :: Float -> Float -> Float -> Float
root2 a b c
	| discr a b c >= 0 = ((-b) - sqrt(b ^ 2 - 4 * a * c)) / (2 * a)
	| otherwise = error "negative discriminant"

discr :: Float -> Float -> Float -> Float
discr a b c = b ^ 2 - 4 * a * c

--5
extrX :: Float -> Float -> Float
extrX a b = b / (2 * a)

extrY :: Float -> Float -> Float -> Float
extrY a b c = c - b ^ 2 / (4 * a)

--6
mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = mylength xs + 1

mysum :: [Int] -> Int
mysum [x] = x
mysum (a:b:xs) = mysum((a+b) : xs)

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = reverse xs ++ [x]

mytake :: Int -> [a] -> [a]
mytake n _
    | n <= 0 = []
mytake _ [] = []
mytake n (x:xs) = x : mytake (n-1) xs

myelem :: (Eq a) => a -> [a] -> Bool
myelem a [] = False
myelem a (x:xs)
	| a == x = True
	| otherwise = myelem a xs

myconcat :: [[a]] -> [a]
myconcat ([]) = []
myconcat (x:xs) = x ++ myconcat xs

mymaximum :: [Int] -> Int
mymaximum [] = error "Empty list"
mymaximum [x] = x
mymaximum (x:y:xs)
	| x > y = mymaximum(x : xs)
	| otherwise = mymaximum(y : xs)

myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y):myzip xs ys

--7
r :: (Num a, Enum a) => a -> a -> [a]
r a d = [a + (d*i) | i <- [0..]]

-- r1 is niks anders dan de !! operator? Of moet hier een eigen functie geschreven worden die over de lijst itereert?

r1 :: (Num a) => Int -> [a] -> a
r1 n r = r !! n

total :: (Num a, Enum a) => Int -> Int -> a -> a -> a
total i j a d = sum $ drop i $ take (j+1) (r a d)

--8

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:y:xs) = x==y && allEqual (y:xs)

allDiffs :: (Num a) => [a] -> [a]
allDiffs [] = []
allDiffs [x] = []
allDiffs (x:y:xs) = (y-x) : allDiffs (y:xs)

allDiffs2 :: (Num a) => [a] -> [a]
allDiffs2 l@(x:xs) = zipWith (-) xs l

isAS :: (Num a, Eq a) => [a] -> Bool
isAS = allEqual . allDiffs

--9
checkRows :: (Eq a) => [[a]] -> Bool
checkRows [] = error "Empty matrix"
checkRows [a] = True
checkRows (m:mm:ms)
	| mylength m == mylength mm = checkRows(mm:ms)
	| otherwise = False

sumRows :: [[Int]] -> [Int]
sumRows = map mysum

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

--Lazy and ineffective way
sumCols :: [[Int]] -> [Int]
sumCols = sumRows . transpose

--Better
sumCols2 :: [[Int]] -> [Int]
sumCols2 ([]:_) = []
sumCols2 x = mysum(map head x) : sumCols2 (map tail x)
