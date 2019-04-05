{-
READ ME: Change the name of this file to YOURSTUDENTNUMBER.hs. For example, if your
student number were 123456789, then you would rename the file 123456789.hs.

REPLACE the function definitions for each of the questions. 
The names of the functions correspond to the names given in the document cwk19handout.pdf. 

DO NOT CHANGE THE TYPE SIGNATURES, OR THE ORDER OF THE ARGUMENTS!

You may add as many extra or helper functions as you wish, but do not make any "import" statements.
-}


-- ****** HELPER FUNCTIONS ****** --

subset :: (Eq a) => [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys
    | elem x ys = subset xs ys
    | otherwise False

count :: (Eq a) => a -> [a] -> Int
count x [] = 0
count x ys = length (filter(==x)ys)

flipPair :: (Eq a) => (a,a) -> (a,a)
flipPair (x,y) = (y,x)

addAtEnd :: a -> [a] -> [a]
addAtEnd x [] = [x]
addAtEnd x (y:ys) = y:addAtEnd x ys

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = append xs : addAtEnd x ys

divides :: Int -> Int -> Bool
divides x y = rem x y == 0

isComposite :: Int -> Bool
isComposite n = foldl (||) False (map (divides n) [2..(n-1)])

isCoPrime :: Int -> Int -> Bool
isCoPrime x y
    | gcd x y == 1 = True
    | otherwise = False

isPrime :: Int -> Bool
isPrime n
    | n <= 0 = error "Are you high? That's never going to work!"
    | otherwise = not (isComposite n)

-- QUESTION 1: Sets

complement :: (Eq a) => [a] -> [a] -> Maybe [a]
complement [] ys = Just ys
complement xs ys 
    | subset xs ys = Just (filter (not.(`elem`xs))ys)
    | otherwise = Nothing

toMultiset :: (Eq a) => [a] -> [(a,Int)]
toMultiset [] = []
toMultiset (x:xs) = (x, count(x:xs)):(toMultiset ((filter(not.(==x))) xs))

mIntersect :: (Eq a) => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
mIntersect xs ys = error "You've not tried to write mUnion yet"

-- TEST SET FOR Q1
{-
Your functions should have the following behaviour:
complement [1,2,3] [1..5] = Just [4,5]
complement [1,2,3] [2..5] = Nothing
toMultiset [1,1,1,2,4,1,2] = [(1,4),(2,2),(4,1)]
toMultiset "from each according to his ability, to each according to his needs" = [('f',1),('m',1),('b',1),('l',1),('y',1),(',',1),('a',5),('c',6),('r',3),('g',2),('t',4),('o',6),('h',4),('i',6),(' ',11),('n',3),('e',4),('d',3),('s',3)]
mIntersect [(1,6),(2,3)] [(1,2),(2,5),(3,1)] = [(1,2),(2,3)]
mIntersect [(1,2),(4,1)] [(1,1),(4,2)] = [(1,1),(4,1)]
 
THE ORDER OF ELEMENTS IN THE RESULTS OF mUnion IS NOT IMPORTANT.
-}



-- QUESTION 2: Functions and relations

transClosure :: (Eq a) => [(a,a)] -> [(a,a)]
transClosure xs = error "You've not tried to write symClosure yet"

-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:
transClosure [(1,2),(2,3)] = [(1,2),(2,3),(1,3)]
transClosure [(1,1),(3,5), (5,3)] = [(1,1),(3,5),(5,3),(3,3),(5,5)]

DO NOT WORRY ABOUT THE ORDER IN WHICH PAIRS APPEAR IN YOUR LIST
-}



-- QUESTION 3: Combinatorics

missing2 :: [Int] -> [[Int]]
missing2 xs = error "You've not tried to write choose2 yet"

-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
missing2 [1,2,3] = [[1],[2],[3]]
missing2 [2,6,9,12] = [[9,12],[6,12],[6,9],[2,12],[2,9],[2,6]]
NOTE THAT THE SMALLER LISTS ARE SORTED. THE ORDERING OF THE LISTS IN THE BIG LIST DOES NOT MATTER.
-}




-- QUESTION 4: Primes

factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime

nextPrimes :: Int -> [Int]
nextPrimes n
    | n <= 99 = error "You've not tried to write nextPrimes for 2 digit numbers"
    | n <= 9999 = error "You've not tried to write nextPrimes for 4 digit numbers"
    | n <= 99999 = error "You've not tried to write nextPrimes for 5 digit numbers"
    | n <= 999999 = error "You've not tried to write nextPrimes for 6 digit numbers"
    | otherwise = error "You've not tried to write nextPrimes for big numbers yet"

primeFactorisation :: Int -> [Int]
primeFactorisation n = factor n

{- 
Leave the error messages in place if you do not want to attempt the parts for the input size. You should remove the guards up to the point you want to attempt. For example, if you were confident of anything up to five digits, the function would look like:

primeFactorisation n
    | n <= 99999 = whatever_your_calculation_is
    | n <= 999999 = error "..."
    | otherwise = error "..."

 -}




-- TEST SET FOR Q4
{-
Your functions should have the following behaviour:
nextPrimes 75 = [79,83,89]
nextPrimes 64 = [67,71,73]
primeFactorisation 75 = [3,5,5]
primeFactorisation 64 = [2,2,2,2,2,2]
-}




-- QUESTION 5: RSA

eTotient :: Int -> Int
eTotient n
    | isPrime n = n-1
    | otherwise = length (filter (isCoPrime n) [0..n])

encode :: Int -> Int -> Int -> Int -> Maybe Int
encode p q m e 
    | (isPrime p) && (isPrime q) && (isCoPrime e (eTotient (p*q))) && (1<e) && (e<eTotient(p*q)) = Just ((m^e) `mod` (p*q))
    | otherwise = Nothing

-- TEST SET FOR Q5
{-
Your functions should have the following behaviour:
eTotient 54 = 18
eTotient 73 = 72
encode 37 23 29 5 = Just 347
encode 99 18 108 45 = Nothing
encode 37 17 23 48 = Nothing
-}


