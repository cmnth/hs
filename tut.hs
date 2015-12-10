{-# LANGUAGE TemplateHaskell #-}
import           Data.List
import           System.IO
import           Data.Char hiding (toUpper,isDigit)
import           Debug.HTrace
import           Test.HUnit
import           Test.QuickCheck

-- Int -2^63  2^63
maxInt = maxBound :: Int
minInt = minBound :: Int

-- Double
bigFloat = 3.99999999999 + 0.000000000005

always5 :: Int
always5 = 5

sumOfNums = sum [1..1000]

addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

modEx = mod 5 4
modEx2 = 5 `mod` 4

negNumEx = 5 + (-4)

num9 = 9 :: Int
sqrtof9 = sqrt (fromIntegral num9)

primeNumbers = [3,5,7,11]

morePrime = primeNumbers ++ [13,17,19,23,29]

favNums = 2 : 7 : 21 : 66 :[]

multList = [[3,5,7],[11,13,17]]

morePrimes = 2 : morePrime

lenPrime = length morePrimes

revPrime = reverse morePrimes

isListEmpty = null morePrimes

secondPrime = morePrimes !! 1
firstPrime = head morePrimes
lastPrime = last morePrimes
first3Primes = take 3 morePrimes
removedPrimes = drop 3 morePrimes
is7InList = 7 `elem` morePrimes

zeroToTen = [0..10]
evenList = [2,4..20]
letterList = ['A','C'..'Z']

many2s = take 10 (repeat 2)
many3s = replicate 10 3
cycleList = take 10 (cycle [1,2,3,4,5])
listTimes2 = [x * 2 | x <- [1..10]]
listTimes3 = [x * 3 | x <- [1..10]]
listTimes4 = [x * 4 | x <- [1..10]]

listTimes3a = [x * 3 | x <- [1..500], x * 3 <= 80]

divisBy9n13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [9,1,8,3,4,7,6]

sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]
sumOfLists2 = zipWith (*) [1,2,3,4,5] [6,7,8,9,10]

listBiggerThan5 = filter (>5) morePrimes
evensUpTo20 = takeWhile (<= 20) [2,4..]
multOfList = foldl (-) 0 [1..10]
multOfList2 = foldr (-) 0 [1..10]

foldlHello = foldl (flip (:)) "!" "Hello"
foldrHello = foldr (:) "!" "Hello"

pow3List = [3^n | n <- [1..10]]
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]
randTuple = (1, "Random Tuple")
bobSmith = ("Bob Smith", 52)
bobsName = fst bobSmith
bobsAge = snd bobSmith
names = ["Bob","Mary","Tom"]
addresses = ["123 Main","234 North","567 South"]
namesNAddress = zip names addresses

addMe :: Int -> Int -> Int

-- funcName param1 param2 = operations (return value)

addMe x y = x + y

sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge _ = "Nothing Important"

factorial :: Int -> Int
-- factorial 0 = 1
-- factorial n = n * factorial (n - 1)

factorial n
  | n == 1    = 1
  | otherwise = n * factorial (n - 1)

-- 3 * 2 = 6
-- 2 * 1 = 2
-- 1 * factorial(0) : 1

prodFact n = product [1..n]

isOdd, isEven :: Int -> Bool
-- isOdd n
    -- | n `mod` 2 == 0 = False
    -- | otherwise = True

isOdd n
  | n < 0     = False
  | otherwise = isEven (n-1)

isEven n
  | n < 0     = False
  | n == 0    = True
  | otherwise = isOdd (n-1)



whatGrade :: Int -> String
whatGrade age
    | (age >= 5) && (age <= 6) = "Kindergarten"
    | (age > 6) && (age <= 10) = "Elementary School"
    | (age > 10) && (age <= 14) = "Middle School"
    | (age > 14) && (age <= 18) = "High School"
    | otherwise = "Go to college"

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
    | avg <= 0.200 = "Terrible Batting Average"
    | avg <= 0.250 = "Average Player"
    | avg <= 0.280 = "You're doing pretty good"
    | otherwise = "You're a Superstar"
    where avg = hits / atBats

getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ " and the rest are " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

times4 :: Int -> Int
times4 x = x * 4
listMapTimes4 = map times4 [1,2,3,4,5]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y
adds3 = getAddFunc 3
fourPlus3 = adds3 4
threePlusList = map adds3 [1,2,3,4,5]

dbl1To10 = map (\x -> x *2) [1..10]

doubleEvenNumber y =
  if (y `mod` 2 /= 0)
     then y
          else y * 2

dbl2To10 = map doubleEvenNumber [1..10]

multMax a b x = (max a b) * x

powersOf2 n = powersOf2loop n 1 0
powersOf2loop n x i =
  if i < n
     then powersOf2loop n (x * 2) (i + 1)
          else x

double nums =
  if null nums then []
  else (2 * (head nums)) : double (tail nums)

double2 nums = case nums of
  []       -> []
  (x : xs) -> (2 * x) : (double xs)

getClass :: Int -> String
getClass n = case n of
  5 -> "Go to Kindergarten"
  6 -> "Go to elementary school"
  _ -> "Go away"


headAndLength list = (head list, length list)

fst' (a,b) = a

pow2 n
  | n == 0    = 1
  | otherwise = 2 * (pow2 (n-1))

removeOdd [] = []
removeOdd (x:xs)
  | mod x 2 == 0  = x : (removeOdd xs)
  | otherwise     = removeOdd xs

f :: [Int] -> Int
f ls = head ls + length ls

dividesEvenly :: Int -> Int -> Bool
dividesEvenly x y = (y `div` x) * x == y

myxs = [1,2,3,4,5,6,7,8]

-- doubleMe :: [Integer] -> [Integer]
doubleMe = map (\x -> x * 2)

isDigit :: Char -> Bool
isDigit ch = (ch >= '0') && (ch <= '9')


doubleX x = x + x
quadruple x = doubleX (doubleX x)
average ns = sum ns `div` length ns


a = b + c
    where
      b = 1
      c = 2

d = a * 2

zeroto :: Int -> [Int]
zeroto n = [0..n]

second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
doublEx x = x * 2
palindrome xs = reverse xs == xs
-- twice f x = f (f x)
twice f = f . f

odds :: Int -> [Int]
-- odds n = map f [0..n - 1]
         -- where f x = x * 2 + 1
odds n = map (\x -> x * 2 + 1) [0..n - 1]

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

-- halve xs = (take n xs, drop n xs)
--           where
--n             n = length xs 'div' 2

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <-xs]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool

prime n = factors n == [1,n]

-- qsort Ord t => [t] -> [t]
qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
               where (left,right) = (filter (<=a) as, filter (>a) as)

xs'' = map (\x -> htrace (show x) x) [1..10]

s = foldl (\a b -> htrace "+" (a+b)) 0 xs''
s2 = foldl' (\a b -> htrace "+" (a+b)) 0 xs''

b = htrace "b" 2
c = htrace "c" 3
e = htrace "e" $ b + c
x = htrace "x" $ b + c

hailstone :: Integer -> Integer
hailstone n
  | even n  = n `div` 2
  | otherwise = 3 * n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = [] -- Do nothning to the empty list
sumEveryTwo [x]     = [x]  -- Do nothing to lists with a single element
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs

sumPairs :: [(Integer, Integer)] -> [Integer]
sumPairs [] = []
sumPairs ((x,y) : zs) = x + y : sumPairs zs

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs =
  and [x <= y | (x,y) <- pairs xs]


positions :: Eq a => a -> [a] -> [Int]
positions x xs =
  [i | (x',i) <- zip xs [0..n], x == x']
  where n = length xs - 1

lowers :: String -> Int
lowers xs =
  length [x | x <- xs, isLower x]

-- isLower :: Char -> Bool
-- isLower c
    -- | c >= 'a' &&  c <='z' = True
    -- | otherwise = False


findZ :: Eq a => a ->[(a,b)] -> [b]
findZ k t = [ v | (k', v) <- t, k == k']

count :: Char -> String -> Int
count x xs = length [ x' | x' <- xs, x == x']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs ]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

-- 5.7 exercises

ex571 = sum [x * 2 | x <- [1..100]]

replicateX572 n x = [x | _ <- [1..n]]

pyths n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x ^ 2 + y ^ 2 == z ^ 2]

perfects :: Int -> [Int]
perfects n =
  [ x | x <- [1..n], sum (init (factors x)) == x ]


lengthCk  = sum . map (\_ -> 1)
lengthCk2 = foldr (\_ x -> 1 + x) 0

cube :: Integer -> Integer
cube n = n * n * n

mystery :: Integer -> Integer -> Integer -> Bool
mystery m n p = not ((m==n) && (n==p))

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = ((m/=n) && (n/=p) && (m/=p))

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p q = ((m==n) && (n==p) && (p==q))

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + offset)

averageThree :: Integer -> Integer -> Integer -> Float
averageThree m n p =  (fromIntegral m + fromIntegral n + fromIntegral p) / 3


maxThree' x y z = max (max x y) z
maxThree x y z = (x `max` y) `max` z
minThree x y z = (x `min` y) `min` z

middleNumber :: Integer -> Integer -> Integer -> Integer
middleNumber x y z
  | between' y x z      = x
  | between' x y z      = y
  | otherwise          = z

between :: Integer -> Integer -> Integer -> Bool
between m n p
  | (m < n) && (n < p)   = True
  | otherwise            = False

between' :: Integer -> Integer -> Integer -> Bool
between' x y z =
  weakAscendingOrder x y z || weakDescendingOrder x y z


maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour   m n p q
  |  (m >= n) && (m >= p) && (m >= q)  = m
  |  (n >= p) && (n >= q)              = n
  |  p >= q                            = p
  |  otherwise                         = q

maxFour'  m n p q = max (max (max m n) p) q

maxFour'' m n p q = max (maxThree m n p) q

weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder m n p
  | (m <= n) && (n <=p) = True
  | otherwise           = False

weakDescendingOrder :: Integer -> Integer -> Integer -> Bool
weakDescendingOrder m n p
  | (m >= n) && (n >= p) = True
  | otherwise            = False

exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

twoEqual :: Int -> Int -> Bool
twoEqual m n = (m == n)

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m == n) && (n == p)
-- howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual m n p
  | threeEqual m n p                              = 3
  | twoEqual m n || twoEqual n p || twoEqual m p  = 2
  | otherwise                                     = 0

howManyFourEqual :: Int -> Int -> Int -> Int -> Int
howManyFourEqual m n p q
  | fourEqual m n p q                             = 4
  | threeEqual m n p    || threeEqual n p q
        || threeEqual m p q
        || threeEqual m n q                       = 3
  | twoEqual m n        || twoEqual n p
        || twoEqual m p || twoEqual p q
        || twoEqual m q || twoEqual n q           = 2
  | otherwise                                     = 0

sumSquares :: Integer -> Integer -> Integer
sumSquares n m
  = sqN + sqM
    where
      sqN = n*n
      sqM = m*m

-- Rock, Paper, Scissors

data Move = Rock | Paper | Scissors
          deriving (Show,Eq)

beat :: Move -> Move

beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

lose :: Move -> Move

lose Rock     = Scissors
lose Paper    = Rock
lose _        = Paper

data MyResult = Win | Lose | Draw
            deriving (Show,Eq)

outcome :: Move -> Move -> MyResult

outcome Rock Scissors     = Win
outcome Scissors Paper    = Win
outcome Paper Rock        = Win

outcome Scissors Rock     = Lose
outcome Paper Scissors    = Lose
outcome Rock Paper        = Lose

outcome Rock Rock         = Draw
outcome Scissors Scissors = Draw
outcome Paper Paper       = Draw


data Season = Winter | Spring | Summer | Autumn
            deriving (Show,Eq)

data Temp = Cold | Hot
          deriving (Eq, Show, Ord)

data Month = January | February | March |
             April | May | June |
             July | August | September |
             October | November | December
           deriving (Eq, Show, Ord)

seasonTemp  :: Season -> Temp

seasonTemp Winter = Cold
seasonTemp Spring = Cold
seasonTemp Summer = Hot
seasonTemp Autumn = Cold


monthSeason  :: Month -> Season

monthSeason  January   = Winter
monthSeason  February  = Winter
monthSeason  March     = Spring
monthSeason  April     = Spring
monthSeason  May       = Spring
monthSeason  June      = Summer
monthSeason  July      = Summer
monthSeason  August    = Summer
monthSeason  September = Autumn
monthSeason  October   = Autumn
monthSeason  November  = Autumn
monthSeason  December  = Winter

fac :: Integer -> Integer
fac n
  | n == 0   = 1
  | n > 0    = fac (n -1) * n

rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
  | n < m   = 0
  | n == m  = m
  | n > m  = m * (rangeProduct (m+1) n)

fac' :: Integer -> Integer
fac' n
  | n > 0    = rangeProduct 1 n
  | otherwise  = error "fac only defined on natural numbers"

power2 n
  | n == 0   = 1
  | n > 0    = 2 * power2 (n - 1)

sumFacs :: Integer -> Integer
sumFacs n
  | n == 0   = 1
  | n > 0    = sumFacs (n-1) + fac n

sumFunHOF :: (Integer -> Integer) -> Integer -> Integer
sumFunHOF f n
  | n == 0      = f 0
  | n > 0       = sumFunHOF f (n-1) + f n

regions :: Integer -> Integer
regions n
  | n == 0    = 1
  | n > 0     = regions (n-1) + n

add' :: Integer -> Integer -> Integer
add' m n = m + n

multRec :: Integer -> Integer -> Integer
multRec m n
  | n == 0 || m == 0   = 0
  | n > 0              = m + multRec m (n-1)

square :: Int -> Int
square n = n^2

sqrInt :: Int -> Int
sqrInt x = floor (sqrt (fromIntegral x))

fib :: Integer -> Integer
fib n
  | n == 0    = 0
  | n == 1    = 1
  | n > 1     = fib (n-2) + fib (n-1)

remainder :: Integer -> Integer -> Integer
remainder m n
  | m < n      = m
  | otherwise  = remainder (m-n) n

divide :: Integer -> Integer -> Integer
divide m n
  | m < n      = 0
  | otherwise  = 1 + divide (m-n) n

highestCommonFactor :: Int -> Int -> Int -> Int
highestCommonFactor x y z
  | (guess `isFactorOf` small) && (guess `isFactorOf` large) = guess
  | otherwise = highestCommonFactor x y (guess-1)
  where small = min x y
        large = max x y
        guess = min z small

isFactorOf :: Int -> Int -> Bool
isFactorOf x y = y `mod` x == 0


testMax1 = TestCase (assertEqual "for: maxThree 6 4 1" 6 (maxThree 6 4 1))
testMax2 = TestCase (assertEqual "for: maxThree 6 6 6" 6 (maxThree 6 6 6))
testMax3 = TestCase (assertEqual "for: maxThree 2 6 6" 6 (maxThree 2 6 6))
testMax4 = TestCase (assertEqual "for: maxThree 2 2 6" 6 (maxThree 2 2 6))
testMax5 = TestCase (assertEqual "for: maxThree 6 6 2" 6 (mysteryMax 6 6 2))

testsMax = TestList [testMax1, testMax2, testMax3, testMax4, testMax5]


allEqual :: Integer -> Integer -> Integer -> Bool
allEqual m n p
  | (m ==n) && (n == p)  = True
  | otherwise            = False

testEq1 = TestCase (assertEqual "for: allEqual 4 5 6" False (allEqual 4 5 6))
testEq2 = TestCase (assertEqual "for: allEqual 4 4 6" False (allEqual 4 4 6))
testEq3 = TestCase (assertEqual "for: allEqual 4 5 4" False (allEqual 4 5 4))
testEq4 = TestCase (assertEqual "for: allEqual 5 4 4" False (allEqual 5 4 4))
testEq5 = TestCase (assertEqual "for: allEqual 4 4 4" True (allEqual 4 4 4))

testsEq = TestList [testEq1, testEq2, testEq3, testEq4, testEq5]

mysteryMax :: Integer -> Integer -> Integer -> Integer
mysteryMax x y z
  | x > y && x > z   = x
  | y > x && y > z   = y
  | otherwise        = z

fact :: Integer -> Integer
fact n
  | n > 1     = n * fact (n-1)
  | otherwise = 1

prop_fact n =
  fact n > 0

-- Chapter 5

type ShopItem = (String,Int)


type Basket = [ShopItem]

minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax x y
  | x >= y     = (y, x)
  | otherwise  = (x, y)

addPair :: (Integer,Integer) -> Integer
addPair (0,y) = y
addPair (x,y) = x + y

shift' :: ((Integer,Integer),Integer) -> (Integer,(Integer,Integer))
shift' ((x,y),z) = (x,(y,z))

name  :: ShopItem -> String
price :: ShopItem -> Int

name  (n,p) = n
price (n,p) = p

addPair' :: (Integer,Integer) -> Integer
addPair' p = fst p + snd p

fibPair :: Integer -> (Integer, Integer)
fibPair n
  | n == 0    = (0,1)
  | otherwise = fibStep (fibPair (n-1))

fibStep :: (Integer,Integer) -> (Integer,Integer)
fibStep (x,y) = (y,x+y)

fibTwoStep :: Integer ->Integer -> (Integer,Integer)
fibTwoStep x y = (y,x+y)

fastFib :: Integer -> Integer
fastFib = fst . fibPair

maxOccurs :: Integer -> Integer -> (Integer,Integer)
maxOccurs x y
  | x == y    = (x,2)
  | otherwise = (max x y, 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer,Integer)
maxThreeOccurs x y z
  | x == y && y == z                     = (x,3)
  | (x == y) || (x == z) && x == maxElem = (x,2)
  | y == z && y == maxElem               = (y,2)
  | otherwise                            = (maxElem,1)
  where
    maxElem = max (max x y) z

orderTriple :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
orderTriple (x,y,z) = (minThree x y z,middleNumber x y z,maxThree x y z)
