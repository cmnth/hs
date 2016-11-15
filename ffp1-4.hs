{-# LANGUAGE FlexibleContexts #-}

module Ffp where

-- Chapter 2

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

printInc2' n = (\plusTwo -> print plusTwo) (n + 2)

mult1      = x * y
   where x = 5
         y = 6

mult2     = x * 3 + y
  where x = 3
        y = 1000

mult3 x = x * 3

mult5     = x * 5
  where y = 10
        x = 10 * 5 + y

mult7     = z / x + y
  where x = 7
        y = negate x
        z = y * 10

square x = x * x

triple x = x * 3

waxOn     = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

waxOff  x = div (/10) $ square $ triple x

-- Chapter 3

myGreeting :: String
myGreeting = (++) "Hello" " World!"

hello :: String
hello = "hello"

world :: String
world = "world!"

greeting = "Yarrr"

main :: IO ()
main = do
  putStrLn greeting
  printSecond

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main' :: IO ()
main' = do
  putStrLn myGreeting
  putStrLn secondGreeting
     where secondGreeting = (++) hello ((++) " " world)

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
   where woot :: Integer
         woot = 10

topLevelValue :: Integer
topLevelValue = 5

area d = pi * (r * r)
   where r = d / 2

tailDrop n s = tail $ drop n $ s
takeDrop n m s = take n $ drop m $ s

thirdLetter :: String -> Char
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs s = c ++ " " ++ b ++ " "++ a
   where a = take 5 s
         b = take 2 $ drop 6 $ s
         c = drop 9 s

-- Chapter 4

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "eyyy. What's shakin'?"
  else
    putStrLn "pshhh."
  where cool = coolness == "downright frosty yo"

printUpTo10 n
  | n > 10 = return ()
  | otherwise = do
      print n
      printUpTo10 (n+1)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- 9
myAbs :: Integer -> Integer
myAbs x = if (x < 0) then (negate x) else x

-- 10
tupleInsideSwap :: (a, b) -> (c, d) -> ((b, d), (a, c))
tupleInsideSwap x y = ((snd x, snd y), (fst x, fst y))

-- Reading syntax

-- 1

x = (+1)
f1 xs = x w
   where w = length xs

-- 2 identity function
f2 = (\x -> x)

-- 3
f3 (x:xs) = x

-- 4
f4 (a, b) = a

type Name = String

data Pet = Cat | Dog Name

isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y
