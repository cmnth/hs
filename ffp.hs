{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
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
