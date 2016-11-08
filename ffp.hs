module Ffp where
-- First, we declare the name of our module so
-- it can be imported by name in a project.
-- We won't be doing a project of this size
-- for a while yet

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

mult3 x = x * 3

x = 7
y = 10
f = x + y

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

printInc2' n = (\plusTwo -> print plusTwo) (n + 2)

