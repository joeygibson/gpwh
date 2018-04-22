-- recursion and pattern matching
-- The secret to writing recursive functions is to not think about the recursion!
-- Thinking about recursion too much leads to headaches. The way to solve recursive
-- functions is by following this simple set of rules:
--
--  1 Identify the end goal(s).
--  2 Determine what happens when a goal is reached.
--  3 List all alternate possibilities.
--  4 Determine your “rinse and repeat” process.
--  5 Ensure that each alternative moves you toward your goal.

-- Euclid's Algorithm - computes the greatest common devisor (GCD) of two numbers

myGCD a b =
    if remainder == 0
        then b
        else myGCD b remainder
    where remainder = a `mod` b

-- not using pattern matching
sayAmount0 n = case n of
    1 -> "one"
    2 -> "two"
    _ -> "bunch"

-- using pattern matching. it looks like three separate function definitions
sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "bunch"

isEmpty [] = True
isEmpty _  = False

myHead (x:xs) = x
myHead []     = error "No head for empty list"

myTail (_:xs) = xs
myTail []     = error "no tail for empty list"

myDrop n (x:xs) = if n == 1
    then xs
    else myDrop (n - 1) xs
myDrop n [] = []

myLen []     = 0
myLen (x:xs) = 1 + myLen xs

myTake _ []     = []
myTake 0 _      = []
myTake n (x:xs) = x : rest
    where rest = myTake (n - 1) xs

finiteCycle (first:rest) = first:rest ++ [first]

myCycle (first:rest) = first:myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann(m - 1) 1
ackermann m n = ackermann(m - 1) (ackermann m (n - 1))

-- Collatz Conjecture
collatz 1 = 1
collatz n = if even n
    then
        1 + collatz (n `div` 2)
    else
        1 + collatz (n * 3 + 1)

myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
