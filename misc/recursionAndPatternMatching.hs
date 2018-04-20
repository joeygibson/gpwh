-- recursion and pattern matching
-- The secret to writing recursive functions is to not think about the recursion!
-- Thinking about recursion too much leads to headaches. The way to solve recursive
-- functions is by following this simple set of rules:
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
