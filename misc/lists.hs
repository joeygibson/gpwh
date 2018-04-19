-- lists
-- `:` is pronounced `cons`, like in Lisp `(cons 1 '(2 3 4))`
-- `++` combines two lists
-- generate a list of ten digits
l = [1 .. 10]

-- generate a list of odd numbers
l = [1,3 .. 10]

-- generate a list of number by 0.5
l = [1,1.5 .. 10]

-- generate a decrementing list
l = [1,0 .. -10]

-- generate infinite list
l = [1 ..]

-- lists are lazy
simple x = x

longList = [1 ..]

stillLongList = simple longList

-- `!!` indexes a list
v = [1, 2, 3] !! 0

-- use `!!` as prefix operator
v = (!!) [1, 2, 3] 0

-- partial application using `!!`
paExample1 = (!!) "dog"

v = paExample 2

-- use a "section" to partially apply a binary operator
paExample2 = ("dog" !!)

v = paExample2 2

paExample3 = (!! 2)

v = paExample3 "dog"

-- some list functions
isPalindrome word = word == reverse word

-- membership
v = elem 13 [0,3 .. 100]

-- any binary function can be treated like an infix operator
-- by wrapping with ``
respond phrase =
  if '!' `elem` phrase
    then "wow!"
    else "uh... OK"

-- combine `take` and `reverse` to create `takeLast`
takeLast n list = reverse (take n (reverse list))

-- `zip` combines lists into tuple pairs
l = zip [1, 2, 3] [4, 5, 6]

-- use `zip` to add indices to list elements
l = zip "hello" [1 ..]

-- `cycle` repeats a list infinitely
ones n = take n (cycle [1])

-- use `cycle` to assign things to groups
assignToGroups n aList = zip groups aList
  where
    groups = cycle [1 .. n]

subseq start end list = drop start (take end list)

inFirstHalf el list = elem el firstHalf
  where firstHalf = take halfIndex list
        halfIndex = (length list) `div` 2
