-- higher order functions
{-# LANGUAGE TemplateHaskell #-}

-- res = map reverse ["dog", "cat", "moose"]

-- res = map ("a " ++) ["train", "plane", "boat"]

-- res = map (^2) [1, 2, 3]

addAnA []     = []
addAnA (x:xs) = ("a " ++ x) : addAnA xs

myMap f []     = []
myMap f (x:xs) = (f x):myMap f xs

-- filter even [1, 2, 3, 4, 5]

-- filter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"]

myFilter test [] = []
myFilter test (x:xs) = if test x
    then x:myFilter test xs
    else myFilter test xs

myRemove test [] = []
myRemove test (x:xs) = if test x
    then myRemove test xs
    else x:myRemove test xs

-- `foldl` is like `inject:into:`
-- foldl (+) 0 [1, 2, 3, 4]

-- myProduct []     = 1
-- myProduct (x:xs) = x * myProduct xs
myProduct xs = foldl (*) 1 xs

-- concatAll []     = []
-- concatAll (x:xs) = x ++ concatAll xs
concatAll xs = foldl (++) "" xs

-- uses `map` and `foldl` together
-- sumOfSquares xs = foldl (+) 0 squares
--     where squares = map (^2) xs
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

-- use `foldl` to reverse a list
rcons x y = y:x
myReverse xs = foldl rcons [] xs

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
    where rightResult = myFoldr f init xs

-- * foldl is the most intuitive behaving of the folds, but it usually has terrible
--   performance and can’t be used on infinite lists.
-- * foldl' is a nonlazy version of foldl that’s often much more efficient.
-- * foldr is often more efficient than foldl and is the only fold that works on infinite
--   lists.
