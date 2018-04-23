-- types

-- a 32- or 64-bit integral number
x :: Int
x = 2

-- an unbounded integral number
y :: Integer
y = 2

double :: Int -> Int
double n = n * 2

half :: Int -> Double
half n = (fromIntegral n) / 2

-- `show 6` converts `6` to "6"

anotherNumber :: Int
anotherNumber = read "6"

-- `read "6" :: Int` returns an Int
-- `read "6" :: Double` returns a Double

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n

-- type variables are like `T` in Java generics
simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) = if f x
                    then x:myFilter f xs
                    else myFilter f xs

