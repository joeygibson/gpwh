-- type classes

addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where
    describe :: a -> String

-- derived type class
-- includes the other type classes' functions
data IceCream = Chocolate | Vanilla deriving (Show, Ord, Eq)

inc :: Int -> Int
inc x = x + 1

