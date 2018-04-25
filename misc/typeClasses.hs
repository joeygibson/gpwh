import           Data.List

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

-- six-sided die
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Ord, Eq, Enum)

instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

-- instance Eq SixSidedDie where
--     (==) S6 S6 = True
--     (==) S5 S5 = True
--     (==) S4 S4 = True
--     (==) S3 S3 = True
--     (==) S2 S2 = True
--     (==) S1 S1 = True
--     (==) _ _   = False

-- instance Ord SixSidedDie where
--     compare S6 S6 = EQ
--     compare S6 _ = GT
--     compare

-- instance Enum SixSidedDie where
--     toEnum 0 = S1
--     toEnum 1 = S2
--     toEnum 2 = S3
--     toEnum 3 = S4
--     toEnum 4 = S5
--     toEnum 5 = S6
--     toEnum _ = error "No such value"

--     fromEnum S1 = 0
--     fromEnum S2 = 1
--     fromEnum S3 = 2
--     fromEnum S4 = 3
--     fromEnum S5 = 4
--     fromEnum S6 = 5

data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [Name ("Ben", "Smith"), Name ("Fred", "Johnson")]
sortedNames = sort names
