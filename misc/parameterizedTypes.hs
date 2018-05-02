-- parameterized types
import           Data.Char
import qualified Data.Map  as Map

data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillps" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

-- functions to work on Triples
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ y _) = y

third :: Triple a -> a
third (Triple _ _ z) = z

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- Lists, by hand
data List a = Empty | Cons a (List a) deriving Show

builtInEx1 :: [Int]
builtInEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtInEx2 :: [Char]
builtInEx2 = 'c':'a':'t':[]

ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty         = Empty
ourMap f (Cons a rest) = Cons (f a) (ourMap f rest)

-- tuples
itemCount1 :: (String, Int)
itemCount1 = ("erasers", 25)

itemCount2 :: (String, Int)
itemCount2 = ("pencils", 25)

itemCount3 :: (String, Int)
itemCount3 = ("pens", 13)

itemInventory :: [(String, Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]

-- Types have types, too. They are called "kinds"
-- The kind of a type indicates the number of parameters the type takes, which are
-- expressed using an asterisk (*). Types that take no parameters have a kind of *,
-- types that take one parameter have the kind * -> *, types with two parameters
-- have the kind * -> * -> *, and so forth.

-- Data.Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Map.lookup 13 organCatalog -> Just Brain

organInvPairs :: [(Organ, Int)]
organInvPairs = zip organs (repeat 23)

organInventory :: Map.Map Organ Int
organInventory = Map.fromList organInvPairs
