-- still more types
-- Types that are made by combining other types with an and are called product types.
-- Types combined using or are called sum types.

-- data AuthorName = AuthorName {
--     firstName :: String,
--     lastName  :: String
-- }

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName

data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name

data Artist = Person Name | Band String

hpLovecraft :: Creator
hpLovecraft = AuthorCreator
                (Author
                    (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data Book = Book {
    author    :: Creator,
    isbn      :: String,
    bookTitle :: String,
    bookYear  :: Int,
    bookPrice :: Double
}

data VinylRecord = VinylRecord {
    artist      :: Creator,
    recordTitle :: String,
    recordYear  :: Int,
    recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name           :: String,
    toyDescription :: String,
    toyPrice       :: Double
}

data Pamphlet = Pamphlet {
    pamphletTitle       :: String,
    pamphletDescription :: String,
    contact             :: Creator,
    pamphletPrice       :: Double
}

data StoreItem = BookItem Book
    | RecordItem VinylRecord
    | ToyItem CollectibleToy
    | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book)         = bookPrice book
price (RecordItem record)     = recordPrice record
price (ToyItem toy)           = toyPrice toy
price (PamphletItem pamphlet) = pamphletPrice pamphlet

-- madeBy :: StoreItem -> String
-- madeBy (BookItem book)     = show (author book)
-- madeBy (RecordItem record) = show (artist record)
-- madeBy _                   = "None"

data Circle = Circle {
    diameter :: Float
}

data Square = Square {
    side :: Float
}

data Rectangle = Rectangle {
    height :: Float,
    width  :: Float
}

data Shape = CircleShape Circle
    | SquareShape Square
    | RectangleShape Rectangle

