import qualified Data.Map as Map

names :: Map.Map String String
names = Map.fromList [("zippy", "Zippy T. Pinhead"), ("frank", "Frank Sinatra")]

helloPerson :: String -> String
helloPerson name    = "Hello, " ++ name ++ "!"

maybeMain :: Maybe String
maybeMain = do
    -- name <- Map.lookup "zippy" names
    -- let result = helloPerson name

    name <- Map.lookup "dino" names
    let result = helloPerson name

    return result

