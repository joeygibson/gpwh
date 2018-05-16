import           System.IO

main :: IO ()
main = do
    myFile <- openFile "files.hs" ReadMode
    firstLine <- hGetLine myFile
    putStrLn ("First: " ++ firstLine)
    hClose myFile
    putStrLn "Done!"

