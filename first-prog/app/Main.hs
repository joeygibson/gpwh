module Main where

--import Lib
toPart recipient = "Dear " ++ recipient ++ ",\n"

bookPart title = "Thanks for buying " ++ title

authorPart author = "\nthanks,\n" ++ author

createEmail recipient title author =
  toPart recipient ++ bookPart title ++ authorPart author

-- messy main
main :: IO ()
main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)
