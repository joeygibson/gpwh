{-# LANGUAGE OverloadedStrings #-}

import           Data.Semigroup
import qualified Data.Text      as T

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

aWord :: T.Text
aWord = "foo"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

chunks = T.lines sampleInput
sampleWords = T.words sampleInput

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"
