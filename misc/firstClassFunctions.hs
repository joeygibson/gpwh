import           Data.List

ifEven myFunction n =
  if even n
    then myFunction n
    else n

inc n = n + 1

double n = n * 2

square n = n ^ 2

ifEvenInc n = ifEven inc n

ifEvenDouble n = ifEven double n

ifEvenSquare n = ifEven square n

ifEvenCube n = ifEven (\x -> x * x * x) n

-- Sorting with passed-in functions
-- a tuple
author = ("Patrick", "O'Brian")

names =
  [ ("Ian", "Curtis")
  , ("Bernard", "Sumner")
  , ("Peter", "Hook")
  , ("Stephen", "Morris")
  , ("Frank", "Morris")
  ]

compareFirstNames name1 name2 =
  if name1 > name2
    then GT
    else if name1 < name2
           then LT
           else EQ

compareLastNames name1 name2 =
  if lastName1 > lastName2
    then GT
    else if lastName1 < lastName2
           then LT
           else compareFirstNames firstName1 firstName2
  where
    lastName1 = snd name1
    lastName2 = snd name2
    firstName1 = fst name1
    firstName2 = fst name2

-- use built-in `compare` function
compareLastNames' name1 name2 = compare lastName1 lastName2
  where
    lastName1 = snd name1
    lastName2 = snd name2

-- functions returning functions
sfOffice name =
  if lastName < "L"
    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = (fst name) ++ " " ++ (snd name)

dcOffice name = nameText ++ ", Esq. - PO Box 987 Q St, Washington DC 10101"
  where
    nameText = (fst name) ++ " " ++ (snd name)

getLocationFunction location =
  case location of
    "ny"   -> nyOffice
    "sf"   -> sfOffice
    "reno" -> renoOffice
    "dc"   -> dcOffice
    _      -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunction name
  where
    locationFunction = getLocationFunction location
