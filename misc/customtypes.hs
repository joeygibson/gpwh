-- custom types
type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (String, String)

type MiddleName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

-- data Patient = Patient Name Sex Int Int Int BloodType
data Patient = Patient { name       :: Name
                        , sex       :: Sex
                        , age       :: Int
                        , height    :: Int
                        , weight    :: Int
                        , bloodType :: BloodType }

showName :: Name -> String
showName (Name f l)             = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Sex = Male | Female

patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
  where name = lname ++ ", " ++ fname
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in. )"

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType
---- Type ------ Data construcor --------
---- Constructor ------------------------

patient1Bt :: BloodType
patient1Bt = BloodType A Pos

patient2Bt :: BloodType
patient2Bt = BloodType O Neg

patient3Bt :: BloodType
patient3Bt = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _               = True  -- universal donor
canDonateTo _ (BloodType AB _)              = True  -- universal recipient
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _                             = False -- anything else

compatiblePatients :: Patient -> Patient -> Bool
compatiblePatients donor recipient = canDonateTo donorType recipientType
  where donorType = bloodType donor
        recipientType = bloodType recipient

-- johnDoe :: Patient
-- johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

jackieSmith :: Patient
jackieSmith = Patient {name = Name "Jackie" "Smith",
                       age = 43,
                       sex = Female,
                       height = 62,
                       weight = 115,
                       bloodType = BloodType O Neg}

jackieSmithUpdated = jackieSmith {age = 44}
