-- chapter 10 capstone

-- cup flOz = \_ -> flOz
cup flOz = \message -> message flOz

coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz)

-- drink aCup ozDrank = cup (flOz - ozDrank)
--   where flOz = getOz aCup

drink aCup ozDrank = if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where flOz = getOz aCup
        ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

afterManySips = foldl drink coffeeCup [1, 1, 1, 1 , 1]

-- fighting robots
robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot = robot ("Kill3r", 25, 200)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, h) = h

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHp aRobot newHp = aRobot (\(n, a, h) -> robot (n, a, newHp))

printRobot aRobot = aRobot (\(n, a, h) -> n ++ " attack: " ++ (show a) ++ " hp: " ++ (show h))

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHp aRobot > 10
                 then getAttack aRobot
                 else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)
