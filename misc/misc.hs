-- misc Haskell thingies
calcChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

-- Original function
sumSquareOrSquareSum0 x y = if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x^2 + y^2
    squareSum = (x+y)^2

-- rewrite to not use `where` clause
body sumSquare squareSum = if sumSquare > squareSum
    then sumSquare
    else squareSum

sumSquareOrSquareSum1 x y = body (x^2 + y^2) (x+y)^2

-- rewritten again, to use a nested lambda function
sumSquareOrSquareSum2 x y = (\sumSquare squareSum ->
    if sumSquare > squareSum
        then sumSquare
        else squareSum) (x^2 + y^2) (x+y)^2

doubleDouble x = (\dubs -> dubs*2) (x * 2)

-- using a `let` exrepssion
sumSquareOrSquareSum3 x y =
    let sumSquare = (x^2 + y^2)
        squareSum = (x+y)^2
        in
            if sumSquare > squareSum
                then sumSquare
                else squareSum
