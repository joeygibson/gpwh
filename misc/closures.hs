-- closures

ifEven myFunction n =
  if even n
    then myFunction n
    else n

inc n = n + 1

double n = n * 2

square n = n ^ 2

genIfEven f = (\x -> ifEven f x)

ifEvenInc = genIfEven inc

-- url building thingy
getRequestUrl host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genRequestBuilder host = (\apiKey resource id ->
  getRequestUrl host apiKey resource id)

exampleUrlBuilder = genRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey = (\resource id ->
  hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

-- partial application
add4 a b c d = a + b + c + d

mystery = add4 3

-- with partial application, the "builder" stuff above is unnecessary
exampleUrlBuilder' = getRequestUrl "http://example.com"
myExampleUrlBuilder' = exampleUrlBuilder' "1337hAsk3ll"
bookUrlBuilder = myExampleUrlBuilder' "books"

-- Partial application is also the reason we created the rule that arguments
-- should be ordered from most to least general. When you use partial
-- application, the arguments are applied first to last.

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)
-- `flip` is a built-in function

subtract2 n = flip (-) 2
