fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    putStrLn "Which Fib?"
    n <- getLine

    let whichFib = (read n) :: Int

    let f = fib whichFib

    putStrLn ("Fib(" ++ (show whichFib) ++ ") = " ++ (show f))

