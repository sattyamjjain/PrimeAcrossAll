isPrime :: Integer -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | otherwise = not (any divisible [2..(floor (sqrt (fromIntegral n)))])
    where divisible x = n `mod` x == 0

main :: IO ()
main = do
    putStrLn "Enter a number to check if it's prime: "
    input <- getLine
    let number = read input :: Integer
    if isPrime number
        then putStrLn (show number ++ " is a prime number.")
        else putStrLn (show number ++ " is not a prime number.")
