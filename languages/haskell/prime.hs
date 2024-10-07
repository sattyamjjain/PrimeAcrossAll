import System.IO

main :: IO ()
main = do
    putStrLn "Enter a number to check if it's prime: "
    input <- getLine
    let n = read input :: Integer
    if isPrime n then
        putStrLn (show n ++ " is a prime number.")
    else
        putStrLn (show n ++ " is not a prime number.")

isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | n <= 3    = True
    | n `mod` 2 == 0 || n `mod` 3 == 0 = False
    | otherwise = null [ x | x <- [5,7..(floor . sqrt . fromIntegral) n], n `mod` x == 0 ]
