open System

let isPrime n =
    if n <= 1 then false
    elif n = 2 then true
    else
        let limit = int (sqrt (float n))
        seq {2 .. limit} |> Seq.exists (fun x -> n % x = 0) |> not

printf "Enter a number to check if it's prime: "
let input = Console.ReadLine()
let number = int input

if isPrime number then
    printfn "%d is a prime number." number
else
    printfn "%d is not a prime number." number
