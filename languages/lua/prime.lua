-- Function to check if a number is prime
function isPrime(n)
    if n <= 1 then
        return false
    elseif n == 2 then
        return true
    end

    for i = 2, math.sqrt(n) do
        if n % i == 0 then
            return false
        end
    end

    return true
end

-- Main Program
print("Enter a number to check if it's prime:")
local number = io.read("*n")

if isPrime(number) then
    print(number .. " is a prime number.")
else
    print(number .. " is not a prime number.")
end
