function is_prime(n::Int)
    if n <= 1
        return false
    elseif n == 2
        return true
    else
        for i in 2:floor(Int, sqrt(n))
            if n % i == 0
                return false
            end
        end
    end
    return true
end

println("Enter a number to check if it's prime: ")
n = parse(Int, readline())

if is_prime(n)
    println("$n is a prime number.")
else
    println("$n is not a prime number.")
end
