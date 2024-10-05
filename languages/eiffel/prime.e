class
    PRIME_CHECKER

create
    make

feature
    make
        local
            n: INTEGER
        do
            io.put_string("Enter a number: ")
            n := io.read_integer
            if is_prime(n) then
                io.put_string(n.out + " is a prime number.%N")
            else
                io.put_string(n.out + " is not a prime number.%N")
            end
        end

    is_prime(n: INTEGER): BOOLEAN
        local
            i: INTEGER
        do
            if n <= 1 then
                Result := False
            elseif n = 2 or n = 3 then
                Result := True
            elseif n \\ 2 = 0 or n \\ 3 = 0 then
                Result := False
            else
                from
                    i := 5
                until
                    i * i > n
                loop
                    if n \\ i = 0 or n \\ (i + 2) = 0 then
                        Result := False
                        exit
                    end
                    i := i + 6
                end
                Result := True
            end
        end

end
