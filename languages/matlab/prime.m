function prime_check(n)
    if n < 2
        disp([num2str(n), ' is not a prime number.']);
        return;
    end
    for i = 2:sqrt(n)
        if mod(n, i) == 0
            disp([num2str(n), ' is not a prime number.']);
            return;
        end
    end
    disp([num2str(n), ' is a prime number.']);
end

% Get input from user
n = input('Enter a number: ');
prime_check(n);
