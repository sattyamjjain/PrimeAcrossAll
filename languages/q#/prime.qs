namespace PrimeChecker {
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Canon;

    operation IsPrime(n : Int) : Bool {
        body {
            if (n <= 1) {
                return false;
            }
            for (i in 2..(n / 2)) {
                if (n % i == 0) {
                    return false;
                }
            }
            return true;
        }
    }

    @EntryPoint()
    operation CheckPrime() : Unit {
        Message("Enter a number to check if it's prime:");
        let input = IntAsString(ReadLine());
        let number = Int.Parse(input);

        if (IsPrime(number)) {
            Message($"{number} is a prime number.");
        } else {
            Message($"{number} is not a prime number.");
        }
    }
}
