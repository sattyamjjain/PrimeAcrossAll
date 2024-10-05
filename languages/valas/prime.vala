// prime.vala - Vala script to check if a number is prime

public class PrimeChecker {
    public static bool is_prime(int num) {
        if (num <= 1) {
            return false;
        }
        for (int i = 2; i <= num / 2; i++) {
            if (num % i == 0) {
                return false;
            }
        }
        return true;
    }

    public static int main(string[] args) {
        if (args.length != 2) {
            print("Usage: %s <number>\n", args[0]);
            return 1;
        }

        int num = int.parse(args[1]);
        if (PrimeChecker.is_prime(num)) {
            print("%d is a prime number.\n", num);
        } else {
            print("%d is not a prime number.\n", num);
        }
        return 0;
    }
}
