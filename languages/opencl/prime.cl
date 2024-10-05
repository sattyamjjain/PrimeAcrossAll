__kernel void is_prime(__global int *n, __global int *result) {
    int number = n[0];
    if (number <= 1) {
        result[0] = 0;
        return;
    }
    if (number == 2) {
        result[0] = 1;
        return;
    }
    if (number % 2 == 0) {
        result[0] = 0;
        return;
    }
    for (int i = 3; i * i <= number; i += 2) {
        if (number % i == 0) {
            result[0] = 0;
            return;
        }
    }
    result[0] = 1;
}
