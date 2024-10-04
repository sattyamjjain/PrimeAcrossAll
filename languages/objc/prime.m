#import <Foundation/Foundation.h>
#import <math.h>

BOOL isPrime(int n) {
    if (n <= 1) return NO;
    if (n == 2) return YES;

    for (int i = 2; i <= sqrt(n); i++) {
        if (n % i == 0) return NO;
    }
    return YES;
}

int main(int argc, const char * argv[]) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
    int number;
    NSLog(@"Enter a number to check if it's prime: ");
    scanf("%d", &number);

    if (isPrime(number)) {
        NSLog(@"%d is a prime number.", number);
    } else {
        NSLog(@"%d is not a prime number.", number);
    }

    [pool drain];
    return 0;
}
