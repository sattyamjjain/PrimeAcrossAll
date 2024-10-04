function isPrime(n: number): boolean {
    if (n <= 1) return false;
    if (n === 2) return true;

    for (let i = 2; i <= Math.sqrt(n); i++) {
        if (n % i === 0) return false;
    }
    return true;
}

const readline = require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
});

readline.question('Enter a number to check if it\'s prime: ', (input: string) => {
    const number = parseInt(input);
    if (isPrime(number)) {
        console.log(`${number} is a prime number.`);
    } else {
        console.log(`${number} is not a prime number.`);
    }
    readline.close();
});
