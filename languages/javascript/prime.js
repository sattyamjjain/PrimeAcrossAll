const readline = require('readline');

function isPrime(n) {
    if (n <= 1) return false;
    if (n === 2) return true;
    if (n % 2 === 0) return false;

    for (let i = 3; i <= Math.sqrt(n); i += 2) {
        if (n % i === 0) return false;
    }
    return true;
}

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

rl.question("Enter a number to check if it's prime: ", (number) => {
    number = parseInt(number);
    if (isPrime(number)) {
        console.log(`${number} is a prime number.`);
    } else {
        console.log(`${number} is not a prime number.`);
    }
    rl.close();
});
