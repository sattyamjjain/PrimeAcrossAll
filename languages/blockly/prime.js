const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

rl.question("Enter a number to check if it's prime: ", function(number) {
    number = parseInt(number);
    if (isNaN(number)) {
        console.log("Invalid input.");
    } else {
        // Prime checking logic
        let isPrime = true;

        if (number <= 1) {
            isPrime = false;
        } else if (number <= 3) {
            isPrime = true;
        } else if (number % 2 === 0 || number % 3 === 0) {
            isPrime = false;
        } else {
            for (let i = 5; i * i <= number; i += 6) {
                if (number % i === 0 || number % (i + 2) === 0) {
                    isPrime = false;
                    break;
                }
            }
        }

        if (isPrime) {
            console.log(`${number} is a prime number.`);
        } else {
            console.log(`${number} is not a prime number.`);
        }
    }
    rl.close();
});
