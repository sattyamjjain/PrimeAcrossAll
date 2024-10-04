function isPrime(n) {
    if (n <= 1)
        return false;
    if (n === 2)
        return true;
    for (var i = 2; i <= Math.sqrt(n); i++) {
        if (n % i === 0)
            return false;
    }
    return true;
}
var readline = require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
});
readline.question('Enter a number to check if it\'s prime: ', function (input) {
    var number = parseInt(input);
    if (isPrime(number)) {
        console.log("".concat(number, " is a prime number."));
    }
    else {
        console.log("".concat(number, " is not a prime number."));
    }
    readline.close();
});
