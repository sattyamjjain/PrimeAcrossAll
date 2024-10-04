"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var readline_1 = require("readline");
var rl = (0, readline_1.createInterface)({
    input: process.stdin,
    output: process.stdout
});
rl.question('Enter a number to check if it\'s prime: ', function (answer) {
    var number = parseInt(answer);
    if (isPrime(number)) {
        console.log("".concat(number, " is a prime number."));
    }
    else {
        console.log("".concat(number, " is not a prime number."));
    }
    rl.close();
});
function isPrime(n) {
    if (n <= 1)
        return false;
    for (var i = 2; i <= Math.sqrt(n); i++) {
        if (n % i === 0)
            return false;
    }
    return true;
}
