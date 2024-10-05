<cfscript>
function isPrime(n) {
    if (n <= 1) {
        return false;
    } else if (n == 2) {
        return true;
    } else if (mod(n, 2) == 0) {
        return false;
    }
    for (i = 3; i <= sqrt(n); i = i + 2) {
        if (mod(n, i) == 0) {
            return false;
        }
    }
    return true;
}

number = arguments[1];
if (isPrime(number)) {
    writeOutput("true");
} else {
    writeOutput("false");
}
</cfscript>
