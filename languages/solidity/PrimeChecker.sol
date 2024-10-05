// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract PrimeChecker {
    function isPrime(uint256 num) public pure returns (string memory) {
        if (num <= 1) {
            return "Not a prime number";
        }
        for (uint256 i = 2; i * i <= num; i++) {
            if (num % i == 0) {
                return "Not a prime number";
            }
        }
        return "Prime number";
    }
}
