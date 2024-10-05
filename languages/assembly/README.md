# Assembly Prime Number Checker

This directory contains the Assembly implementation of the prime number checking algorithm.

### How to Run

1. Install the NASM assembler if you haven't already. You can do so with the following command on Ubuntu:
    `sudo apt install nasm`
2. Assemble and link the program:
    `nasm -f elf64 -o prime.o prime.asm ld -o prime prime.o`
3. Run the program:
    `./prime`
4. Enter a number when prompted to check if it is prime.

## Example

Enter a number to check if it's prime: 17
17 is a prime number.