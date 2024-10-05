# Verilog Prime Number Checker

This is a Verilog module that checks whether a number is prime or not. A testbench is provided to test the functionality.

## How to Run

1. Install `iverilog` for simulation.

2. Compile and simulate the design:
   ```bash
   iverilog -o prime_checker prime.v testbench.v
   vvp prime_checker
