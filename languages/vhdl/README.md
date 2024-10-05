# VHDL Prime Number Checker

This is a VHDL module that checks whether a number is prime or not. A testbench is provided to test the functionality.

## How to Run

1. Install `ghdl` for VHDL simulation.

2. Analyze, elaborate, and simulate the design:
   ```bash
   ghdl -a prime.vhdl
   ghdl -a testbench.vhdl
   ghdl -e testbench
   ghdl -r testbench --stop-time=40ns --vcd=prime_checker.vcd
