# SystemVerilog Prime Number Checker

This is a basic prime number checker written in SystemVerilog. The checker uses a simple for-loop in hardware description to determine if a number is prime.

## How to Run

To simulate the prime checker, you can use a simulator like **Verilator** or **ModelSim**.

1. Compile the SystemVerilog code:
   `verilator --cc prime.sv --exe --build`
2. Run the generated executable:
    `./obj_dir/Vprime_checker`
