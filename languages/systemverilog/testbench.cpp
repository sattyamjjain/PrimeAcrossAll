// testbench.cpp - C++ testbench for SystemVerilog prime checker using Verilator

#include "Vprime_checker.h"
#include "verilated.h"

int main(int argc, char **argv) {
    Verilated::commandArgs(argc, argv);
    Vprime_checker *top = new Vprime_checker;

    // Input number to check for primality
    vluint32_t num = 0;

    // Check if a number was provided via command-line arguments
    if (argc > 1) {
        num = atoi(argv[1]);
    } else {
        // Default number if none provided
        num = 1000000;
    }

    // Set the input
    top->num = num;

    // Evaluate the model
    top->eval();

    // Output the result
    if (top->is_prime) {
        printf("%u is a prime number.\n", num);
    } else {
        printf("%u is not a prime number.\n", num);
    }

    // Final model cleanup
    delete top;
    return 0;
}
