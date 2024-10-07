// testbench.v - Testbench for Verilog prime checker

`timescale 1ns / 1ps

module testbench;

    reg [31:0] num;
    wire is_prime;

    prime_checker uut (
        .num(num),
        .is_prime(is_prime)
    );

    initial begin
        $monitor("Time = %0t ns, Number = %d, is_prime = %b", $time, num, is_prime);

        // Test cases
        num = 2;
        #10;

        num = 3;
        #10;

        num = 4;
        #10;

        num = 5;
        #10;

        num = 16;
        #10;

        num = 17;
        #10;

        num = 18;
        #10;

        num = 19;
        #10;

        num = 97;
        #10;

        num = 100;
        #10;

        $finish;
    end

endmodule
