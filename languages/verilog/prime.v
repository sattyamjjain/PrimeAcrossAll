// testbench.v - Testbench for Verilog prime checker

module testbench;

reg [31:0] num;
wire is_prime;

prime_checker uut (
    .num(num),
    .is_prime(is_prime)
);

initial begin
    $monitor("Number = %d, Prime = %b", num, is_prime);

    // Test case 1: Prime number
    num = 17;
    #10;

    // Test case 2: Not a prime number
    num = 18;
    #10;

    // Test case 3: Another prime number
    num = 19;
    #10;

    // Test case 4: Another non-prime number
    num = 20;
    #10;

    $finish;
end

endmodule
