module prime_checker #(parameter WIDTH = 32) (
    input logic [WIDTH-1:0] num,
    output logic is_prime
);
    logic [WIDTH-1:0] i;
    logic found_divisor;

    always_comb begin
        if (num <= 1) begin
            is_prime = 0;
        end else begin
            found_divisor = 0;
            for (i = 2; i <= num / 2; i = i + 1) begin
                if (num % i == 0) begin
                    found_divisor = 1;
                end
            end
            is_prime = !found_divisor;
        end
    end
endmodule

module tb_prime_checker;
    logic [31:0] num;
    logic is_prime;

    prime_checker uut (
        .num(num),
        .is_prime(is_prime)
    );

    initial begin
        num = 17;
        #10;
        if (is_prime)
            $display("Number %0d is prime", num);
        else
            $display("Number %0d is not prime", num);

        num = 18;
        #10;
        if (is_prime)
            $display("Number %0d is prime", num);
        else
            $display("Number %0d is not prime", num);

        $finish;
    end
endmodule
