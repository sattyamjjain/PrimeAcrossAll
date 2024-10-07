// prime.v - Verilog module to check if a number is prime

module prime_checker(
    input wire [31:0] num,
    output reg is_prime
);

    integer i;
    reg flag;

    always @(*) begin
        if (num <= 1) begin
            is_prime = 0;
        end else if (num <= 3) begin
            is_prime = 1;
        end else if (num % 2 == 0 || num % 3 == 0) begin
            is_prime = 0;
        end else begin
            flag = 1;
            i = 5;
            while (i * i <= num) begin
                if (num % i == 0 || num % (i + 2) == 0) begin
                    flag = 0;
                    disable while_loop;
                end
                i = i + 6;
            end
            is_prime = flag;
        end
    end

endmodule
