// prime.sv - SystemVerilog module to check if a number is prime

module prime_checker(
    input  logic [31:0] num,
    output logic        is_prime
);

    integer i;
    logic flag;

    always_comb begin
        if (num <= 1) begin
            is_prime = 0;
        end else if (num <= 3) begin
            is_prime = 1;
        end else if ((num % 2 == 0) || (num % 3 == 0)) begin
            is_prime = 0;
        end else begin
            flag = 1;
            for (i = 5; i * i <= num; i = i + 6) begin
                if ((num % i == 0) || (num % (i + 2) == 0)) begin
                    flag = 0;
                    break;
                end
            end
            is_prime = flag;
        end
    end

endmodule
