-- testbench.vhdl - Testbench for VHDL prime checker

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity testbench is
end testbench;

architecture Behavioral of testbench is
    signal num : INTEGER := 0;
    signal is_prime : STD_LOGIC;
begin
    uut: entity work.prime_checker
        Port map (
            num => num,
            is_prime => is_prime
        );

    process
    begin
        -- Test case 1: Prime number
        num <= 17;
        wait for 10 ns;

        -- Test case 2: Not a prime number
        num <= 18;
        wait for 10 ns;

        -- Test case 3: Another prime number
        num <= 19;
        wait for 10 ns;

        -- Test case 4: Another non-prime number
        num <= 20;
        wait for 10 ns;

        wait;
    end process;
end Behavioral;
