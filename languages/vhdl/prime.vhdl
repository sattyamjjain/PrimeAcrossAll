library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity prime_checker is
    Port ( num : in INTEGER;
           is_prime : out BOOLEAN);
end prime_checker;

architecture Behavioral of prime_checker is
begin
    process(num)
    begin
        -- Prime checking logic
    end process;
end Behavioral;
