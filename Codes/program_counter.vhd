Library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY program_counter IS
    GENERIC (
        n : INTEGER := 16
    );
    PORT (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;
        loadH : IN STD_LOGIC;
        loadL : IN STD_LOGIC;
        inc : IN STD_LOGIC;
        din : IN STD_LOGIC_VECTOR(n/2 - 1 DOWNTO 0);
        doutH : OUT STD_LOGIC_VECTOR(n/2 - 1 DOWNTO 0);
        doutL : OUT STD_LOGIC_VECTOR(n/2 - 1 DOWNTO 0)
    );
END program_counter;

ARCHITECTURE behavioural OF program_counter IS
    SIGNAL data : STD_LOGIC_VECTOR(n - 1 DOWNTO 0);
BEGIN

    PROCESS (clk)
    BEGIN
        IF (clk = '1' AND clk'EVENT) THEN
            IF (rst = '1') THEN
                data <= (OTHERS => '0');
            ELSIF (loadH = '1') THEN
                data(15 DOWNTO 8) <= din;
            ELSIF (loadL = '1') THEN
                data(7 DOWNTO 0) <= din;
            ELSIF (inc = '1') THEN
                data <= data + 1;
            ELSE
                data <= data;
            END IF;
        END IF;
    END PROCESS;

    doutL <= data(((n)/2 -1) DOWNTO data'LOW);
    doutH <= data(data'HIGH DOWNTO ((n)/2));

END behavioural;

