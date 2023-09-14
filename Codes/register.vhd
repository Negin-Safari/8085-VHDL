Library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY registerr IS
    GENERIC (
        n : INTEGER := 8
    );
    PORT (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;
        load : IN STD_LOGIC;
        initVal : IN STD_LOGIC_VECTOR(n - 1 DOWNTO 0);
        din : IN STD_LOGIC_VECTOR(n - 1 DOWNTO 0);
        dout : OUT STD_LOGIC_VECTOR(n - 1 DOWNTO 0)
    );
END registerr;

ARCHITECTURE behavioural OF registerr IS
    SIGNAL data : STD_LOGIC_VECTOR(n - 1 DOWNTO 0);
BEGIN

    PROCESS (clk)
    BEGIN
        IF (clk = '1' AND clk'EVENT) THEN
            IF (rst = '1') THEN
                data <= initVal;
            ELSIF (load = '1') THEN
                data <= din;
            ELSE
                data <= data;
            END IF;
        END IF;
    END PROCESS;

    dout <= data(data'HIGH DOWNTO data'LOW);

END behavioural;
