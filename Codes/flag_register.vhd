Library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY flag_register IS
    PORT (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;

        load : IN STD_LOGIC_VECTOR(4 DOWNTO 0);

        ld_invisibleCY : IN STD_LOGIC;
        sel_invisibleCY : IN STD_LOGIC;

        Zinp : IN STD_LOGIC;
        Sinp : IN STD_LOGIC;
        Pinp : IN STD_LOGIC;
        CYinp : IN STD_LOGIC;
        ACinp : IN STD_LOGIC;

        Zout : OUT STD_LOGIC;
        Sout : OUT STD_LOGIC;
        Pout : OUT STD_LOGIC;
        CYout : OUT STD_LOGIC;
        ACout : OUT STD_LOGIC;

        cond : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END flag_register;

ARCHITECTURE behavioural OF flag_register IS
    SIGNAL data : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL Zout_s : STD_LOGIC;
    SIGNAL Sout_s : STD_LOGIC;
    SIGNAL Pout_s : STD_LOGIC;
    SIGNAL CYout_s : STD_LOGIC;
    SIGNAL ACout_s : STD_LOGIC;
    SIGNAL CYinvisible : STD_LOGIC;
BEGIN

    PROCESS (clk)
    BEGIN
        IF (clk = '1' AND clk'EVENT) THEN
            IF (rst = '1') THEN
                data <= (OTHERS => '0');
                Zout_s <= '0';
                Sout_s <= '0';
                Pout_s <= '0';
                CYout_s <= '0';
                ACout_s <= '0';
            ELSIF (load /= "00000") THEN
                IF (load(0) = '1') THEN
                    --data(7) <= NOT(Zinp);
                    data(6) <= Zinp;
                    Zout_s <= Zinp;
                END IF;
                IF (load(1) = '1') THEN
                    --data(1) <= NOT(Sinp);
                    data(7) <= Sinp;
                    Sout_s <= Sinp;
                END IF;
                IF (load(2) = '1') THEN
                    --data(3) <= NOT(Pinp);
                    data(2) <= Pinp;
                    Pout_s <= Pinp;
                END IF;
                IF (load(3) = '1') THEN
                    --data(5) <= NOT(CYinp);
                    data(0) <= CYinp;
                    CYout_s <= CYinp;
                END IF;
                IF (load(4) = '1') THEN
                    data(4) <= ACinp;
                    ACout_s <= ACinp;
                END IF;
            ELSE 
               data <= data;
               Zout_s <= Zout_s;
               Sout_s <= Sout_s;
               Pout_s <= Pout_s;
               CYout_s <= CYout_s;
               ACout_s <= ACout_s;
            END IF;
        END IF;

    END PROCESS;

    cond <= data(data'HIGH DOWNTO data'LOW);

    Zout <= Zout_s;
    Sout <= Sout_s;
    Pout <= Pout_s;
    ACout <= ACout_s;

    WITH sel_invisibleCY SELECT
        CYout <= CYout_s       WHEN '0',
                CYinvisible WHEN OTHERS;

    PROCESS (clk)
    BEGIN
        IF (clk = '1' AND clk'EVENT) THEN
            IF (rst = '1') THEN
                    CYinvisible <= '0';
            ELSIF (ld_invisibleCY = '1') THEN
                    CYinvisible <= CYinp;
            ELSE
                CYinvisible <= CYinvisible;
            END IF;
        END IF;
    END PROCESS;

END behavioural;

