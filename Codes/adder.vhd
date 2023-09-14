LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY adder IS 
    GENERIC (
        n: INTEGER := 8 
    ) ; 
    PORT (
        dina : IN STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
        dinb : IN STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
        sum  : OUT STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
        cin  : IN STD_LOGIC ; 
        acy  : OUT STD_LOGIC ; 
        cout : OUT STD_LOGIC 
    ) ; 
END adder; 

ARCHITECTURE behavioural OF adder IS 
    SIGNAL dina_s :STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL dinb_s :STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL sum_s  :STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL c  :STD_LOGIC_VECTOR(n DOWNTO 0 ); 
    SIGNAL cout_s :STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
BEGIN 
    c(0) <= cin ; 

    G1 : FOR i IN 0 TO n-1 GENERATE 
        c(i+1) <= (dina_s(i) AND dinb_s(i)) OR (c(i) AND dinb_s(i)) OR (dina_s(i) AND c(i));
        sum_s(i) <= (dina_s(i) XOR dinb_s(i) XOR c(i));
    END GENERATE;

    sum <= sum_S; 
    cout <= c(n); 
    dina_s <= dina; 
    dinb_s <= dinb; 
    acy <= c(4); 

END behavioural ; 
