LIBRARY ieee; 
USE ieee.std_logic_1164.ALL; 

ENTITY tri_state IS
    GENERIC (
        n : INTEGER := 8
    );
   PORT ( 
       en : IN std_ulogic; 
       din : IN STD_LOGIC_VECTOR(n-1 downto 0);
       dout : OUT STD_LOGIC_VECTOR(n-1 downto 0)
    ); 
END tri_state; 

ARCHITECTURE behavioural OF tri_state IS 
BEGIN 
    PROCESS(en,din) 
        VARIABLE data : STD_LOGIC_VECTOR(n-1 downto 0); 
    BEGIN 
        IF(en = '1') THEN 
            data := din; 
        ELSE 
            data := (OTHERS => 'Z'); 
        END IF ; 
        dout <= data; 
    END PROCESS; 

END behavioural; 
