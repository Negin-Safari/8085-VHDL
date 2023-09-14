LIBRARY ieee; 
USE ieee.std_logic_1164.ALL; 

ENTITY bufferr IS
    GENERIC (
        n : INTEGER := 8
    );
   PORT ( 
       en : IN std_ulogic; 
       din : IN STD_LOGIC_VECTOR(n-1 downto 0);
       dout : OUT STD_LOGIC_VECTOR(n-1 downto 0)
    ); 
END bufferr; 

ARCHITECTURE behavioural OF bufferr IS 
BEGIN 
    PROCESS(en,din) 
        VARIABLE data : STD_LOGIC_VECTOR(n-1 downto 0); 
    BEGIN 
        IF(en = '1') THEN 
            data := din; 
        ELSE 
            data := data; 
        END IF ; 
        dout <= data; 
    END PROCESS; 

END behavioural; 

