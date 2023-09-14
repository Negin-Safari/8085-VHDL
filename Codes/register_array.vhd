LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY register_array IS
   
    PORT (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;

        xchg : IN STD_LOGIC;
        inc_adr : IN STD_LOGIC;

        din : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        selin : IN STD_LOGIC_VECTOR(3 DOWNTO 0); -- load ha beshan daroni
        selout : IN STD_LOGIC_VECTOR(3 DOWNTO 0);

        seladrout : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
        adrH : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        adrL : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);

        doutW : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        doutZ : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        doutB : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        doutC : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- should be the input of mux + tri + internal bus
        doutD : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        doutE : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        doutH : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        doutL : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);

        decSP : IN STD_LOGIC;
        incSP : IN STD_LOGIC;
        doutHSP : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        doutLSP : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);

        incPC : IN STD_LOGIC; 
        doutHPC : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        doutLPC : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)

    );
END register_array;

ARCHITECTURE behavioural OF register_array IS
    SIGNAL loadW : STD_LOGIC;
    SIGNAL loadZ : STD_LOGIC;
    SIGNAL loadB : STD_LOGIC;
    SIGNAL loadC : STD_LOGIC;
    SIGNAL loadD : STD_LOGIC;
    SIGNAL loadE : STD_LOGIC;
    SIGNAL loadH : STD_LOGIC;
    SIGNAL loadL : STD_LOGIC;
    SIGNAL ldHSP : STD_LOGIC;
    SIGNAL ldLSP : STD_LOGIC;
    SIGNAL ldHPC : STD_LOGIC;
    SIGNAL ldLPC : STD_LOGIC;

    SIGNAL doutW_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL doutZ_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL doutB_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL doutC_reg : STD_LOGIC_VECTOR(7 DOWNTO 0); -- should be the input of mux + tri + internal bus
    SIGNAL doutD_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL doutE_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL doutH_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL doutL_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL dinD : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL dinE : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL dinH : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL dinL : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL doutHSP_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL doutLSP_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL doutHPC_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL doutLPC_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);


    SIGNAL adrH_s : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL adrL_s : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL adr_s : STD_LOGIC_VECTOR(15 DOWNTO 0);

BEGIN



    WITH xchg SELECT
        dinD <= din       WHEN '0',
                doutH_reg WHEN OTHERS;

    WITH xchg SELECT
        dinH <= din       WHEN '0',
                doutD_reg WHEN OTHERS;

    WITH xchg SELECT
        dinL <= din       WHEN '0',
                doutE_reg WHEN OTHERS;

    WITH xchg SELECT
        dinE <= din       WHEN '0',
                doutL_reg WHEN OTHERS;

    WITH inc_adr SELECT
        adr_s <= (adrH_s &  adrL_s)         WHEN '0',
                 ((adrH_s &  adrL_s) + "0000000000000001") WHEN OTHERS;
    
                
    adrH <= adr_s(15 DOWNTO 8);
    adrL <= adr_s(7 DOWNTO 0);

------------------------------ 8 bit register for W -----------------------------
    register_W: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  din ,  
            load    =>  loadW ,  
            dout    =>  doutW_reg,
            initVal => x"00"     
        );  
        
------------------------------ 8 bit register for Z -----------------------------
    register_Z: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  din ,  
            load    =>  loadZ ,  
            dout    =>  doutZ_reg,
            initVal => x"00"      
        );  



------------------------------ 8 bit register for B -----------------------------
    register_B: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  din ,  
            load    =>  loadB ,  
            dout    =>  doutB_reg,
            initVal => x"00"      
        );  
        
------------------------------ 8 bit register for C -----------------------------
    register_C: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  din ,  
            load    =>  loadC ,  
            dout    =>  doutC_reg,
            initVal => x"00"      
        );  

------------------------------ 8 bit register for D -----------------------------
    register_D: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  dinD ,  
            load    =>  loadD ,  
            dout    =>  doutD_reg,
            initVal => x"00"      
        );  
        
------------------------------ 8 bit register for E -----------------------------
    register_E: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  dinE ,  
            load    =>  loadE ,  
            dout    =>  doutE_reg,
            initVal => x"00"      
        ); 

------------------------------ 8 bit register for H -----------------------------
    register_H: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  dinH ,  
            load    =>  loadH ,  
            dout    =>  doutH_reg,
            initVal => x"00"      
        );  
    
------------------------------ 8 bit register for L -----------------------------
    register_L: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  dinL ,  
            load    =>  loadL ,  
            dout    =>  doutL_reg,
            initVal => x"00"      
        ); 

------------------------------ stack pointer -----------------------------
    stack_pointer_register: ENTITY work.stack_pointer(behavioural)
        GENERIC MAP (
            n => 16
        )
        PORT MAP (
            clk     =>  clk , 
            rst     =>  rst ,   
            loadH   =>  ldHSP ,
            loadL   =>  ldLSP ,
            dec     =>  decSP ,
            inc     =>  incSP ,
            din     =>  din ,
            doutH   =>  doutHSP_reg ,
            doutL   =>  doutLSP_reg
        ); 

------------------------------ program counter -----------------------------
    program_counter_register: ENTITY work.program_counter(behavioural)
        GENERIC MAP (
            n => 16
        )
        PORT MAP (
            clk     =>  clk , 
            rst     =>  rst ,   
            loadH   =>  ldHPC ,
            loadL   =>  ldLPC ,
            inc     =>  incPC ,
            din     =>  din ,
            doutH   =>  doutHPC_reg ,
            doutL   =>  doutLPC_reg
        ); 





    PROCESS(selin, xchg)  
        VARIABLE selin_int : INTEGER ;--:= 0 ; 
    BEGIN 
        selin_int := TO_INTEGER(UNSIGNED(selin)); 

        loadW <= '0';
        loadZ <= '0';
        loadB <= '0';
        loadC <= '0';
        loadD <= '0';
        loadE <= '0';
        loadH <= '0';
        loadL <= '0';
        ldHSP <= '0';
        ldLSP <= '0';
        ldHPC <= '0';
        ldLPC <= '0';

        CASE selin_int IS 
            WHEN 0 => 
                loadB <= '1';
       
            WHEN 1 => 
                loadC <= '1'; 
       
            WHEN 2 => 
                loadD <= '1'; 
       
            WHEN 3 => 
                loadE <= '1'; 
       
            WHEN 4 => 
                loadH <= '1'; 
       
            WHEN 5 => 
                loadL <= '1'; 
       
            WHEN 6 => 
                ldHSP <= '1'; 
       
            WHEN 7 => 
                ldLSP <= '1'; 

            WHEN 8 =>
                ldHPC <= '1';
       
            WHEN 9 => 
                ldLPC <= '1'; 
       
            WHEN 10 => 
                loadW <= '1'; 
       
            WHEN 11 => 
                loadZ <= '1'; 
       
            WHEN others => 
                loadW <= '0';
                loadZ <= '0';
                loadB <= '0';
                loadC <= '0';
                loadD <= '0';
                loadE <= '0';
                loadH <= '0';
                loadL <= '0';
                ldHSP <= '0';
                ldLSP <= '0';
                ldHPC <= '0';
                ldLPC <= '0';
       
        END CASE; 
        
        IF (xchg = '1') THEN
            loadD <= '1'; 
            loadE <= '1'; 
            loadH <= '1'; 
            loadL <= '1'; 
            ldHSP <= '0';
            ldLSP <= '0';
            ldHPC <= '0';
            ldLPC <= '0';
            loadW <= '0';
            loadZ <= '0';
            loadB <= '0';
            loadC <= '0';
        END IF;


    END PROCESS; 




    PROCESS(selout, doutB_reg, doutC_reg, doutD_reg, doutE_reg, doutH_reg, doutL_reg, doutHSP_reg, doutLSP_reg, doutHPC_reg, doutLPC_reg, doutW_reg, doutZ_reg)  
        VARIABLE selout_int : INTEGER ;--:= 0 ; 
    BEGIN 
        selout_int := TO_INTEGER(UNSIGNED(selout)); 

        CASE selout_int IS 
            WHEN 0 => 
                dout <= doutB_reg;
       
            WHEN 1 => 
                dout <= doutC_reg; 
       
            WHEN 2 => 
                dout <= doutD_reg;
       
            WHEN 3 => 
                dout <= doutE_reg;
       
            WHEN 4 => 
                dout <= doutH_reg;
       
            WHEN 5 => 
                dout <= doutL_reg;
       
            WHEN 6 => 
                dout <= doutHSP_reg;
       
            WHEN 7 => 
                dout <= doutLSP_reg;

            WHEN 8 =>
                dout <= doutHPC_reg;
       
            WHEN 9 => 
                dout <= doutLPC_reg;
       
            WHEN 10 => 
                dout <= doutW_reg;
       
            WHEN 11 => 
                dout <= doutZ_reg; 
       
            WHEN others => 
                dout <= (OTHERS=> 'Z');
       
        END CASE;         
    END PROCESS;
    
    -- seladrout

    PROCESS(seladrout, doutHPC_reg, doutLPC_reg, doutHSP_reg, doutLSP_reg, doutH_reg, doutL_reg, doutW_reg, doutZ_reg)  
        VARIABLE seladrout_int : INTEGER ;--:= 0 ; 
    BEGIN 
        seladrout_int := TO_INTEGER(UNSIGNED(seladrout)); 

        CASE seladrout_int IS 
            WHEN 1 => 
                adrH_s <= doutHPC_reg;
                adrL_s <= doutLPC_reg;
       
            WHEN 2 => 
                adrH_s <= doutHSP_reg;
                adrL_s <= doutLSP_reg; 
       
            WHEN 3 => 
                adrH_s <= doutH_reg;
                adrL_s <= doutL_reg; 
       
            WHEN 4 => 
                adrH_s <= doutW_reg;
                adrL_s <= doutZ_reg; 
            
            WHEN 5 => 
                adrH_s <= doutB_reg;
                adrL_s <= doutC_reg; 

            WHEN 6 => 
                adrH_s <= doutD_reg;
                adrL_s <= doutE_reg; 
       
            WHEN others => 
                adrH_s <= (OTHERS=> 'Z');
                adrL_s <= (OTHERS=> 'Z');
       
        END CASE;         
    END PROCESS;

    
    doutW   <= doutW_reg;
    doutZ   <= doutZ_reg;
    doutB   <= doutB_reg;
    doutC   <= doutC_reg;
    doutD   <= doutD_reg;
    doutE   <= doutE_reg;
    doutH   <= doutH_reg;
    doutL   <= doutL_reg;
    doutHSP <= doutHSP_reg;
    doutLSP <= doutLSP_reg;
    doutHPC <= doutHPC_reg;
    doutLPC <= doutLPC_reg;

END behavioural;

