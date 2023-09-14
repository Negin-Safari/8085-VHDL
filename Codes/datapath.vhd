Library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY datapath IS
    PORT (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;

        xchg : IN STD_LOGIC;
        inc_adr : IN STD_LOGIC;
        
        en_triAccBus : IN STD_LOGIC;
        en_triFlgBus : IN STD_LOGIC;
        en_triAluBus : IN STD_LOGIC;
        en_triArrBus : IN STD_LOGIC;
        en_bufAdr : IN STD_LOGIC;
        en_bufAdrData : IN STD_LOGIC;
        en_triDataBuf : IN STD_LOGIC;
        en_triAdrBuf : IN STD_LOGIC;
        en_triExtDataBuf : IN STD_LOGIC;
        en_triExtDataBus : IN STD_LOGIC;
        en_triIntDataBus : IN STD_LOGIC;

        ld_acc : IN STD_LOGIC;
        ld_tmp : IN STD_LOGIC;
        ld_flg : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        ld_inst : IN STD_LOGIC;
        sel_operation : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        sel_inArrData : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        sel_outArrData : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        sel_outArrAdr : IN STD_LOGIC_VECTOR(2 DOWNTO 0);

        ld_invisibleCY : IN STD_LOGIC;
        sel_invisibleCY : IN STD_LOGIC;

        decSP : IN STD_LOGIC;   
        incSP : IN STD_LOGIC;
        incPC : IN STD_LOGIC;

        flag : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);

        inst : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        address_bus : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        address_data_bus : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0)

    );
END datapath;

ARCHITECTURE behavioural OF datapath IS
    SIGNAL internal_bus : STD_LOGIC_VECTOR(7 DOWNTO 0);
    --SIGNAL address_data_bus : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL dataOut_acc : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL dataOut_tmp : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL dataOut_flg : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL dataOut_alu : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL dataOut_inst : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL dataOut_arr : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL adrHOut_arr : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL adrLOut_arr : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL adrDataBuf_out : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL dataAdrIn_buf : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL clk_n : STD_LOGIC;

    SIGNAL Z_stored : STD_LOGIC;
    SIGNAL S_stored : STD_LOGIC;
    SIGNAL P_stored : STD_LOGIC;
    SIGNAL CY_stored : STD_LOGIC;
    SIGNAL AC_stored : STD_LOGIC;

    SIGNAL Z_new : STD_LOGIC;
    SIGNAL S_new : STD_LOGIC;
    SIGNAL P_new : STD_LOGIC;
    SIGNAL CY_new : STD_LOGIC;
    SIGNAL AC_new : STD_LOGIC;
    SIGNAL en_triAluBus_s : STD_LOGIC;
    SIGNAL en_triArrBus_s : STD_LOGIC;
    SIGNAL en_triAccBus_s : STD_LOGIC;
    SIGNAL en_triIntDataBus_s : STD_LOGIC;
BEGIN

    flag <= dataOut_flg;

------------------------------ Tri states -----------------------------
    WITH (en_triAluBus AND en_triAccBus AND clk) SELECT
            en_triAccBus_s <= '1'         WHEN '1',
                             ((NOT(en_triAluBus AND en_triAccBus)) AND en_triAccBus) WHEN OTHERS;

    WITH (en_triAluBus AND en_triIntDataBus AND clk) SELECT
            en_triIntDataBus_s <= '1'         WHEN '1',
                             ((NOT(en_triAluBus AND en_triIntDataBus)) AND en_triIntDataBus) WHEN OTHERS;


    T_acc_ibus: ENTITY work.tri_state(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (  
            en     =>  en_triAccBus_s ,  
            din    =>  dataOut_acc ,  
            dout    =>  internal_bus     
        ); 

    T_flg_ibus: ENTITY work.tri_state(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (  
            en     =>  en_triFlgBus ,  
            din    =>  dataOut_flg ,  
            dout    =>  internal_bus     
        ); 

        
    WITH (en_triAluBus AND en_triArrBus AND clk) SELECT
            en_triArrBus_s <= '1'         WHEN '1',
                             ((NOT(en_triAluBus AND en_triArrBus)) AND en_triArrBus) WHEN OTHERS;

    WITH ( (en_triAluBus AND en_triArrBus AND (NOT(clk))) OR (en_triAluBus AND en_triAccBus AND (NOT(clk))) OR (en_triAluBus AND en_triIntDataBus AND (NOT(clk))) ) SELECT
            en_triAluBus_s <= '1'         WHEN '1',
                             (((NOT(en_triAluBus AND en_triArrBus))  AND  (NOT(en_triAluBus AND en_triAccBus) ) AND (NOT(en_triAluBus AND en_triIntDataBus)) ) AND en_triAluBus) WHEN OTHERS;

    T_alu_ibus: ENTITY work.tri_state(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (  
            en     =>  en_triAluBus_s ,  
            din    =>  dataOut_alu ,  
            dout    =>  internal_bus     
        ); 

    T_arr_ibus: ENTITY work.tri_state(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (  
            en     =>  en_triArrBus_s ,  
            din    =>  dataOut_arr ,  
            dout    =>  internal_bus     
        ); 

    T_adr_buf: ENTITY work.tri_state(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (  
            en     =>  en_triAdrBuf ,  
            din    =>  adrLOut_arr ,  
            dout    =>  dataAdrIn_buf     
        ); 
    
    T_data_buf: ENTITY work.tri_state(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (  
            en     =>  en_triDataBuf ,  
            din    =>  internal_bus ,  
            dout    =>  dataAdrIn_buf     
        ); 

        
    T_externaldata_buf: ENTITY work.tri_state(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (  
            en     =>  en_triExtDataBuf ,  
            din    =>  address_data_bus ,  
            dout    =>  dataAdrIn_buf     
        ); 

    T_externaldata_ibus: ENTITY work.tri_state(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (  
            en     =>  en_triExtDataBus ,  
            din    =>  adrDataBuf_out ,  
            dout    => address_data_bus
        ); 

    T_internaldata_ibus: ENTITY work.tri_state(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (  
            en     =>  en_triIntDataBus_s ,  
            din    =>  adrDataBuf_out ,  
            dout    => internal_bus
        ); 

------------------------------ Accmulator Reg -----------------------------
    R_acc: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  internal_bus ,  
            load    =>  ld_acc ,  
            dout    =>  dataOut_acc ,
            initVal => x"00"     
        ); 

------------------------------ Temporary Reg -----------------------------
    clk_n <= NOT(clk);
    R_tmp: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk_n ,  
            rst     =>  rst ,  
            din     =>  internal_bus ,  
            load    =>  ld_tmp ,  
            dout    =>  dataOut_tmp,
            initVal => x"00"      
        ); 

------------------------------ Flag Reg -----------------------------
    R_flg: ENTITY work.flag_register(behavioural)
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  

            load    =>  ld_flg ,
            
            ld_invisibleCY => ld_invisibleCY,
            sel_invisibleCY => sel_invisibleCY,

            Zinp    =>  Z_new ,
            Sinp    =>  S_new ,
            Pinp    =>  P_new ,
            CYinp   =>  CY_new,
            ACinp   =>  AC_new,

            Zout    =>  Z_stored ,
            Sout    =>  S_stored ,
            Pout    =>  P_stored ,
            CYout   =>  CY_stored,
            ACout   =>  AC_stored,
            
            cond    => dataOut_flg
        ); 


------------------------------ ALU -----------------------------
    ALU: ENTITY work.alu(behavioural)
        PORT MAP (
            dina    =>  dataOut_acc ,  
            dinb    =>  dataOut_tmp , 
            dout    =>  dataOut_alu , 

            co      =>  CY_new ,
            acy     =>  AC_new ,
            sgn     =>  S_new ,
            parity  =>  P_new ,
            zero    =>  Z_new ,

            cin     =>  CY_stored ,

            op_sel  =>  sel_operation 
        ); 

------------------------------ Instruction Reg -----------------------------
    R_inst: ENTITY work.registerr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,  
            din     =>  internal_bus ,  
            load    =>  ld_inst ,  
            dout    =>  dataOut_inst ,
            initVal => x"00"     
        ); 
 
    inst <= dataOut_inst;


------------------------------ Register Array -----------------------------
    R_arr: ENTITY work.register_array(behavioural)
        PORT MAP (
            clk     =>  clk ,  
            rst     =>  rst ,
            xchg    =>  xchg , 
            inc_adr =>  inc_adr, 
            din     =>  internal_bus ,   
            dout    =>  dataOut_arr ,    
            selin   =>  sel_inArrData ,
            selout   =>  sel_outArrData ,
            seladrout => sel_outArrAdr ,
            adrH    =>  adrHOut_arr , 
            adrL    =>  adrLOut_arr , 

            decSP   =>  decSP ,
            incSP   =>  incSP ,
            incPC   =>  incPC
        ); 
  

------------------------------ Address Buffer -----------------------------
    B_adr: ENTITY work.bufferr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            en     =>  en_bufAdr ,   
            din     =>  adrHOut_arr ,  
            dout    =>  address_bus     
        ); 

------------------------------ Address/Data Buffer -----------------------------
    B_adrData: ENTITY work.bufferr(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            en     =>  en_bufAdrData ,   
            din     =>  dataAdrIn_buf ,  
            dout    =>  adrDataBuf_out     
        ); 

-----------------------------------------------------------------------------
   -- address_data_bus_o <= address_data_bus;

END behavioural;


