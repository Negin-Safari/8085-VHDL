Library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY cpu IS
    PORT (
        clk                 : IN STD_LOGIC;
        x1                  : IN STD_LOGIC;
        x2                  : IN STD_LOGIC;
        CLKOUT              : OUT STD_LOGIC;
        READY               : IN STD_LOGIC;
        RDbar               : OUT STD_LOGIC;
        WRbar               : OUT STD_LOGIC;
        ALE                 : OUT STD_LOGIC;
        S0                  : OUT STD_LOGIC;
        S1                  : OUT STD_LOGIC;
        IO_Mbar             : OUT STD_LOGIC;
        HOLD                : IN STD_LOGIC;
        HLDA                : IN STD_LOGIC;
        INTAbar             : OUT STD_LOGIC;
        INTR                : IN  STD_LOGIC;
        RST55               : IN  STD_LOGIC;
        RST65               : IN  STD_LOGIC;
        RST75               : IN  STD_LOGIC;
        TRAP                : IN  STD_LOGIC;
        SID                 : IN  STD_LOGIC;
        SOD                 : OUT  STD_LOGIC;
        RESETINbar          : IN STD_LOGIC; -- in top
        RESETOUT            : OUT STD_LOGIC;
        address_bus         : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        address_data_bus    : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END cpu;

ARCHITECTURE behavioural OF cpu IS
    SIGNAL rst                 : STD_LOGIC;
    SIGNAL en_triAccBus        : STD_LOGIC;
    SIGNAL en_triFlgBus        : STD_LOGIC;
    SIGNAL en_triAluBus        : STD_LOGIC;
    SIGNAL en_triArrBus        : STD_LOGIC;
    SIGNAL en_bufAdr           : STD_LOGIC;
    SIGNAL en_bufAdrData       : STD_LOGIC;
    SIGNAL en_triDataBuf       : STD_LOGIC;
    SIGNAL en_triAdrBuf        : STD_LOGIC;
    SIGNAL en_triExtDataBuf    : STD_LOGIC;
    SIGNAL en_triExtDataBus    : STD_LOGIC;
    SIGNAL en_triIntDataBus    : STD_LOGIC;

    SIGNAL ld_acc              : STD_LOGIC;
    SIGNAL ld_tmp              : STD_LOGIC;
    SIGNAL ld_flg              : STD_LOGIC_VECTOR(4 DOWNTO 0);
    SIGNAL ld_inst             : STD_LOGIC;
    SIGNAL sel_operation       : STD_LOGIC_VECTOR(4 DOWNTO 0);
    SIGNAL sel_inArrData       : STD_LOGIC_VECTOR(3 DOWNTO 0);
    SIGNAL sel_outArrData      : STD_LOGIC_VECTOR(3 DOWNTO 0);
    SIGNAL sel_outArrAdr       : STD_LOGIC_VECTOR(2 DOWNTO 0);
    SIGNAL ld_invisibleCY :  STD_LOGIC;
    SIGNAL sel_invisibleCY :  STD_LOGIC;

    SIGNAL decSP : STD_LOGIC;   
    SIGNAL incSP : STD_LOGIC;
    SIGNAL incPC : STD_LOGIC;
    SIGNAL xchg  : STD_LOGIC;
    SIGNAL inc_adr : STD_LOGIC;

    SIGNAL flag : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL inst : STD_LOGIC_VECTOR(7 DOWNTO 0);

    
BEGIN

    rst <= NOT(RESETINbar);

------------------------------ datapath -----------------------------
    DP: ENTITY work.datapath(behavioural)
        PORT MAP (
            clk                 => clk,
            rst                 => rst,

            xchg                => xchg,
            inc_adr             => inc_adr,
            
            en_triAccBus        => en_triAccBus    ,
            en_triFlgBus        => en_triFlgBus    ,
            en_triAluBus        => en_triAluBus    ,
            en_triArrBus        => en_triArrBus    ,
            en_bufAdr           => en_bufAdr       ,
            en_bufAdrData       => en_bufAdrData   ,
            en_triDataBuf       => en_triDataBuf   ,
            en_triAdrBuf        => en_triAdrBuf    ,
            en_triExtDataBuf    => en_triExtDataBuf,
            en_triExtDataBus    => en_triExtDataBus,
            en_triIntDataBus    => en_triIntDataBus,

            ld_acc              => ld_acc        , 
            ld_tmp              => ld_tmp        , 
            ld_flg              => ld_flg        , 
            ld_inst             => ld_inst       , 
            sel_operation       => sel_operation , 
            sel_inArrData       => sel_inArrData , 
            sel_outArrData      => sel_outArrData, 
            sel_outArrAdr       => sel_outArrAdr , 

            decSP               => decSP,   
            incSP               => incSP,
            incPC               => incPC,

            ld_invisibleCY      => ld_invisibleCY ,
            sel_invisibleCY     => sel_invisibleCY,

            flag                => flag,

            inst                => inst,    

            address_bus         => address_bus,

            address_data_bus    => address_data_bus

        );  


------------------------------ controller -----------------------------
    CU: ENTITY work.controller(behavioural)
        PORT MAP (
            clk                 => clk,
            rst                 => rst,

            xchg                => xchg,
            inc_adr             => inc_adr,
            
            en_triAccBus        => en_triAccBus    ,
            en_triFlgBus        => en_triFlgBus    ,
            en_triAluBus        => en_triAluBus    ,
            en_triArrBus        => en_triArrBus    ,
            en_bufAdr           => en_bufAdr       ,
            en_bufAdrData       => en_bufAdrData   ,
            en_triDataBuf       => en_triDataBuf   ,
            en_triAdrBuf        => en_triAdrBuf    ,
            en_triExtDataBuf    => en_triExtDataBuf,
            en_triExtDataBus    => en_triExtDataBus,
            en_triIntDataBus    => en_triIntDataBus,

            ld_acc              => ld_acc        , 
            ld_tmp              => ld_tmp        , 
            ld_flg              => ld_flg        , 
            ld_inst             => ld_inst       , 
            sel_operation       => sel_operation , 
            sel_inArrData       => sel_inArrData , 
            sel_outArrData      => sel_outArrData, 
            sel_outArrAdr       => sel_outArrAdr , 

            decSP               => decSP,   
            incSP               => incSP,
            incPC               => incPC,

            ld_invisibleCY      => ld_invisibleCY ,
            sel_invisibleCY     => sel_invisibleCY,

            inst                => inst,   
            
            flag                => flag,

            x1                  => x1,
            x2                  => x2,
            CLKOUT              => CLKOUT,
            READY               => READY,
            RDbar               => RDbar,
            WRbar               => WRbar,
            ALE                 => ALE  ,
            S0                  => S0,
            S1                  => S1,
            IO_Mbar             => IO_Mbar,
            HOLD                => HOLD,
            HLDA                => HLDA,
            INTAbar             => INTAbar,
            INTR                => INTR ,
            RST55               => RST55,
            RST65               => RST65,
            RST75               => RST75,
            TRAP                => TRAP ,
            SID                 => SID  ,
            SOD                 => SOD  ,
            RESETOUT            => RESETOUT
        );  
    

    


       

    
END behavioural;
