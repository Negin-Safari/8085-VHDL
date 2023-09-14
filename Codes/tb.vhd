LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY tb IS
END tb;

ARCHITECTURE behavioral OF tb IS
    SIGNAL clk                : STD_LOGIC := '0';
    SIGNAL READY              : STD_LOGIC;
    SIGNAL CLK_OUT            : STD_LOGIC;
    SIGNAL RESETINbar         : STD_LOGIC;
    SIGNAL RESETOUT           : STD_LOGIC;
    SIGNAL RDbar              : STD_LOGIC;
    SIGNAL WRbar              : STD_LOGIC;
    SIGNAL ALE                : STD_LOGIC;
    SIGNAL S0                 : STD_LOGIC;
    SIGNAL S1                 : STD_LOGIC;
    SIGNAL IO_Mbar            : STD_LOGIC;
    SIGNAL HLDA               : STD_LOGIC;
    SIGNAL SOD                : STD_LOGIC;
    SIGNAL INTAbar            : STD_LOGIC;
    SIGNAL address_data_bus   : STD_LOGIC_VECTOR(8-1 downto 0);
    SIGNAL address_bus        : STD_LOGIC_VECTOR(8-1 downto 0);
    SIGNAL memAddress         : STD_LOGIC_VECTOR(2*8-1 downto 0);

    SIGNAL CLKOUT             : STD_LOGIC;
    SIGNAL RST55              : STD_LOGIC;
    SIGNAL RST65              : STD_LOGIC;
    SIGNAL RST75              : STD_LOGIC;
    SIGNAL TRAP               : STD_LOGIC;
BEGIN

    clk <= NOT clk AFTER 10 ns;
    RESETINbar <= '0', '1' AFTER 30 ns;
    READY <= '1';

    DUT : ENTITY WORK.cpu (behavioural)
        PORT MAP (
            clk                 => clk             ,  
            x1                  => 'Z'              ,  
            x2                  => 'Z'              ,  
            CLKOUT              => CLKOUT          ,  
            READY               => READY           ,  
            RDbar               => RDbar           ,  
            WRbar               => WRbar           ,  
            ALE                 => ALE             ,  
            S0                  => S0              ,  
            S1                  => S1              ,  
            IO_Mbar             => IO_Mbar         ,  
            HOLD                => 'Z'            ,  
            HLDA                => HLDA            ,  
            INTAbar             => INTAbar         ,  
            INTR                => 'Z'            ,  
            RST55               => 'Z'           ,  
            RST65               => 'Z'           ,  
            RST75               => 'Z'           ,  
            TRAP                => 'Z'           ,  
            SID                 => 'Z'           ,  
            SOD                 => SOD             ,  
            RESETINbar          => RESETINbar      ,  
            RESETOUT            => RESETOUT        ,  
            address_bus         => address_bus     ,  
            address_data_bus    => address_data_bus
        );



    memAddress <= address_bus & address_data_bus;

    MUT : ENTITY WORK.memory (behavioral)
        GENERIC MAP (
            dataWidth    => 8,
            addressWidth => 16,
            blockSize    => 2 ** 16
        )
        PORT MAP (
            clk         => clk,
            readmem     => RDbar,            
            writemem    => WRbar,            
            addressBus  => memAddress,
            dataBus     => address_data_bus
        );

    SIMULATION : PROCESS
    BEGIN
        WAIT UNTIL (RESETINbar = '1');
        WAIT FOR 2000 ns;
        WAIT;
    END PROCESS;

END ARCHITECTURE behavioral;
