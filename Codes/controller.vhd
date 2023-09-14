Library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY controller IS
    PORT (
        clk : IN STD_LOGIC;
        rst : IN STD_LOGIC;

        xchg : OUT STD_LOGIC;
        inc_adr : OUT STD_LOGIC;
        
        en_triAccBus        : OUT STD_LOGIC;
        en_triFlgBus        : OUT STD_LOGIC;
        en_triAluBus        : OUT STD_LOGIC;
        en_triArrBus        : OUT STD_LOGIC;
        en_bufAdr           : OUT STD_LOGIC;
        en_bufAdrData       : OUT STD_LOGIC;
        en_triDataBuf       : OUT STD_LOGIC;
        en_triAdrBuf        : OUT STD_LOGIC;
        en_triExtDataBuf    : OUT STD_LOGIC;
        en_triExtDataBus    : OUT STD_LOGIC;
        en_triIntDataBus    : OUT STD_LOGIC;

        ld_acc              : OUT STD_LOGIC;
        ld_tmp              : OUT STD_LOGIC;
        ld_flg              : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
        ld_inst             : OUT STD_LOGIC;
        sel_operation       : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
        sel_inArrData       : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
        sel_outArrData      : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
        sel_outArrAdr       : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);

        decSP : OUT STD_LOGIC;   
        incSP : OUT STD_LOGIC;
        incPC : OUT STD_LOGIC;

        ld_invisibleCY : OUT STD_LOGIC;
        sel_invisibleCY : OUT STD_LOGIC;


        inst : IN STD_LOGIC_VECTOR(7 DOWNTO 0);

        flag : IN STD_LOGIC_VECTOR(7 DOWNTO 0);

        x1 : IN STD_LOGIC;
        x2 : IN STD_LOGIC;

        CLKOUT : OUT STD_LOGIC;

        READY : IN STD_LOGIC;

        RDbar : OUT STD_LOGIC;
        WRbar : OUT STD_LOGIC;
        ALE   : OUT STD_LOGIC;

        S0 : OUT STD_LOGIC;
        S1 : OUT STD_LOGIC;
        IO_Mbar   : OUT STD_LOGIC;

        HOLD : IN STD_LOGIC;
        HLDA : IN STD_LOGIC;

        INTAbar : OUT STD_LOGIC;
        INTR    : IN  STD_LOGIC;
        RST55   : IN  STD_LOGIC;
        RST65   : IN  STD_LOGIC;
        RST75   : IN  STD_LOGIC;
        TRAP    : IN  STD_LOGIC;
        SID     : IN  STD_LOGIC;
        SOD     : OUT  STD_LOGIC;

       -- RESETINbar : IN STD_LOGIC; -- in top
        RESETOUT : OUT STD_LOGIC

    );
END controller;

ARCHITECTURE behavioural OF controller IS
    TYPE state IS (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17);
    SIGNAL pstate  : state;
    SIGNAL nstate  : state;
    
BEGIN

    sequential : PROCESS (clk, rst) BEGIN
        IF rst = '1' THEN
            pstate <= T0;
        ELSIF (clk = '1' AND clk'event) THEN
            pstate <= nstate;
        END IF;
    END PROCESS sequential;

    PROCESS (pstate, inst) BEGIN
        CASE pstate IS
            WHEN T0=>
                nstate <= T1;

            WHEN T1=>
                nstate <= T2;

            WHEN T2=>
                nstate <= T3;

            WHEN T3=>
                IF (inst(7 DOWNTO 6) = "01" ) THEN 
                    IF((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOV R M
                        nstate <= T4;
                    ELSIF((inst(5 DOWNTO 3) = "110") AND (inst(2 DOWNTO 0) /= "110"))  THEN -- MOV M R
                        nstate <= T4;
                    ELSIF ((inst(5 DOWNTO 3) /= "110")  AND (inst(2 DOWNTO 0) /= "110")) THEN -- MOV RR
                        nstate <= T0;
                    END IF;
                ELSIF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOVI R
                        nstate <= T4;
                    ELSIF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) = "110")) THEN  -- MOVI M
                        nstate <= T4;
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) THEN  -- LXI
                        nstate <= T4;
                    ELSIF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T4;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T4;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T4;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T4;
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        nstate <= T4;
                    ELSIF (inst(2 DOWNTO 0) = "011") THEN  --INX -- DCX --
                        nstate <= T4;
                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN  --DAD
                        nstate <= T4;
                    
                    ELSIF((inst(7 DOWNTO 5) = "000") AND (inst(2 DOWNTO 0) = "010")) THEN -- LDAX, STAX
                        nstate <= T4;
    
                    ELSIF( ((inst(7 DOWNTO 4) = "0000") AND (inst(2 DOWNTO 0) = "111")) OR  
                        ((inst(7 DOWNTO 5) = "001") AND (inst(2 DOWNTO 0) = "111") AND (inst(4 DOWNTO 3) /= "00")) )THEN -- RLC RRC CMA STC CMC
                        nstate <= T0;

                    ELSE -- INR R --DCR R 
                        nstate <= T0;
                    END IF;

                

                ELSIF (inst(7 DOWNTO 6) = "10" ) THEN
                    IF ((inst(5) = '0') AND (inst(2 DOWNTO 0) /= "110")) THEN -- ADDr -- ADCr, SUBr, SBBr
                        nstate <= T0;
                    ELSIF ((inst(5) = '0') AND (inst(2 DOWNTO 0) = "110")) THEN -- ADD M -- ADC M, SUB M, SBB M
                        nstate <= T4;
                    ELSIF ((inst(5) = '1') AND (inst(2 DOWNTO 0) /= "110")) THEN -- ANAr -- XRAr, ORAr, CMPr
                        nstate <= T0;
                    END IF;

                ELSIF((inst(7 DOWNTO 5) = "110") AND (inst(2 DOWNTO 0) = "110")) THEN -- ADI -- ACI , SUI , SBI
                    nstate <= T4;

                ELSIF(inst = "11101011") THEN -- XCHG
                    nstate <= T0;
                ELSIF(inst = "11111110") THEN -- CPI
                    nstate <= T4;
                ELSIF ((inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010"))) THEN -- jumps
                    nstate <= T4;
                ELSIF (inst = "11001001") THEN -- RET
                    nstate <= T4;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T4;
                ELSE 
                        nstate <= T0;
                END IF;

            
            WHEN T4=>
                IF (inst(7 DOWNTO 6) = "01" ) THEN 
                    IF((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOV R M
                        nstate <= T5;
                    ELSIF((inst(5 DOWNTO 3) = "110")  AND (inst(2 DOWNTO 0) /= "110"))  THEN -- MOV M R
                        nstate <= T5;
                    ELSE 

                    END IF;
                ELSIF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOVI R
                        nstate <= T5;
                    ELSIF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) = "110")) THEN  -- MOVI M
                        nstate <= T5;
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) THEN  -- LXI
                        nstate <= T5;
                    ELSIF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T5;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T5;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T5;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T5;
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        nstate <= T5;
                    ELSIF (inst(2 DOWNTO 0) = "011") THEN  --INX -- DCX --
                        nstate <= T5;
                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN  --DAD
                        nstate <= T5;
                    ELSIF((inst(7 DOWNTO 5) = "000") AND (inst(2 DOWNTO 0) = "010")) THEN -- LDAX, STAX
                        nstate <= T5;
                    END IF;

                

                ELSIF((inst(7 DOWNTO 5) = "110") AND (inst(2 DOWNTO 0) = "110")) THEN -- ADI -- ACI , SUI , SBI
                    nstate <= T5;

                ELSIF (inst(7 DOWNTO 6) = "10" ) THEN
                    IF ((inst(5) = '0') AND (inst(2 DOWNTO 0) = "110")) THEN -- ADD M -- ADC M, SUB M, SBB M
                        nstate <= T5;
                    END IF;
                ELSIF(inst = "11111110") THEN -- CPI
                    nstate <= T5;
                ELSIF (inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010")) THEN -- jumps
                    nstate <= T5;
                ELSIF (inst = "11001001") THEN -- RET
                    nstate <= T5;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T5;
                END IF;

            WHEN T5=>
                IF (inst(7 DOWNTO 6) = "01" ) THEN 
                    IF((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOV R M
                        nstate <= T6;
                    ELSIF((inst(5 DOWNTO 3) = "110")  AND (inst(2 DOWNTO 0) /= "110"))  THEN -- MOV M R
                        nstate <= T6;
                    ELSE 

                    END IF;
                ELSIF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOVI R
                        nstate <= T6;
                    ELSIF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) = "110")) THEN  -- MOVI M
                        nstate <= T6;
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) THEN  -- LXI
                        nstate <= T6;
                    ELSIF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T6;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T6;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T6;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T6;
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        nstate <= T6;
                    ELSIF (inst(2 DOWNTO 0) = "011") THEN  --INX -- DCX --
                        nstate <= T0;
                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN  --DAD
                        nstate <= T6;
                    ELSIF((inst(7 DOWNTO 5) = "000") AND (inst(2 DOWNTO 0) = "010")) THEN -- LDAX, STAX
                        nstate <= T6;
                    END IF;

                
                ELSIF (inst(7 DOWNTO 6) = "10" ) THEN
                    IF ((inst(5) = '0') AND (inst(2 DOWNTO 0) = "110")) THEN -- ADD M -- ADC M, SUB M, SBB M
                        nstate <= T6;
                    END IF;

                ELSIF((inst(7 DOWNTO 5) = "110") AND (inst(2 DOWNTO 0) = "110")) THEN -- ADI -- ACI , SUI , SBI
                    nstate <= T6;
                ELSIF(inst = "11111110") THEN -- CPI
                    nstate <= T6;
                ELSIF (inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010")) THEN -- jumps
                    nstate <= T6;
                ELSIF (inst = "11001001") THEN -- RET
                    nstate <= T6;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T6;
                END IF;
            
            WHEN T6=>
                IF (inst(7 DOWNTO 6) = "01" ) THEN 
                    IF((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOV R M
                        nstate <= T0;
                    ELSIF((inst(5 DOWNTO 3) = "110")  AND (inst(2 DOWNTO 0) /= "110"))  THEN -- MOV M R
                        nstate <= T0;
                    ELSE 

                    END IF;
                ELSIF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOVI R
                        nstate <= T0;
                    ELSIF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) = "110")) THEN  -- MOVI M
                        nstate <= T7;
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) THEN  -- LXI
                        nstate <= T7;
                    ELSIF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T7;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T7;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T7;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T7;
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        nstate <= T7;
                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN  --DAD
                        nstate <= T7;
                    ELSIF((inst(7 DOWNTO 5) = "000") AND (inst(2 DOWNTO 0) = "010")) THEN -- LDAX, STAX
                        nstate <= T0;
                    END IF;
                
                ELSIF (inst(7 DOWNTO 6) = "10" ) THEN
                    IF ((inst(5) = '0') AND (inst(2 DOWNTO 0) = "110")) THEN -- ADD M -- ADC M, SUB M, SBB M
                        nstate <= T0;
                    END IF;

                ELSIF((inst(7 DOWNTO 5) = "110") AND (inst(2 DOWNTO 0) = "110")) THEN -- ADI -- ACI , SUI , SBI
                    nstate <= T0;
                ELSIF(inst = "11111110") THEN -- CPI
                    nstate <= T0;
                ELSIF (inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010")) THEN -- jumps
                    nstate <= T7;
                ELSIF (inst = "11001001") THEN -- RET
                    nstate <= T7;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T7;
                END IF;
            
            WHEN T7 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) = "110")) THEN -- MOVI M
                        nstate <= T8;
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) THEN  -- LXI
                        nstate <= T8;
                    ELSIF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T8;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T8;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T8;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T8;
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        nstate <= T8;
                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN  --DAD
                        nstate <= T8;
                    END IF;
                ELSIF (inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010")) THEN -- jumps
                    nstate <= T8;
                ELSIF (inst = "11001001") THEN -- RET
                    nstate <= T8;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T8;
                END IF;
            
            WHEN T8 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) = "110")) THEN -- MOVI M
                        nstate <= T9;
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) THEN  -- LXI
                        nstate <= T9;
                    ELSIF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T9;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T9;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T9;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T9;
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        nstate <= T9;
                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN  --DAD
                        nstate <= T9;
                    END IF;
                ELSIF (inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010")) THEN -- jumps
                    nstate <= T9;
                ELSIF (inst = "11001001") THEN -- RET
                    nstate <= T9;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T9;
                END IF;

            WHEN T9 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) = "110")) THEN -- MOVI M
                        nstate <= T0;
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) THEN  -- LXI
                        nstate <= T0;
                    ELSIF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T10;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T10;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T10;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T10;
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        nstate <= T0;
                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN  --DAD
                        nstate <= T0;
                    END IF;
                ELSIF (inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010")) THEN -- jumps
                    nstate <= T0;
                ELSIF (inst = "11001001") THEN -- RET
                    nstate <= T0;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T10;
                END IF;

            WHEN T10 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T11;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T11;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T11;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T11;
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T11;
                END IF;

            WHEN T11 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T12;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T12;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T12;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T12;
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T12;
                END IF;

            WHEN T12 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "111010") THEN  -- LDA
                        nstate <= T0;
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN  -- STA
                        nstate <= T0;
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T13;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T13;
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T13;
                END IF;

            WHEN T13 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T14;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T14;
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T14;
                END IF;

            WHEN T14 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T15;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T15;
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T15;
                END IF;

            WHEN T15 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "101010") THEN  -- LHLD
                        nstate <= T0;
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN  -- SHLD
                        nstate <= T0;
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL
                    nstate <= T16;
                END IF;

            WHEN T16 =>
                IF (inst = "11001101") THEN -- CALL
                    nstate <= T17;
                END IF;

            WHEN T17 =>
                IF (inst = "11001101") THEN -- CALL
                    nstate <= T0;
                END IF;

            WHEN OTHERS =>
                nstate <= T0;
        END CASE;
    END PROCESS;


    PROCESS (pstate, inst) BEGIN
        en_triAccBus     <= '0';
        en_triFlgBus     <= '0';
        en_triAluBus     <= '0';
        en_triArrBus     <= '0';
        en_bufAdr        <= '0';
        en_bufAdrData    <= '0';
        en_triDataBuf    <= '0';
        en_triAdrBuf     <= '0';
        en_triExtDataBuf <= '0';
        en_triExtDataBus <= '0';
        en_triIntDataBus <= '0';
        ld_acc           <= '0'    ; 
        ld_tmp           <= '0'    ; 
        ld_flg           <= "00000"; 
        ld_inst          <= '0'    ; 
        sel_operation    <= "11111" ; 
        sel_inArrData    <= "1111" ; 
        sel_outArrData   <= "1111" ; 
        sel_outArrAdr    <= "000"  ; 
        decSP            <= '0';
        incSP            <= '0';
        incPC            <= '0';
        RDbar            <= '1';  
        WRbar            <= '1';  
        ALE              <= '0';    
        S0               <= '1';      
        S1               <= '1';      
        IO_Mbar          <= '0'; 
        INTAbar          <= '1';
        xchg             <= '0';
        inc_adr          <= '0';
        ld_invisibleCY   <= '0';
        sel_invisibleCY  <= '0';

        CASE pstate IS
            WHEN T0=>
                ALE <= '1';
                RDbar <= '1';
                S1 <= '1';
                S0 <= '1';
                en_triAdrBuf <= '1';
                en_bufAdrData <= '1';
                en_triExtDataBus <= '1';
                en_bufAdr <= '1';
                sel_outArrAdr <= "001";
                

            WHEN T1=>
                RDbar <= '0';
                S1 <= '1';
                S0 <= '1';
                incPC <= '1';
                en_triExtDataBus <= '1';
                sel_outArrAdr <= "001";
                

            WHEN T2=>
                RDbar <= '1';
                S1 <= '1';
                S0 <= '1';
                en_triExtDataBuf <= '1';
                en_bufAdrData <= '1';
                en_triIntDataBus <= '1';
                sel_outArrAdr <= "001";
                ld_inst <= '1';
                

            WHEN T3=>
                IF (inst(7 DOWNTO 6) = "01" ) THEN 
                    IF((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOV R M
                        S1 <= '1';
                        S0 <= '1';
                        ---- nothing according to waveform
                    ELSIF((inst(5 DOWNTO 3) = "110")  AND (inst(2 DOWNTO 0) /= "110"))  THEN -- MOV M R
                        S1 <= '1';
                        S0 <= '1';
                        ---- nothing according to waveform
                    ELSIF((inst(5 DOWNTO 3) /= "110")  AND (inst(2 DOWNTO 0) /= "110")) THEN -- MOV RR
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '1';
                        sel_outArrData <= ('0' & inst(2 DOWNTO 0)); -- source
                        sel_inArrData <= ('0' & inst(5 DOWNTO 3)); -- dest
                        en_triArrBus <= '1';
                        IF (inst(2 DOWNTO 0) = "111") THEN -- source
                            en_triArrBus <= '0';
                            en_triAccBus <= '1';
                        END IF;
                        IF (inst(5 DOWNTO 3) = "111") THEN -- dest
                            ld_acc <= '1';
                            sel_inArrData <= "1111";
                        END IF;
                    END IF;
                    
                ELSIF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ( ((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) /= "110")) OR -- MOVI R
                        ((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) = "110")) OR -- MOVI M 
                        ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) OR  -- LXI
                        (inst(5 DOWNTO 0) = "111010") OR  -- LDA
                        (inst(5 DOWNTO 0) = "110010") OR  -- STA 
                        (inst(5 DOWNTO 0) = "101010") OR  -- LHLD
                        (inst(5 DOWNTO 0) = "100010")   OR -- SHLD
                        ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) OR --INR M -- DCR M --
                        (inst(2 DOWNTO 0) = "011") OR --INX -- DCX -- 
                        (inst(3 DOWNTO 0) = "1001") )THEN  --DAD
                        S1 <= '1';
                        S0 <= '1'; --############################
                        
                    ELSIF ((inst(2 DOWNTO 1) = "10") AND (inst(5 DOWNTO 3) /= "110"))  THEN  -- INR R -- DCR R
                        en_triAluBus <= '1';
                        en_triArrBus <= '1';
                        ld_flg <= "10111";
                        ld_tmp <= '1';
                        sel_outArrData <= ('0'& inst(5 DOWNTO 3)); --src
                        sel_inArrData <= ('0' & inst(5 DOWNTO 3)); --dst
                        IF (inst(0) = '0') THEN
                            sel_operation <= "01001";
                        ELSE
                            sel_operation <= "01010";
                        END IF;
                        
                        IF (inst(5 DOWNTO 3) = "111") THEN -- acc
                            en_triArrBus <= '0';
                            en_triAccBus <= '1';
                            ld_acc <= '1';
                            sel_inArrData <= "1111";
                            sel_outArrData <= "1111";
                        END IF;
                    
                    ELSIF((inst(7 DOWNTO 5) = "000") AND (inst(2 DOWNTO 0) = "010")) THEN -- LDAX, STAX
                        IF(((inst(4 DOWNTO 3) = "11") OR (inst(4 DOWNTO 3) = "01")) OR -- LDAX
                        ((inst(4 DOWNTO 3) = "00") OR (inst(4 DOWNTO 3) = "10"))) THEN --STAX
                            S1 <= '1';
                            S0 <= '1';
                        END IF;

                    ELSIF( ((inst(7 DOWNTO 4) = "0000") AND (inst(2 DOWNTO 0) = "111")) OR  
                        ((inst(7 DOWNTO 5) = "001") AND (inst(2 DOWNTO 0) = "111") AND (inst(4 DOWNTO 3) /= "00")) )THEN -- RLC RRC CMA STC CMC
                        en_triAluBus <= '1';
                        ld_flg <= "01000";
                        ld_acc <= '1';
        
                        IF (inst(5 DOWNTO 3) = "000") THEN --RLC
                            sel_operation <= "01101";
                        ELSIF (inst(5 DOWNTO 3) = "001") THEN -- RRC
                            sel_operation <= "01110";
                        ELSIF (inst(5 DOWNTO 3) = "101") THEN -- CMA
                            sel_operation <= "00110";
                            ld_flg <= "00000"; -- no carry save
                        ELSIF (inst(5 DOWNTO 3) = "110") THEN -- STC
                            sel_operation <= "00100";
                            ld_acc <= '0';
                        ELSIF (inst(5 DOWNTO 3) = "111") THEN -- CMC
                            sel_operation <= "00101";
                            ld_acc <= '0';
                        END IF;

                    END IF;
                
                
                    
                
                    
                    
                    
                ELSIF (inst(7 DOWNTO 6) = "10" ) THEN
                    IF ((inst(5) = '0') AND (inst(2 DOWNTO 0) /= "110")) THEN -- ADDr -- ADCr, SUBr, SBBr
                        en_triAluBus <= '1';
                        en_triArrBus <= '1';
                        ld_flg <= "11111";
                        ld_tmp <= '1';
                        sel_outArrData <= ('0'& inst(2 DOWNTO 0)); --src

                        IF (inst(4 DOWNTO 3) = "00") THEN
                            sel_operation <= "00111";
                        ELSIF (inst(4 DOWNTO 3) = "01") THEN
                            sel_operation <= "01011";
                        ELSIF (inst(4 DOWNTO 3) = "10") THEN
                            sel_operation <= "01000";
                        ELSE
                            sel_operation <= "01100";
                        END IF;

                        ld_acc <= '1';
                        sel_inArrData <= "1111";
                        IF (inst(2 DOWNTO 0) = "111") THEN -- acc
                            en_triArrBus <= '0';
                            en_triAccBus <= '1';
                        END IF;
                        
                    ELSIF ((inst(5) = '0') AND (inst(2 DOWNTO 0) = "110")) THEN -- ADD M -- ADC M, SUB M, SBB M
                        S1 <= '1';
                        S0 <= '1';
                    ELSIF ((inst(5) = '1') AND (inst(2 DOWNTO 0) /= "110")) THEN -- ANAr -- XRAr, ORAr, CMPr
                        en_triAluBus <= '1';
                        en_triArrBus <= '1';
                        ld_flg <= "11111";
                        ld_tmp <= '1';
                        sel_outArrData <= ('0'& inst(2 DOWNTO 0)); --src

                        ld_acc <= '1';
                        sel_inArrData <= "1111";
                        IF (inst(2 DOWNTO 0) = "111") THEN -- acc
                            en_triArrBus <= '0';
                            en_triAccBus <= '1';
                        END IF;

                        IF (inst(4 DOWNTO 3) = "00") THEN --AND
                            sel_operation <= "00010";
                        ELSIF (inst(4 DOWNTO 3) = "01") THEN -- XOR
                            sel_operation <= "00001";
                        ELSIF (inst(4 DOWNTO 3) = "10") THEN -- OR
                            sel_operation <= "00011";
                        ELSE -- CMPr A - r = sub
                            sel_operation <= "01000";
                            -- result of sub must not be saved :
                            ld_acc <= '0';
                        END IF;
                    END IF;
                     
                ELSIF((inst(7 DOWNTO 5) = "110") AND (inst(2 DOWNTO 0) = "110")) THEN -- ADI -- ACI , SUI , SBI
                    S1 <= '1';
                    S0 <= '1';
                ELSIF(inst = "11111110") THEN -- CPI
                    S1 <= '1';
                    S0 <= '1';                       
                ELSIF(inst = "11101011") THEN -- XCHG
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '1';
                    xchg <= '1';
                ELSIF ((inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010")) )THEN -- jumps
                    S1 <= '1';
                    S0 <= '1';
                ELSIF (inst = "11001001") THEN -- RET
                    S1 <= '1';
                    S0 <= '1';
                    IO_Mbar <= '1';
                ELSIF (inst = "11001101") THEN -- CALL
                    S1 <= '1';
                    S0 <= '1';
                    IO_Mbar <= '1';
                END IF;
                
            WHEN T4=>
                IF (inst(7 DOWNTO 6) = "01" ) THEN 
                    IF((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOV R M
                        ALE <= '1';
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "011";
                    ELSIF((inst(5 DOWNTO 3) = "110") AND (inst(2 DOWNTO 0) /= "110"))  THEN -- MOV M R
                        ALE <= '1';
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "011";
                    END IF;
                ELSIF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110") OR ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) OR
                        (inst(5 DOWNTO 0) = "111010") OR (inst(5 DOWNTO 0) = "110010") OR (inst(5 DOWNTO 0) = "101010") OR (inst(5 DOWNTO 0) = "100010")) THEN 
                        -- MOVI R -- MOVI M -- LXI --LDA --STA -- LHLD --SHLD
                        ALE <= '1';
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "001";
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        ALE <= '1';
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "011";
                    ELSIF (inst(2 DOWNTO 0) = "011") THEN --INX --DCX
                        en_triAluBus <= '1';
                        en_triArrBus <= '1';
                        ld_flg <= "00000";
                        ld_tmp <= '1';
                        sel_outArrData <= ('0'& inst(5 DOWNTO 4) & '1'); --src
                        sel_inArrData <= ('0' & inst(5 DOWNTO 4) & '1'); --dst
                        ld_invisibleCY   <= '1';
                        --sel_invisibleCY  <= '0';
                        IF (inst(3) = '0') THEN
                            sel_operation <= "01001"; -- normal inc
                        ELSE
                            sel_operation <= "01010"; -- normal dec
                        END IF;

                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN -- DAD
                        sel_inArrData <= ("1011"); -- dest Z
                         -- source Acc
                        en_triAccBus <= '1';
                        -- storing Acc in Z to save it


                    ELSIF((inst(7 DOWNTO 5) = "000") AND (inst(2 DOWNTO 0) = "010")) THEN -- LDAX, STAX
                        IF((inst(4 DOWNTO 3) = "11") OR (inst(4 DOWNTO 3) = "01")) THEN -- LDAX
                            ALE <= '1';
                            RDbar <= '1';
                            S1 <= '1';
                            S0 <= '0';
                            en_triAdrBuf <= '1';
                            en_bufAdrData <= '1';
                            en_triExtDataBus <= '1';
                            en_bufAdr <= '1';
                            IF (inst(4 DOWNTO 3) = "01") THEN
                                sel_outArrAdr <= "101";
                            ELSE 
                                sel_outArrAdr <= "110";
                            END IF;
    
                        ELSIF((inst(4 DOWNTO 3) = "00") OR (inst(4 DOWNTO 3) = "10")) THEN -- STAX
                            ALE <= '1';
                            WRbar <= '1';
                            S1 <= '0';
                            S0 <= '1';
                            en_triAdrBuf <= '1';
                            en_bufAdrData <= '1';
                            en_triExtDataBus <= '1';
                            en_bufAdr <= '1';
                            IF (inst(4 DOWNTO 3) = "00") THEN
                                sel_outArrAdr <= "101";
                            ELSE 
                                sel_outArrAdr <= "110";
                            END IF;
                        END IF;

                    END IF;

                

                ELSIF (inst(7 DOWNTO 6) = "10" ) THEN
                    IF ((inst(5) = '0') AND (inst(2 DOWNTO 0) = "110")) THEN -- ADD M -- ADC M, SUB M, SBB M
                        ALE <= '1';
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "011";
                    END IF;
                    
                ELSIF( ((inst(7 DOWNTO 5) = "110") AND (inst(2 DOWNTO 0) = "110")) OR -- ADI -- ACI , SUI , SBI
                    (inst = "11111110") ) THEN -- CPI
                    ALE <= '1';
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triAdrBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdr <= '1';
                    sel_outArrAdr <= "001";
                ELSIF ((inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010"))) THEN -- jumps
                    ALE <= '1';
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triAdrBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdr <= '1';
                    sel_outArrAdr <= "001";
                ELSIF (inst = "11001001") THEN -- RET
                    ALE <= '1';
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triAdrBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdr <= '1';
                    sel_outArrAdr <= "010";
                ELSIF (inst = "11001101") THEN -- CALL
                    S1 <= '1';
                    S0 <= '1';
                    IO_Mbar <= '1';
                END IF;

            WHEN T5=>
                IF (inst(7 DOWNTO 6) = "01" ) THEN 
                    IF((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOV R M
                        RDbar <= '0';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "011";
                    ELSIF((inst(5 DOWNTO 3) = "110")  AND (inst(2 DOWNTO 0) /= "110"))  THEN -- MOV M R
                        WRbar <= '0';
                        S1 <= '0';
                        S0 <= '1';
                        en_triDataBuf <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdrData <= '1';
                        sel_outArrAdr <= "011";
                        sel_outArrData <= ('0' & inst(2 DOWNTO 0)); -- source
                        en_triArrBus <= '1';
                        IF (inst(2 DOWNTO 0) = "111") THEN -- source
                            en_triArrBus <= '0';
                            en_triAccBus <= '1';
                        END IF;
                    END IF;
                ELSIF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110") OR ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) OR 
                        (inst(5 DOWNTO 0) = "111010") OR (inst(5 DOWNTO 0) = "110010") OR (inst(5 DOWNTO 0) = "101010") OR (inst(5 DOWNTO 0) = "100010")) THEN 
                        -- MOVI R -- MOVI M -- LXI --LDA --STA --LHLD -- SHLD
                        RDbar <= '0';
                        S1 <= '1';
                        S0 <= '0';
                        incPC <= '1';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "001";
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        RDbar <= '0';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "011";
                    ELSIF (inst(2 DOWNTO 0) = "011") THEN --INX --DCX
                        en_triAluBus <= '1';
                        en_triArrBus <= '1';
                        ld_flg <= "00000";
                        ld_tmp <= '1';
                        sel_outArrData <= ('0'& inst(5 DOWNTO 4) & '0'); --src
                        sel_inArrData <= ('0' & inst(5 DOWNTO 4) & '0'); --dst
                        --ld_invisibleCY   <= '1';
                        sel_invisibleCY  <= '1';
                        IF (inst(3) = '0') THEN
                            sel_operation <= "01111"; -- cond inc
                        ELSE
                            sel_operation <= "10000"; -- cond dec
                        END IF;

                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN -- DAD
                        sel_outArrData <= ("0101"); -- source L
                        en_triArrBus <= '1';
                        ld_acc <= '1'; -- dest
                        sel_inArrData <= "1111";
                        -- storing L in Acc for operation

                    ELSIF((inst(7 DOWNTO 5) = "000") AND (inst(2 DOWNTO 0) = "010")) THEN -- LDAX, STAX
                        IF((inst(4 DOWNTO 3) = "11") OR (inst(4 DOWNTO 3) = "01")) THEN -- LDAX
                            RDbar <= '0';
                            S1 <= '1';
                            S0 <= '0';
                            en_triExtDataBus <= '1';
                            IF (inst(4 DOWNTO 3) = "01") THEN
                                sel_outArrAdr <= "101";
                            ELSE 
                                sel_outArrAdr <= "110";
                            END IF;
                        ELSIF((inst(4 DOWNTO 3) = "00") OR (inst(4 DOWNTO 3) = "10")) THEN -- STAX
                            WRbar <= '0';
                            S1 <= '0';
                            S0 <= '1';
                            en_triDataBuf <= '1';
                            en_triExtDataBus <= '1';
                            en_bufAdrData <= '1';
                            en_triAccBus <= '1';
                            IF (inst(4 DOWNTO 3) = "00") THEN
                                sel_outArrAdr <= "101";
                            ELSE 
                                sel_outArrAdr <= "110";
                            END IF;
                        END IF;

                    END IF;
                
                ELSIF (inst(7 DOWNTO 6) = "10" ) THEN
                    IF ((inst(5) = '0') AND (inst(2 DOWNTO 0) = "110")) THEN -- ADD M -- ADC M, SUB M, SBB M
                        RDbar <= '0';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "011";
                    END IF;
                    
                ELSIF(( (inst(7 DOWNTO 5) = "110") AND (inst(2 DOWNTO 0) = "110")) OR -- ADI -- ACI , SUI , SBI
                    (inst = "11111110") )THEN -- CPI
                    RDbar <= '0';
                    S1 <= '1';
                    S0 <= '0';
                    incPC <= '1';
                    en_triExtDataBus <= '1';
                    sel_outArrAdr <= "001";
                ELSIF ((inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010"))) THEN -- jumps
                    RDbar <= '0';
                    S1 <= '1';
                    S0 <= '0';
                    incPC <= '1';
                    en_triExtDataBus <= '1';
                    sel_outArrAdr <= "001";
                ELSIF (inst = "11001001") THEN -- RET
                    RDbar <= '0';
                    S1 <= '1';
                    S0 <= '0';
                    en_triExtDataBus <= '1';
                    sel_outArrAdr <= "010";
                ELSIF (inst = "11001101") THEN -- CALL
                    S1 <= '1';
                    S0 <= '1';
                    IO_Mbar <= '1';

                END IF;

            WHEN T6=>
                IF (inst(7 DOWNTO 6) = "01" ) THEN 
                    IF((inst(2 DOWNTO 0) = "110") AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOV R M
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        sel_outArrAdr <= "011";
                        sel_inArrData <= ('0' & inst(5 DOWNTO 3));
                        IF (inst(5 DOWNTO 3) = "111")  THEN -- dest
                            ld_acc <= '1';
                            sel_inArrData <= "1111";
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "110") AND (inst(2 DOWNTO 0) /= "110"))  THEN -- MOV M R
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "011";
                    END IF;

                ELSIF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) /= "110")) THEN -- MOVI R
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        sel_outArrAdr <= "001"; 
                        sel_inArrData <= ('0' & inst(5 DOWNTO 3)); -- dest
                        IF (inst(5 DOWNTO 3) = "111")  THEN -- dest
                            ld_acc <= '1';
                            sel_inArrData <= "1111";
                        END IF;
                    ELSIF (((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) = "110")) OR (inst(5 DOWNTO 0) = "111010") OR (inst(5 DOWNTO 0) = "110010")
                           OR (inst(5 DOWNTO 0) = "101010") OR (inst(5 DOWNTO 0) = "100010")) THEN -- MOVI M -- LDA --STA --LHLD --SHLD
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        sel_outArrAdr <= "001"; 
                        sel_inArrData <= ("1011"); -- dest Z 
                    
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) THEN  -- LXI
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        sel_outArrAdr <= "001"; 
                        sel_inArrData <= ('0' & inst(5 DOWNTO 4) & '1'); -- dest C E L 
                    
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        en_triAluBus <= '1';
                        ld_flg <= "10111";
                        ld_tmp <= '1';
                        sel_inArrData <= ("1011"); --dst Z temporary
                        IF (inst(0) = '0') THEN
                            sel_operation <= "01001";
                        ELSE
                            sel_operation <= "01010";
                        END IF;

                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN -- DAD
                        sel_outArrData <= ('0' & inst(5 DOWNTO 4) & '1'); -- source C E L SPL (lsb)
                        en_triArrBus <= '1';
                        -- storing L in Acc for operation
                        sel_operation <= "00111"; -- add

                        en_triAluBus <= '1';
                        ld_flg <= "01000";
                        ld_tmp <= '1'; 
                        sel_inArrData <= ("0101"); -- dest  L  (lsb)
                    
                    ELSIF((inst(7 DOWNTO 5) = "000") AND (inst(2 DOWNTO 0) = "010")) THEN -- LDAX, STAX
                        IF((inst(4 DOWNTO 3) = "11") OR (inst(4 DOWNTO 3) = "01")) THEN -- LDAX
                            RDbar <= '1';
                            S1 <= '1';
                            S0 <= '0';
                            en_triExtDataBuf <= '1';
                            en_bufAdrData <= '1';
                            en_triIntDataBus <= '1';
                            ld_acc <= '1';
                            sel_inArrData <= "1111";   
                        ELSIF((inst(4 DOWNTO 3) = "00") OR (inst(4 DOWNTO 3) = "10")) THEN -- STAX
                            WRbar <= '1';
                            S1 <= '0';
                            S0 <= '1';
                            en_triExtDataBus <= '1';
                            IF (inst(4 DOWNTO 3) = "00") THEN
                                sel_outArrAdr <= "101";
                            ELSE 
                                sel_outArrAdr <= "110";
                            END IF;
                        END IF;
                        
                    END IF;

                ELSIF( ((inst(7 DOWNTO 5) = "110") AND (inst(2 DOWNTO 0) = "110")) OR -- ADI -- ACI , SUI , SBI
                    (inst = "11111110") ) THEN --CPI
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triExtDataBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triIntDataBus <= '1';
                    en_triAluBus <= '1';
                    ld_flg <= "11111";
                    ld_tmp <= '1';
                    ld_acc <= '1';
                    sel_inArrData <= "1111";
                    IF (inst(4 DOWNTO 3) = "00") THEN
                        sel_operation <= "00111";
                    ELSIF (inst(4 DOWNTO 3) = "01") THEN
                        sel_operation <= "01011";
                    ELSIF (inst(4 DOWNTO 3) = "10") THEN
                        sel_operation <= "01000";
                    ELSE
                        sel_operation <= "01100";
                    END IF;
                
                    IF (inst = "11111110") THEN
                        ld_acc <= '0';
                        sel_operation <= "01000";
                    END IF;
                
                ELSIF (inst(7 DOWNTO 6) = "10" ) THEN
                    IF ((inst(5) = '0') AND (inst(2 DOWNTO 0) = "110")) THEN -- ADD M -- ADC M, SUB M, SBB M
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        en_triAluBus <= '1';
                        ld_flg <= "11111";
                        ld_tmp <= '1';
                        ld_acc <= '1';
                        sel_inArrData <= "1111";
                        IF (inst(4 DOWNTO 3) = "00") THEN
                            sel_operation <= "00111";
                        ELSIF (inst(4 DOWNTO 3) = "01") THEN
                            sel_operation <= "01011";
                        ELSIF (inst(4 DOWNTO 3) = "10") THEN
                            sel_operation <= "01000";
                        ELSE
                            sel_operation <= "01100";
                        END IF;

                    END IF;

                ELSIF ((inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010"))) THEN -- jumps
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triExtDataBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triIntDataBus <= '1';
                    sel_outArrAdr <= "001"; 
                    sel_inArrData <= ("1011"); -- temporary dest low PC
                ELSIF (inst = "11001001") THEN -- RET
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triExtDataBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triIntDataBus <= '1';
                    sel_outArrAdr <= "010";
                    sel_inArrData <= ("1001");
                ELSIF (inst = "11001101") THEN -- CALL
                    ALE <= '1';
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triAdrBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdr <= '1';
                    sel_outArrAdr <= "001";

                END IF;

            WHEN T7 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) = "110")) THEN -- MOVI M
                        ALE <= '1';
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "011";
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001") OR (inst(5 DOWNTO 0) = "111010") OR
                           (inst(5 DOWNTO 0) = "110010") OR (inst(5 DOWNTO 0) = "101010") OR (inst(5 DOWNTO 0) = "100010")) THEN -- LXI -- LDA --STA --LHLD -- SHLD
                        ALE <= '1';
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "001"; 
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        ALE <= '1';
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "011";

                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN -- DAD 
                        sel_outArrData <= ("0100"); -- source H
                        en_triArrBus <= '1';
                        ld_acc <= '1'; -- dest
                        sel_inArrData <= "1111";
                        -- storing L in Acc for operation
                    END IF;
                
                ELSIF ((inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010"))) THEN -- jumps
                    ALE <= '1';
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triAdrBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdr <= '1';
                    sel_outArrAdr <= "001";

                ELSIF (inst = "11001001") THEN -- RET
                    ALE <= '1';
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triAdrBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdr <= '1';
                    sel_outArrAdr <= "010";
                    inc_adr <= '1';

                 ELSIF (inst = "11001101") THEN -- CALL
                    RDbar <= '0';
                    S1 <= '1';
                    S0 <= '0';
                    incPC <= '1';
                    en_triExtDataBus <= '1';
                    sel_outArrAdr <= "001";


                END IF;

            WHEN T8 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) = "110")) THEN -- MOVI M
                        WRbar <= '0';
                        S1 <= '0';
                        S0 <= '1';
                        en_triDataBuf <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdrData <= '1';
                        sel_outArrAdr <= "011";
                        sel_outArrData <= ("1011"); -- source
                        en_triArrBus <= '1'; 
                    ELSIF (((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) OR (inst(5 DOWNTO 0) = "111010") OR
                           (inst(5 DOWNTO 0) = "110010") OR (inst(5 DOWNTO 0) = "101010") OR (inst(5 DOWNTO 0) = "100010"))THEN -- LXI --LDA --STA --LHLD --SHLD
                        RDbar <= '0';
                        S1 <= '1';
                        S0 <= '0';
                        incPC <= '1';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "001";  
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        WRbar <= '0';
                        S1 <= '0';
                        S0 <= '1';
                        en_triDataBuf <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdrData <= '1';
                        sel_outArrAdr <= "011";
                        sel_outArrData <= ("1011"); -- source Z temp
                        en_triArrBus <= '1';
                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN -- DAD
                        sel_outArrData <= ('0' & inst(5 DOWNTO 4) & '0'); -- source B D H SPH (msb)
                        en_triArrBus <= '1';
                        -- storing L in Acc for operation
                        sel_operation <= "01011"; -- addc

                        en_triAluBus <= '1';
                        ld_flg <= "01000";
                        ld_tmp <= '1'; 
                        sel_inArrData <= ("0100"); -- H  (msb)
                      
                    END IF;

                ELSIF ((inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010"))) THEN -- jumps
                    RDbar <= '0';
                    S1 <= '1';
                    S0 <= '0';
                    incPC <= '1';
                    en_triExtDataBus <= '1';
                    sel_outArrAdr <= "001";
                    
                    sel_outArrData <= ("1011"); -- source Z temp
                    en_triArrBus <= '1';
                    IF((inst(5 DOWNTO 3) = "000") AND (inst(0) = '1')) THEN
                        sel_inArrData <= ("1001"); -- dest low PC
                    ELSIF((inst(5 DOWNTO 3) = "011") AND (inst(0) = '0')) THEN -- on C
                        IF (flag(0)='1') THEN
                            sel_inArrData <= ("1001"); -- dest low PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "010") AND (inst(0) = '0')) THEN -- on no C
                        IF (flag(0)='0') THEN
                            sel_inArrData <= ("1001"); -- dest low PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "001") AND (inst(0) = '0')) THEN -- on Z
                        IF (flag(6)='1') THEN
                            sel_inArrData <= ("1001"); -- dest low PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "000") AND (inst(0) = '0')) THEN -- on no Z
                        IF (flag(6)='0') THEN
                            sel_inArrData <= ("1001"); -- dest low PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "110") AND (inst(0) = '0')) THEN -- on Pos
                        IF (flag(7)='0') THEN
                            sel_inArrData <= ("1001"); -- dest low PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "111") AND (inst(0) = '0')) THEN -- on Min
                        IF (flag(7)='1') THEN
                            sel_inArrData <= ("1001"); -- dest low PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "101") AND (inst(0) = '0')) THEN -- on Par Even
                        IF (flag(2)='1') THEN
                            sel_inArrData <= ("1001"); -- dest low PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "100") AND (inst(0) = '0')) THEN -- on Par Odd
                        IF (flag(2)='0') THEN
                            sel_inArrData <= ("1001"); -- dest low PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    END IF;

                ELSIF (inst = "11001001") THEN -- RET
                    RDbar <= '0';
                    S1 <= '1';
                    S0 <= '0';
                    en_triExtDataBus <= '1';
                    sel_outArrAdr <= "010";
                    inc_adr <= '1';
                 ELSIF (inst = "11001101") THEN -- CALL
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triExtDataBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triIntDataBus <= '1';
                    sel_outArrAdr <= "001"; 
                    sel_inArrData <= ("1011"); -- dest Z temp 
                    
                END IF;

            WHEN T9 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(2 DOWNTO 0) = "110")  AND (inst(5 DOWNTO 3) = "110")) THEN -- MOVI M
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "011"; 
                    ELSIF ((inst(5 DOWNTO 0) = "000001") OR (inst(5 DOWNTO 0) = "010001") OR (inst(5 DOWNTO 0) = "100001")) THEN -- LXI
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        sel_outArrAdr <= "001"; 
                        sel_inArrData <= ('0' & inst(5 DOWNTO 3)); -- dest B D H  b3
                    ELSIF ((inst(5 DOWNTO 0) = "111010") OR (inst(5 DOWNTO 0) = "110010") OR (inst(5 DOWNTO 0) = "101010") OR (inst(5 DOWNTO 0) = "100010")) THEN 
                       -- LDA -- STA --LHLD -- SHLD
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        sel_outArrAdr <= "001"; 
                        sel_inArrData <= ("1010"); -- dest W b3
                    ELSIF ((inst(5 DOWNTO 0) = "110100") OR (inst(5 DOWNTO 0) = "110101")) THEN  --INR M -- DCR M --
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "011"; 
                        --------
                        ------
                        ------
                    ELSIF (inst(3 DOWNTO 0) = "1001") THEN -- DAD ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
                        en_triArrBus <= '1';                             
                        sel_outArrData <= ("1011"); --src
                        ld_acc <= '1';
                        sel_inArrData <= "1111";
                                            
                    END IF;

                ELSIF ((inst = "11000011") OR ((inst(7 DOWNTO 6) = "11") AND (inst(2 DOWNTO 0) = "010"))) THEN -- jumps
                    
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triExtDataBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triIntDataBus <= '1';
                    sel_outArrAdr <= "001"; 
                    
                    en_triArrBus <= '1';
                    IF((inst(5 DOWNTO 3) = "000") AND (inst(0) = '1')) THEN
                        sel_inArrData <= ("1000"); -- dest high PC
                    ELSIF((inst(5 DOWNTO 3) = "011") AND (inst(0) = '0')) THEN -- on C
                        IF (flag(0)='1') THEN
                            sel_inArrData <= ("1000"); -- dest high PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "010") AND (inst(0) = '0')) THEN -- on no C
                        IF (flag(0)='0') THEN
                            sel_inArrData <= ("1000"); -- dest high PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "001") AND (inst(0) = '0')) THEN -- on Z
                        IF (flag(6)='1') THEN
                            sel_inArrData <= ("1000"); -- dest high PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "000") AND (inst(0) = '0')) THEN -- on no Z
                        IF (flag(6)='0') THEN
                            sel_inArrData <= ("1000"); -- dest high PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "110") AND (inst(0) = '0')) THEN -- on Pos
                        IF (flag(7)='0') THEN
                            sel_inArrData <= ("1000"); -- dest high PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "111") AND (inst(0) = '0')) THEN -- on Min
                        IF (flag(7)='1') THEN
                            sel_inArrData <= ("1000"); -- dest high PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "101") AND (inst(0) = '0')) THEN -- on Par Even
                        IF (flag(2)='1') THEN
                            sel_inArrData <= ("1000"); -- dest high PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    ELSIF((inst(5 DOWNTO 3) = "100") AND (inst(0) = '0')) THEN -- on Par Odd
                        IF (flag(2)='0') THEN
                            sel_inArrData <= ("1000"); -- dest high PC
                        ELSE
                            sel_inArrData <= ("1111");
                        END IF;
                    END IF;

                ELSIF (inst = "11001001") THEN -- RET
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triExtDataBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triIntDataBus <= '1';
                    sel_outArrAdr <= "010";
                    sel_inArrData <= ("1000");
                    inc_adr <= '1';
                    -- SP +=2
                    incSP <= '1'; -- +2

                ELSIF (inst = "11001101") THEN -- CALL
                    ALE <= '1';
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triAdrBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdr <= '1';
                    sel_outArrAdr <= "001";
                END IF;

            WHEN T10 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(5 DOWNTO 0) = "111010") OR (inst(5 DOWNTO 0) = "101010")) THEN -- LDA -- LHLD
                        ALE <= '1';
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "100";
                    ELSIF ((inst(5 DOWNTO 0) = "110010") OR (inst(5 DOWNTO 0) = "100010")) THEN -- STA --SHLD
                        ALE <= '1';
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "100";
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL ***********************************
                    RDbar <= '0';
                    S1 <= '1';
                    S0 <= '0';
                    incPC <= '1';
                    en_triExtDataBus <= '1';
                    sel_outArrAdr <= "001";
            END IF;

            WHEN T11 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF ((inst(5 DOWNTO 0) = "111010") OR (inst(5 DOWNTO 0) = "101010")) THEN -- LDA --LHLD
                        RDbar <= '0';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "100"; 
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN -- STA
                        WRbar <= '0';
                        S1 <= '0';
                        S0 <= '1';
                        en_triDataBuf <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdrData <= '1';
                        sel_outArrAdr <= "100";
                        en_triArrBus <= '0';
                        en_triAccBus <= '1';
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN --SHLD
                        WRbar <= '0';
                        S1 <= '0';
                        S0 <= '1';
                        en_triDataBuf <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdrData <= '1';
                        sel_outArrAdr <= "100";
                        en_triArrBus <= '1';
                        sel_outArrData <= ("0101"); -- source L
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL ***********************************
                    RDbar <= '1';
                    S1 <= '1';
                    S0 <= '0';
                    en_triExtDataBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triIntDataBus <= '1';
                    sel_outArrAdr <= "001"; 
                    sel_inArrData <= ("1010"); -- dest W temp 
                    decSP <= '1';
                END IF;

            WHEN T12 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "111010") THEN -- LDA
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        ld_acc <= '1';
                        sel_inArrData <= "1111";
                        sel_outArrAdr <= "100"; 
                    ELSIF (inst(5 DOWNTO 0) = "101010") THEN -- LHLD
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        sel_outArrAdr <= "100";
                        sel_inArrData <= "0101"; -- L
                    ELSIF (inst(5 DOWNTO 0) = "110010") THEN -- STA
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "100";
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN --SHLD
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "100";
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL ***********************************
                    ALE <= '1';
                    WRbar <= '1';
                    S1 <= '0';
                    S0 <= '1';
                    en_triAdrBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdr <= '1';
                    sel_outArrAdr <= "010"; -- SP adr
                    
                END IF;

            WHEN T13 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "101010") THEN -- LHLD
                        ALE <= '1';
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "100";
                        inc_adr <= '1';
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN --SHLD
                        ALE <= '1';
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triAdrBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdr <= '1';
                        sel_outArrAdr <= "100";
                        inc_adr <= '1';
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL ***********************************
                    WRbar <= '0';
                    S1 <= '0';
                    S0 <= '1';
                    en_triDataBuf <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdrData <= '1';
                    sel_outArrAdr <= "010";
                    sel_outArrData <= ("1000"); -- source H PC
                    en_triArrBus <= '1';
                END IF;

            WHEN T14 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "101010") THEN -- LHLD
                        RDbar <= '0';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "100";
                        inc_adr <= '1';
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN --SHLD
                        WRbar <= '0';
                        S1 <= '0';
                        S0 <= '1';
                        en_triDataBuf <= '1';
                        en_triExtDataBus <= '1';
                        en_bufAdrData <= '1';
                        sel_outArrAdr <= "100";
                        en_triArrBus <= '1';
                        sel_outArrData <= ("0100"); -- source H
                        inc_adr <= '1';
                    END IF;
                ELSIF (inst = "11001101") THEN -- CALL ***********************************
                    WRbar <= '1';
                    S1 <= '0';
                    S0 <= '1';
                    en_triExtDataBus <= '1';
                    sel_outArrAdr <= "010";
                    decSP <= '1';
                    -------------*******  negz
                    sel_outArrData <= ("1010"); -- source W  
                    sel_inArrData <= ("1000"); -- dest HPC
                    en_triArrBus <= '1';
                    
                    -----------**********  
                END IF;

            WHEN T15 =>
                IF (inst(7 DOWNTO 6) = "00" ) THEN
                    IF (inst(5 DOWNTO 0) = "101010") THEN -- LHLD
                        RDbar <= '1';
                        S1 <= '1';
                        S0 <= '0';
                        en_triExtDataBuf <= '1';
                        en_bufAdrData <= '1';
                        en_triIntDataBus <= '1';
                        sel_outArrAdr <= "100";
                        inc_adr <= '1';
                        sel_inArrData <= "0100"; -- H
                    ELSIF (inst(5 DOWNTO 0) = "100010") THEN --SHLD
                        WRbar <= '1';
                        S1 <= '0';
                        S0 <= '1';
                        en_triExtDataBus <= '1';
                        sel_outArrAdr <= "100";
                    END IF;

                ELSIF (inst = "11001101") THEN -- CALL ***********************************
                    ALE <= '1';
                    WRbar <= '1';
                    S1 <= '0';
                    S0 <= '1';
                    en_triAdrBuf <= '1';
                    en_bufAdrData <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdr <= '1';
                    sel_outArrAdr <= "010"; -- SP adr
                    
                END IF;

            WHEN T16 =>
                IF (inst = "11001101") THEN -- CALL ***********************************
                    WRbar <= '0';
                    S1 <= '0';
                    S0 <= '1';
                    en_triDataBuf <= '1';
                    en_triExtDataBus <= '1';
                    en_bufAdrData <= '1';
                    sel_outArrAdr <= "010";
                    sel_outArrData <= ("1001"); -- source L PC
                    en_triArrBus <= '1';

                END IF;

            WHEN T17 =>
                IF (inst = "11001101") THEN -- CALL ***********************************
                    WRbar <= '1';
                    S1 <= '0';
                    S0 <= '1';
                    en_triExtDataBus <= '1';
                    sel_outArrAdr <= "010";
                    -------------*******  negz
                    sel_outArrData <= ("1011"); -- source Z 1011 1010
                    sel_inArrData <= ("1001"); -- dest LPC 1001 1000
                    en_triArrBus <= '1';
                    -----------**********
                END IF;


            
           
            WHEN OTHERS =>
                en_triAccBus     <= '0';
                en_triFlgBus     <= '0';
                en_triAluBus     <= '0';
                en_triArrBus     <= '0';
                en_bufAdr        <= '0';
                en_bufAdrData    <= '0';
                en_triDataBuf    <= '0';
                en_triAdrBuf     <= '0';
                en_triExtDataBuf <= '0';
                en_triExtDataBus <= '0';
                en_triIntDataBus <= '0';
                ld_acc           <= '0'    ; 
                ld_tmp           <= '0'    ; 
                ld_flg           <= "00000"; 
                ld_inst          <= '0'    ; 
                sel_operation    <= "11111" ; 
                sel_inArrData    <= "1111" ; 
                sel_outArrData   <= "1111" ; 
                sel_outArrAdr    <= "000"  ; 
                decSP            <= '0';
                incSP            <= '0';
                incPC            <= '0';
                RDbar            <= '1';  
                WRbar            <= '1';  
                ALE              <= '0';    
                S0               <= '1';      
                S1               <= '1';      
                IO_Mbar          <= '0'; 
                INTAbar          <= '1';
                xchg             <= '0';
                inc_adr          <= '0';
                ld_invisibleCY   <= '0';
                sel_invisibleCY  <= '0';
                
        END CASE;
    END PROCESS;



    
END behavioural;



