LIBRARY ieee; 
USE ieee.STD_LOGIC_1164.ALL; 
USE ieee.STD_LOGIC_unsigned.ALL ; 

ENTITY alu IS 
    GENERIC ( 
        n : INTEGER := 8
    ); 
    PORT(
        dina    : IN STD_LOGIC_VECTOR(n-1 DOWNTO 0); 
        dinb    : IN STD_LOGIC_VECTOR(n-1 DOWNTO 0); 
        dout    : OUT STD_LOGIC_VECTOR(n-1 DOWNTO 0); 
        co      : OUT STD_LOGIC; --CY
        acy     : OUT STD_LOGIC; --AC
        sgn     : OUT STD_LOGIC; --S
        parity  : OUT STD_LOGIC; --P
        zero    : OUT STD_LOGIC; --Z
        cin     : IN STD_LOGIC; 
        op_sel  : IN STD_LOGIC_VECTOR(4 DOWNTO 0)
    ); 
END alu; 
  
  

ARCHITECTURE behavioural OF alu IS 
    SIGNAL res    : STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL dina_s    : STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL dinb_s    : STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL dinb_s_inv : STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL dina_s_inv : STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL dinb_alu  : STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL dina_alu  : STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL sum_s     : STD_LOGIC_VECTOR(n-1 DOWNTO 0 ); 
    SIGNAL acy_s    : STD_LOGIC ; 
    SIGNAL co_s    : STD_LOGIC ; 
    SIGNAL cin_s    : STD_LOGIC ; 
BEGIN 
dina_s <= dina ; 
dinb_s <= dinb ; 
dinb_s_inv <= NOT(dinb) ; 
dina_s_inv <= NOT(dina) ; 

    adder_8b: ENTITY work.adder(behavioural)
        GENERIC MAP (
            n => 8
        )
        PORT MAP (
            dina => dina_alu,
            dinb => dinb_alu,
            sum  => sum_s,
            cin  => cin_s,
            acy  => acy_s,
            cout => co_s
        ); 

    PROCESS(dina, dinb, op_sel, dina_s, dinb_s, dinb_s_inv, dina_s_inv, sum_s, cin, co_s) 
        VARIABLE xor_res:STD_LOGIC_VECTOR(n-1 DOWNTO 0) ; 
        VARIABLE shr_res:STD_LOGIC_VECTOR(n-1 DOWNTO 0) ; 
        VARIABLE shl_res:STD_LOGIC_VECTOR(n-1 DOWNTO 0) ; 
        VARIABLE and_res:STD_LOGIC_VECTOR(n-1 DOWNTO 0); 
        VARIABLE or_res:STD_LOGIC_VECTOR(n-1 DOWNTO 0); 
        VARIABLE dout_var:STD_LOGIC_VECTOR(n-1 DOWNTO 0); 
        VARIABLE op_sel_int : INTEGER := 0 ; 
    BEGIN 
        op_sel_int := CONV_INTEGER(op_sel); 

        FOR i IN n-1 DOWNTO 0 LOOP 
            xor_res(i) := dina(i) XOR dinb(i); 
        END LOOP; 

        FOR i IN n-1-1 DOWNTO 0 LOOP 
            shr_res(i) := dina(i+1) ; 
        END LOOP; 
        shr_res(n-1) := dina(0) ; -- '0'

        FOR i IN n-1-1 DOWNTO 0 LOOP 
            shl_res(i+1) := dina(i) ; 
        END LOOP; 
        shl_res(0) := dina(n-1) ; -- '0'

        FOR i IN n-1 DOWNTO 0 LOOP 
            and_res(i) := dina(i) AND dinb(i); 
        END LOOP; 

        FOR i IN n-1 DOWNTO 0 LOOP 
            or_res(i) := dina(i) OR dinb(i); 
        END LOOP; 

        CASE op_sel_int IS 
            WHEN 1 => 
                cin_s <= '0';
                dout_var := xor_res ; 
                dinb_alu <= dinb_s ; 
                dina_alu <= dina_s ; 
                co <= '0' ; 
                acy <= '0' ;
            -- WHEN 2 => 
            --     dout_var := xnor_res ; 
            --     dinb_alu <= dinb_s ; 
            --     dina_alu <= dina_s ; 
            --     co <= '0' ; 
            WHEN 2 => 
                cin_s <= '0';
                cin_s <= '0';
                dout_var := and_res ; 
                dinb_alu <= dinb_s ; 
                dina_alu <= dina_s ; 
                co <= '0' ; 
                acy <= '0' ;
            WHEN 3 => 
                cin_s <= '0';
                dout_var := or_res ; 
                dinb_alu <= dinb_s ; 
                dina_alu <= dina_s ; 
                co <= '0' ; 
                acy <= '0' ;
            WHEN 4 => -- STC(cin) set
                cin_s <= '0';
                dout_var := sum_s ; 
                dinb_alu <= dinb_s ; 
                dina_alu <= dina_s ; 
                co <= '1' ; 
                acy <= acy_s ;
            WHEN 5 => -- CMC(cin) inv
                cin_s <= '0';
                dout_var := sum_s ; 
                dinb_alu <= dinb_s ; 
                dina_alu <= dina_s ; 
                co <= NOT cin ; 
                acy <= acy_s ;
            WHEN 6 => -- NOT NOT(A) 
                cin_s <= '0';
                dinb_alu <= (OTHERS => '0') ; 
                dout_var := sum_s ; 
                dina_alu <= dina_s_inv ; 
                co <= co_s ; 
                acy <= acy_s ;
            WHEN 7 =>  -- add A + B 
                cin_s <= '0';
                dinb_alu <= dinb_s ; 
                dout_var := sum_s ; 
                dina_alu <= dina_s ; 
                co <= co_s ; 
                acy <= acy_s ;
            WHEN 8 =>   -- sub A - B 
                cin_s <= '1';
                dinb_alu <= dinb_s_inv ; 
                dout_var := sum_s ; 
                dina_alu <= dina_s ; 
                co <= co_s;
                acy <= acy_s ; 
            WHEN 9 =>  -- INr REG + 1 
                cin_s <= '1';
                dinb_alu <= dinb_s ; 
                dout_var := sum_s ; 
                dina_alu <= (OTHERS => '0') ; 
                co <= co_s ; 
                acy <= acy_s ;
            WHEN 10 =>  -- dcr REG - 1 
                cin_s <= '0';
                dinb_alu <= dinb_s ;
                dout_var := sum_s ; 
                dina_alu <= (OTHERS => '1') ;
                co <= co_s ; 
                acy <= acy_s ;
            WHEN 11 =>  -- addc A + B + cin
                cin_s <= cin;
                dinb_alu <= dinb_s ; 
                dout_var := sum_s ; 
                dina_alu <= dina_s ; 
                co <= co_s ; 
                acy <= acy_s ;
            WHEN 12 =>   -- subc A - B - cin
                cin_s <= NOT(cin);
                dinb_alu <= (dinb_s_inv) ; 
                dout_var := sum_s ; 
                dina_alu <= dina_s; 
                co <= co_s; 
                acy <= acy_s ;
            WHEN 13 =>  -- shl A <- shl(A) RLC
                cin_s <= '0';
                dinb_alu <= dinb_s ; 
                dout_var := shl_res ; 
                dina_alu <= dina_s; 
                co <= shl_res(0) ; 
                acy <= acy_s ;
            -- WHEN 12 =>  -- zero/clear 
            --     cin_s <= '0';
            --     dinb_alu <= "00000000" ; 
            --     dout_var := sum_s ; 
            --     dina_alu <= "00000000"; 
            --     co <= co_s ; 
            WHEN 14 => -- SHR(A) RRC
                cin_s <= '0';
                dout_var := shr_res ; 
                dinb_alu <= dinb_s ; 
                dina_alu <= dina_s ; 
                co <= shr_res(n-1) ;
                acy <= acy_s ; 
            WHEN 15 => -- INC if Cin==1 
                cin_s <= cin;
                dinb_alu <= dinb_s ; 
                dout_var := sum_s ; 
                dina_alu <= (OTHERS => '0') ; 
                co <= co_s ; 
                acy <= acy_s ;
            WHEN 16 => -- DEC if Cin==0
                cin_s <= cin;
                dinb_alu <= dinb_s ;
                dout_var := sum_s ; 
                dina_alu <= (OTHERS => '1') ;
                co <= co_s ; 
                acy <= acy_s ;
            WHEN OTHERS => 
                cin_s <= '0';
                dout_var := sum_s ; 
                dinb_alu <= (OTHERS => '0') ; 
                dina_alu <= (OTHERS => '0') ; 
                co <= co_s ; 
                acy <= acy_s ;
        END CASE; 
        dout <= dout_var ; 
        parity <= (dout_var(0) XOR dout_var(1) XOR dout_var(2) XOR dout_var(3) XOR dout_var(4) XOR dout_var(5) XOR dout_var(6) XOR dout_var(7)) ; 
        res <= dout_var ; 
    END PROCESS; 

    zero <= NOT (res(0) OR res(1) OR res(2) OR res(3) OR res(4) OR res(5) OR res(6) OR res(7)) ; 
    sgn <= res(7) ; 

END behavioural; 
