LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY cpu IS
    GENERIC (entry_point : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"80010000");

    PORT (
        rst, clk : IN STD_LOGIC;

        -- Instruction memory bus
        inst_width : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
        inst_addr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        inst_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        inst_re : OUT STD_LOGIC;
        inst_rdy : IN STD_LOGIC;

        -- Data memory bus
        data_width : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
        data_addr, data_wdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_re, data_we : OUT STD_LOGIC;
        data_rdy, data_wack : IN STD_LOGIC;

        -- Register file
        registerfile_rs1, registerfile_rs2, registerfile_rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
        registerfile_wdata_rd : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        registerfile_rdata_rs1, registerfile_rdata_rs2 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        registerfile_we : OUT STD_LOGIC;

        err : OUT STD_LOGIC
    );
END cpu;

ARCHITECTURE behavioural OF cpu IS

    TYPE instruction_details_t IS
    RECORD
    selected, decode_error, execution_done, use_rs1, use_rs2, use_rd, decrement_counter : STD_LOGIC;
    result, next_pc, imm : STD_LOGIC_VECTOR(31 DOWNTO 0);

    -- mmu interface
    data_width : STD_LOGIC_VECTOR(1 DOWNTO 0);
    data_addr, data_wdata : STD_LOGIC_VECTOR(31 DOWNTO 0);
    data_re, data_we : STD_LOGIC;
END RECORD;
CONSTANT init_instruction_details : instruction_details_t := (selected => '0', imm => (OTHERS => '0'), execution_done => '0', decode_error => '1', result => (OTHERS => '0'), next_pc => (OTHERS => '0'), use_rs1 => '0', use_rs2 => '0', use_rd => '0', decrement_counter => '0',
data_width => "10", data_addr => (OTHERS => '0'), data_wdata => (OTHERS => '0'), data_re => '0', data_we => '0');
TYPE instruction_details_array_t IS ARRAY (NATURAL RANGE <>) OF instruction_details_t;
SIGNAL instruction_details_array : instruction_details_array_t(127 DOWNTO 0) := (OTHERS => init_instruction_details);
SIGNAL opcode, funct7 : STD_LOGIC_VECTOR(6 DOWNTO 0);
SIGNAL instruction, n_instruction : STD_LOGIC_VECTOR(31 DOWNTO 0);

SIGNAL pc, n_pc : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL rs1, rs2, rd : STD_LOGIC_VECTOR(4 DOWNTO 0);
SIGNAL funct3 : STD_LOGIC_VECTOR(2 DOWNTO 0);
SIGNAL imm_i, imm_s, imm_b, imm_u, imm_j, imm_jalr : STD_LOGIC_VECTOR(31 DOWNTO 0);

CONSTANT R_TYPE : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0110011"; -- Register/Register (ADD, ...)
CONSTANT I_TYPE : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0010011"; -- Register/Immediate (ADDI, ...)
CONSTANT I_TYPE_LOAD : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0000011";
CONSTANT S_TYPE : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0100011"; -- Store (SB, SH, SW)
CONSTANT B_TYPE : STD_LOGIC_VECTOR(6 DOWNTO 0) := "1100011"; -- Branch
CONSTANT U_TYPE_LUI : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0110111"; -- LUI
CONSTANT U_TYPE_AUIPC : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0010111"; -- AUIPC
CONSTANT J_TYPE_JAL : STD_LOGIC_VECTOR(6 DOWNTO 0) := "1101111"; -- JAL
CONSTANT J_TYPE_JALR : STD_LOGIC_VECTOR(6 DOWNTO 0) := "1100111"; -- JALR
TYPE state_t IS (FETCH_INSTRUCTION, WAIT_UNTIL_RD_UNLOCKED, FETCH_RS1, FETCH_RS2, EXECUTE, WRITEBACK, INCREMENT_PC, PANIC);
--ATTRIBUTE syn_encoding : STRING;
--ATTRIBUTE syn_encoding OF state_t : TYPE IS "one-hot";
SIGNAL state, n_state : state_t;

SIGNAL set_instruction : STD_LOGIC;

SIGNAL decode_error : STD_LOGIC_VECTOR(127 DOWNTO 0) := (OTHERS => '1');
SIGNAL use_rs1 : STD_LOGIC_VECTOR(127 DOWNTO 0) := (OTHERS => '0');
SIGNAL use_rs2 : STD_LOGIC_VECTOR(127 DOWNTO 0) := (OTHERS => '0');
SIGNAL use_rd : STD_LOGIC_VECTOR(127 DOWNTO 0) := (OTHERS => '0');
SIGNAL execution_done : STD_LOGIC_VECTOR(127 DOWNTO 0) := (OTHERS => '1');
SIGNAL dec_counter : STD_LOGIC_VECTOR(127 DOWNTO 0) := (OTHERS => '0');
SIGNAL dwe : STD_LOGIC_VECTOR(127 DOWNTO 0) := (OTHERS => '0'); -- data_we
SIGNAL selected : STD_LOGIC_VECTOR(127 DOWNTO 0) := (OTHERS => '0');
TYPE word_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL next_pc : word_t(127 DOWNTO 0) := (OTHERS => (OTHERS => '0'));
SIGNAL result : word_t(127 DOWNTO 0) := (OTHERS => (OTHERS => '0'));
SIGNAL imm : word_t(127 DOWNTO 0) := (OTHERS => (OTHERS => '0'));
SIGNAL wdata : word_t(127 DOWNTO 0) := (OTHERS => (OTHERS => '0'));
SIGNAL daddr : word_t(127 DOWNTO 0) := (OTHERS => (OTHERS => '0'));

SIGNAL state_out : STD_LOGIC_VECTOR(2 DOWNTO 0);

SIGNAL i_inst_addr, i_data_wdata, i_data_addr : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL i_data_we, i_data_re : STD_LOGIC;

-- component ila_0 PORT (
--   clk : in std_logic;
--   probe2, probe3, probe4, probe5, probe6, probe7, probe8 : in std_logic_vector(31 downto 0);
--   probe0, probe9, probe10 : in std_logic;
--   probe1 : in std_logic_vector(2 downto 0);
--   probe11, probe12 : in std_logic_vector(127 downto 0)
-- );
-- end component;
IMPURE FUNCTION DoShift (
    value : STD_LOGIC_VECTOR(31 DOWNTO 0);
    shamt : INTEGER RANGE 0 TO 31;
    arithmetic_shift : BOOLEAN;
    shleft : BOOLEAN
) RETURN STD_LOGIC_VECTOR IS
    VARIABLE ires : STD_LOGIC_VECTOR(31 DOWNTO 0);
    VARIABLE appendbit : STD_LOGIC;
BEGIN
    IF arithmetic_shift = true THEN
        appendbit := value(31);
    ELSE
        appendbit := '0';
    END IF;

    IF shamt > 31 THEN
        ires := (OTHERS => appendbit);
        RETURN ires;
    ELSIF shamt = 0 THEN
        RETURN value;
    END IF;

    IF shleft = true THEN
        ires := value;
        --        FOR I IN 1 TO shamt LOOP
        IF shamt > 0 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 1 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 2 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 3 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 4 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 5 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 6 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 7 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 8 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 9 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 10 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 10 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 11 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 12 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 13 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 14 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 15 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 16 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 17 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 18 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 19 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 20 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 21 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 22 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 23 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 24 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 25 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 26 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 27 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 28 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 29 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        IF shamt > 30 THEN
            ires := ires(30 DOWNTO 0) & '0';
        END IF;

        --        END LOOP;
    ELSE
        ires := value;

        IF shamt > 0 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 1 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 2 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 3 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 4 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 5 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 6 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 7 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 8 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 9 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 10 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 10 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 11 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 12 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 13 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 14 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 15 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 16 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 17 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 18 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 19 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 20 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 21 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 22 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 23 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 24 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 25 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 26 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 27 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 28 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 29 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;

        IF shamt > 30 THEN
            ires := appendbit & ires(31 DOWNTO 1);
        END IF;
        -- FOR I IN 1 TO shamt LOOP
        --     ires := appendbit & ires(31 DOWNTO 1);
        -- END LOOP;
    END IF;

    -- IF shleft = true THEN
    --     ires := (OTHERS => '0');
    --     ires(31 DOWNTO shamt) := value(31 - shamt DOWNTO 0);
    -- ELSE
    --     ires := (OTHERS => appendbit);
    --     ires(31 - shamt DOWNTO 0) := value(31 DOWNTO shamt);
    -- END IF;
    RETURN ires;
END FUNCTION;

IMPURE FUNCTION DoDivision (
    dividend, divisor : STD_LOGIC_VECTOR(31 DOWNTO 0)
) RETURN STD_LOGIC_VECTOR IS
    VARIABLE temp, quotient : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
BEGIN
    temp := dividend;
    WHILE temp /= X"00000000" LOOP
        IF temp > divisor THEN
            quotient := quotient + X"00000001";
            temp := temp - divisor;
        ELSE
            temp := X"00000000";
        END IF;
    END LOOP;
    RETURN quotient;
END FUNCTION;

BEGIN
i_inst_addr <= pc;
inst_addr <= i_inst_addr;
data_wdata <= i_data_wdata;
data_re <= i_data_re;
data_we <= i_data_we;
data_addr <= i_data_addr;

-- ila: ila_0 PORT MAP(
--   clk => clk,
--   probe0 => rst,
--   probe1 => state_out,
--   probe2 => pc,
--   probe3 => i_inst_addr,
--   probe4 => inst_rdata,
--   probe5 => instruction,
--   probe6 => data_rdata,
--   probe7 => i_data_wdata,
--   probe8 => i_data_addr,
--   probe9 => i_data_we,
--   probe10 => i_data_re,
--   probe11 => selected,
--   probe12 => execution_done
-- );

funct7 <= instruction(31 DOWNTO 25);
rs2 <= instruction(24 DOWNTO 20);
rs1 <= instruction(19 DOWNTO 15);
funct3 <= instruction(14 DOWNTO 12);
rd <= instruction(11 DOWNTO 7);
opcode <= instruction(6 DOWNTO 0);

registerfile_rs1 <= rs1;
registerfile_rs2 <= rs2;
registerfile_rd <= rd;
registerfile_wdata_rd <= result(to_integer(unsigned(opcode)));
inst_width <= "10";

fsm : PROCESS (state, instruction_details_array, pc, inst_rdy, opcode, decode_error, use_rd, execution_done, next_pc, result, daddr, dwe)
BEGIN
    n_state <= state;
    n_pc <= pc;
    registerfile_we <= '0';
    set_instruction <= '0';
    inst_re <= '0';
    err <= '0';
    selected <= (OTHERS => '0');
    data_width <= (OTHERS => '0');
    i_data_addr <= (OTHERS => '0');
    i_data_wdata <= (OTHERS => '0');
    i_data_re <= '0';
    i_data_we <= '0';

    CASE state IS
        WHEN FETCH_INSTRUCTION =>
            set_instruction <= '1';
            IF inst_rdy = '1' THEN
                inst_re <= '1';
                n_state <= EXECUTE;
            END IF;

        WHEN EXECUTE =>

            data_width <= instruction_details_array(to_integer(unsigned(opcode))).data_width;
            i_data_addr <= daddr(to_integer(unsigned(opcode)));
            i_data_wdata <= wdata(to_integer(unsigned(opcode)));
            i_data_re <= instruction_details_array(to_integer(unsigned(opcode))).data_re;
            i_data_we <= dwe(to_integer(unsigned(opcode)));

            selected(to_integer(unsigned(opcode))) <= '1';

            IF execution_done(to_integer(unsigned(opcode))) = '1' THEN
                --set_instruction <= '1';
                n_pc <= next_pc(to_integer(unsigned(opcode)));
                n_state <= FETCH_INSTRUCTION;
                IF use_rd(to_integer(unsigned(opcode))) = '1' THEN
                    registerfile_we <= '1';
                END IF;
            ELSIF decode_error(to_integer(unsigned(opcode))) = '1' THEN
                n_state <= PANIC;
            END IF;

        WHEN PANIC =>
            err <= '1';
        WHEN OTHERS =>
            n_state <= FETCH_INSTRUCTION;
    END CASE;
END PROCESS;

synchronous : PROCESS (rst, clk)
BEGIN
    IF rst = '1' THEN
        state <= FETCH_INSTRUCTION;
        instruction <= (OTHERS => '0');
        pc <= entry_point;
    ELSIF rising_edge(clk) THEN
        pc <= n_pc;
        IF set_instruction = '1' THEN
            instruction <= inst_rdata;
        END IF;

        state <= n_state;

    END IF;
END PROCESS;

decode_store : PROCESS (imm_s, pc, registerfile_rdata_rs1, registerfile_rdata_rs2, data_wack, funct3, selected(to_integer(unsigned(S_TYPE))))
BEGIN
    imm(to_integer(unsigned(S_TYPE))) <= imm_s;
    result(to_integer(unsigned(S_TYPE))) <= imm_s;
    use_rs1(to_integer(unsigned(S_TYPE))) <= '1';
    use_rs2(to_integer(unsigned(S_TYPE))) <= '1';
    next_pc(to_integer(unsigned(S_TYPE))) <= pc + X"00000004";
    execution_done(to_integer(unsigned(S_TYPE))) <= data_wack;
    decode_error(to_integer(unsigned(S_TYPE))) <= '0';

    daddr(to_integer(unsigned(S_TYPE))) <= registerfile_rdata_rs1 + imm_s;
    wdata(to_integer(unsigned(S_TYPE))) <= registerfile_rdata_rs2;
    dwe(to_integer(unsigned(S_TYPE))) <= selected(to_integer(unsigned(S_TYPE)));
    instruction_details_array(to_integer(unsigned(S_TYPE))).data_width <= funct3(1 DOWNTO 0);
END PROCESS;

decode_load : PROCESS (imm_i, pc, registerfile_rdata_rs1, data_rdy, data_rdata, funct3)
BEGIN
    imm(to_integer(unsigned(I_TYPE_LOAD))) <= imm_i;
    use_rs1(to_integer(unsigned(I_TYPE_LOAD))) <= '1';
    use_rd(to_integer(unsigned(I_TYPE_LOAD))) <= '1';

    next_pc(to_integer(unsigned(I_TYPE_LOAD))) <= pc + X"00000004";
    execution_done(to_integer(unsigned(I_TYPE_LOAD))) <= data_rdy;
    decode_error(to_integer(unsigned(I_TYPE_LOAD))) <= '0';

    daddr(to_integer(unsigned(I_TYPE_LOAD))) <= registerfile_rdata_rs1 + imm_i;
    instruction_details_array(to_integer(unsigned(I_TYPE_LOAD))).data_re <= '1';

    result(to_integer(unsigned(I_TYPE_LOAD))) <= data_rdata;

    instruction_details_array(to_integer(unsigned(I_TYPE_LOAD))).data_width <= funct3(1 DOWNTO 0);
    IF (funct3(2) = '0') THEN
        CASE funct3(1 DOWNTO 0) IS
            WHEN "00" =>
                result(to_integer(unsigned(I_TYPE_LOAD)))(31 DOWNTO 8) <= (OTHERS => data_rdata(7));

            WHEN "01" =>
                result(to_integer(unsigned(I_TYPE_LOAD)))(31 DOWNTO 16) <= (OTHERS => data_rdata(15));

            WHEN "11" =>
                decode_error(to_integer(unsigned(I_TYPE_LOAD))) <= '1';

            WHEN OTHERS =>

        END CASE;
    END IF;

END PROCESS;

execution_done(55) <= '1';
decode_lui : PROCESS (imm_u, pc)
BEGIN
    imm(to_integer(unsigned(U_TYPE_LUI))) <= imm_u;
    result(to_integer(unsigned(U_TYPE_LUI))) <= imm_u;
    use_rd(to_integer(unsigned(U_TYPE_LUI))) <= '1';
    next_pc(to_integer(unsigned(U_TYPE_LUI))) <= pc + X"00000004";
    --execution_done(to_integer(unsigned(U_TYPE_LUI))) <= '1';
    decode_error(to_integer(unsigned(U_TYPE_LUI))) <= '0';
END PROCESS;

execution_done(23) <= '1';
decode_auipc : PROCESS (imm_u, pc)
BEGIN
    imm(to_integer(unsigned(U_TYPE_AUIPC))) <= imm_u;
    use_rd(to_integer(unsigned(U_TYPE_AUIPC))) <= '1';
    result(to_integer(unsigned(U_TYPE_AUIPC))) <= pc + imm_u;
    next_pc(to_integer(unsigned(U_TYPE_AUIPC))) <= pc + X"00000004";
    --execution_done(to_integer(unsigned(U_TYPE_AUIPC))) <= '1';
    decode_error(to_integer(unsigned(U_TYPE_AUIPC))) <= '0';
END PROCESS;

execution_done(111) <= '1';
decode_jal : PROCESS (imm_j, pc)
BEGIN
    imm(to_integer(unsigned(J_TYPE_JAL))) <= imm_j;
    use_rd(to_integer(unsigned(J_TYPE_JAL))) <= '1';
    result(to_integer(unsigned(J_TYPE_JAL))) <= pc + X"00000004";
    next_pc(to_integer(unsigned(J_TYPE_JAL))) <= pc + imm_j;
    --execution_done(to_integer(unsigned(J_TYPE_JAL))) <= '1';
    decode_error(to_integer(unsigned(J_TYPE_JAL))) <= '0';
END PROCESS;

execution_done(103) <= '1';
decode_jalr : PROCESS (registerfile_rdata_rs1, pc, imm_jalr)
BEGIN
    imm(to_integer(unsigned(J_TYPE_JALR))) <= imm_jalr;
    use_rd(to_integer(unsigned(J_TYPE_JALR))) <= '1';
    use_rs1(to_integer(unsigned(J_TYPE_JALR))) <= '1';
    result(to_integer(unsigned(J_TYPE_JALR))) <= pc + X"00000004";
    next_pc(to_integer(unsigned(J_TYPE_JALR))) <= (imm_jalr + registerfile_rdata_rs1) AND X"FFFFFFFE"; --(pc + registerfile_rdata_rs1) and X"FFFFFFFE";
    --execution_done(to_integer(unsigned(J_TYPE_JALR))) <= '1';
    decode_error(to_integer(unsigned(J_TYPE_JALR))) <= '0';
END PROCESS;

execution_done(99) <= '1';
decode_b_type : PROCESS (funct3, registerfile_rdata_rs1, registerfile_rdata_rs2, imm_b, pc)
BEGIN
    imm(to_integer(unsigned(B_TYPE))) <= imm_b;

    result(to_integer(unsigned(B_TYPE))) <= (OTHERS => '0');
    use_rs1(to_integer(unsigned(B_TYPE))) <= '1';
    use_rs2(to_integer(unsigned(B_TYPE))) <= '1';
    next_pc(to_integer(unsigned(B_TYPE))) <= pc + X"00000004";

    decode_error(to_integer(unsigned(B_TYPE))) <= '0';
    --execution_done(to_integer(unsigned(B_TYPE))) <= '1';

    CASE funct3 IS
        WHEN "000" => -- BEQ
            IF signed(registerfile_rdata_rs1) = signed(registerfile_rdata_rs2) THEN
                next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
            END IF;
        WHEN "001" => -- BNE
            IF signed(registerfile_rdata_rs1) /= signed(registerfile_rdata_rs2) THEN
                next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
            END IF;
        WHEN "100" => -- BLT
            IF signed(registerfile_rdata_rs1) < signed(registerfile_rdata_rs2) THEN
                next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
            END IF;
        WHEN "101" => -- BGE
            IF signed(registerfile_rdata_rs1) >= signed(registerfile_rdata_rs2) THEN
                next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
            END IF;
        WHEN "110" => -- BLTU
            IF unsigned(registerfile_rdata_rs1) < unsigned(registerfile_rdata_rs2) THEN
                next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
            END IF;
        WHEN "111" => -- BGEU
            IF unsigned(registerfile_rdata_rs1) >= unsigned(registerfile_rdata_rs2) THEN
                next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
            END IF;
        WHEN OTHERS =>
            decode_error(to_integer(unsigned(B_TYPE))) <= '1';
    END CASE;
END PROCESS;
decode_r_type : PROCESS (funct3, funct7, registerfile_rdata_rs1, registerfile_rdata_rs2, pc) --clk, pc)
BEGIN
    use_rs1(to_integer(unsigned(R_TYPE))) <= '1';
    use_rs2(to_integer(unsigned(R_TYPE))) <= '1';
    use_rd(to_integer(unsigned(R_TYPE))) <= '1';
    next_pc(to_integer(unsigned(R_TYPE))) <= pc + X"00000004";
    decode_error(to_integer(unsigned(R_TYPE))) <= '0';
    result(to_integer(unsigned(R_TYPE))) <= (OTHERS => '0');
    imm(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs2;
    execution_done(to_integer(unsigned(R_TYPE))) <= '1';
    dec_counter(to_integer(unsigned(R_TYPE))) <= '0';

    CASE funct3 IS
        WHEN "000" =>
            CASE funct7 IS
                WHEN "0000000" => -- ADD
                    result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 + registerfile_rdata_rs2;

                WHEN "0100000" => -- SUB
                    result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 - registerfile_rdata_rs2;

                WHEN OTHERS =>
                    decode_error(to_integer(unsigned(R_TYPE))) <= '1';
            END CASE;

        WHEN "001" => -- SLL
            execution_done(to_integer(unsigned(R_TYPE))) <= '1';
            result(to_integer(unsigned(R_TYPE))) <= DoShift(registerfile_rdata_rs1, to_integer(unsigned(registerfile_rdata_rs2(4 DOWNTO 0))), false, true);

        WHEN "010" => -- SLT
            IF signed(registerfile_rdata_rs1) < signed(registerfile_rdata_rs2) THEN
                result(to_integer(unsigned(R_TYPE))) <= X"00000001";
            ELSE
                result(to_integer(unsigned(R_TYPE))) <= (OTHERS => '0');
            END IF;

        WHEN "011" => -- SLTU
            IF unsigned(registerfile_rdata_rs1) < unsigned(registerfile_rdata_rs2) THEN
                result(to_integer(unsigned(R_TYPE))) <= X"00000001";
            ELSE
                result(to_integer(unsigned(R_TYPE))) <= (OTHERS => '0');
            END IF;

        WHEN "100" => -- XOR
            result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 XOR registerfile_rdata_rs2;

        WHEN "101" =>
            execution_done(to_integer(unsigned(R_TYPE))) <= '0';

            CASE funct7 IS
                WHEN "0000000" => -- SRL

                    execution_done(to_integer(unsigned(R_TYPE))) <= '1';
                    result(to_integer(unsigned(R_TYPE))) <= DoShift(registerfile_rdata_rs1, to_integer(unsigned(registerfile_rdata_rs2(4 DOWNTO 0))), false, false);

                WHEN "0100000" => -- SRA
                    execution_done(to_integer(unsigned(R_TYPE))) <= '1';
                    result(to_integer(unsigned(R_TYPE))) <= DoShift(registerfile_rdata_rs1, to_integer(unsigned(registerfile_rdata_rs2(4 DOWNTO 0))), true, false);

                WHEN OTHERS =>
                    decode_error(to_integer(unsigned(R_TYPE))) <= '1';
            END CASE;

        WHEN "110" => -- OR
            result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 OR registerfile_rdata_rs2;

        WHEN "111" => -- AND
            result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 AND registerfile_rdata_rs2;
        WHEN OTHERS =>
            decode_error(to_integer(unsigned(R_TYPE))) <= '1';
    END CASE;
END PROCESS;

decode_i_type : PROCESS (imm_i, funct3, funct7, registerfile_rdata_rs1, pc) --clk, pc)
BEGIN
    imm(to_integer(unsigned(I_TYPE))) <= imm_i;

    decode_error(to_integer(unsigned(I_TYPE))) <= '0';
    execution_done(to_integer(unsigned(I_TYPE))) <= '1';
    next_pc(to_integer(unsigned(I_TYPE))) <= pc + X"00000004";

    use_rs1(to_integer(unsigned(I_TYPE))) <= '1';
    use_rs2(to_integer(unsigned(I_TYPE))) <= '1';
    use_rd(to_integer(unsigned(I_TYPE))) <= '1';

    result(to_integer(unsigned(I_TYPE))) <= (OTHERS => '0');

    dec_counter(to_integer(unsigned(I_TYPE))) <= '0';
    CASE funct3 IS
        WHEN "000" => -- ADDI
            result(to_integer(unsigned(I_TYPE))) <= registerfile_rdata_rs1 + imm_i;

        WHEN "001" =>
            CASE funct7 IS
                WHEN "0000000" => -- SLLI
                    execution_done(to_integer(unsigned(I_TYPE))) <= '1';
                    result(to_integer(unsigned(I_TYPE))) <= DoShift(registerfile_rdata_rs1, to_integer(unsigned(imm_i(4 DOWNTO 0))), false, true);

                WHEN OTHERS =>
                    decode_error(to_integer(unsigned(I_TYPE))) <= '1';
            END CASE;
        WHEN "010" => -- SLTI
            IF signed(registerfile_rdata_rs1) < signed(imm_i) THEN
                result(to_integer(unsigned(I_TYPE))) <= X"00000001";
            ELSE
                result(to_integer(unsigned(I_TYPE))) <= (OTHERS => '0');
            END IF;

        WHEN "011" => -- SLTIU
            IF unsigned(registerfile_rdata_rs1) < unsigned(imm_i) THEN
                result(to_integer(unsigned(I_TYPE))) <= X"00000001";
            ELSE
                result(to_integer(unsigned(I_TYPE))) <= (OTHERS => '0');
            END IF;

        WHEN "100" => -- XORI
            result(to_integer(unsigned(I_TYPE))) <= registerfile_rdata_rs1 XOR imm_i;

        WHEN "101" =>
            execution_done(to_integer(unsigned(I_TYPE))) <= '0';

            CASE funct7 IS
                WHEN "0000000" => -- SRLI
                    execution_done(to_integer(unsigned(I_TYPE))) <= '1';
                    result(to_integer(unsigned(I_TYPE))) <= DoShift(registerfile_rdata_rs1, to_integer(unsigned(imm_i(4 DOWNTO 0))), false, false);

                WHEN "0100000" => -- SRAI
                    execution_done(to_integer(unsigned(I_TYPE))) <= '1';
                    result(to_integer(unsigned(I_TYPE))) <= DoShift(registerfile_rdata_rs1, to_integer(unsigned(imm_i(4 DOWNTO 0))), true, false);

                WHEN OTHERS =>
                    decode_error(to_integer(unsigned(I_TYPE))) <= '1';
            END CASE;
        WHEN "110" => -- ORI
            result(to_integer(unsigned(I_TYPE))) <= registerfile_rdata_rs1 OR imm_i;

        WHEN "111" => -- ANDI
            result(to_integer(unsigned(I_TYPE))) <= registerfile_rdata_rs1 AND imm_i;

        WHEN OTHERS =>
            decode_error(to_integer(unsigned(I_TYPE))) <= '1';

    END CASE;
    --end if;
END PROCESS;

-- Immediate fields
decode_imm : PROCESS (instruction)
BEGIN
    -- I-type
    imm_i(31 DOWNTO 11) <= (OTHERS => instruction(31));
    imm_i(10 DOWNTO 5) <= instruction(30 DOWNTO 25);
    imm_i(4 DOWNTO 1) <= instruction(24 DOWNTO 21);
    imm_i(0) <= instruction(20);

    -- S-type
    imm_s(31 DOWNTO 11) <= (OTHERS => instruction(31));
    imm_s(10 DOWNTO 5) <= instruction(30 DOWNTO 25);
    imm_s(4 DOWNTO 1) <= instruction(11 DOWNTO 8);
    imm_s(0) <= instruction(7);

    -- B-type
    imm_b(31 DOWNTO 12) <= (OTHERS => instruction(31));
    imm_b(11) <= instruction(7);
    imm_b(10 DOWNTO 5) <= instruction(30 DOWNTO 25);
    imm_b(4 DOWNTO 1) <= instruction(11 DOWNTO 8);
    imm_b(0) <= '0';

    -- U-type
    imm_u(31) <= instruction(31);
    imm_u(30 DOWNTO 20) <= instruction(30 DOWNTO 20);
    imm_u(19 DOWNTO 12) <= instruction(19 DOWNTO 12);
    imm_u(11 DOWNTO 0) <= (OTHERS => '0');

    -- J-type
    imm_j(31 DOWNTO 20) <= (OTHERS => instruction(31));
    imm_j(19 DOWNTO 12) <= instruction(19 DOWNTO 12);
    imm_j(11) <= instruction(20);
    imm_j(10 DOWNTO 5) <= instruction(30 DOWNTO 25);
    imm_j(4 DOWNTO 1) <= instruction(24 DOWNTO 21);
    imm_j(0) <= '0';

    -- JALR
    imm_jalr <= (OTHERS => '0');
    imm_jalr(11 DOWNTO 0) <= instruction(31 DOWNTO 20);

END PROCESS;

--interrupt_error <= instruction_details_array(to_integer(unsigned(opcode))).decode_error;
--exec_done <= execution_done(to_integer(unsigned(opcode)));
END behavioural;