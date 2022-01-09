LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY cpu IS
    GENERIC (entry_point : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00200000");

    PORT (
        rst, clk : IN STD_LOGIC;

        -- Instruction memory bus
        inst_width : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
        inst_addr  : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        inst_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        inst_re    : OUT STD_LOGIC;
        inst_rdy   : IN STD_LOGIC;

        -- Data memory bus
        data_width            : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
        data_addr, data_wdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_rdata            : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_re, data_we      : OUT STD_LOGIC;
        data_rdy, data_wack   : IN STD_LOGIC

    );
END cpu;

ARCHITECTURE behavioural OF cpu IS

    TYPE owner_t IS ARRAY(NATURAL RANGE <>) OF opcode_group_t;
    SIGNAL owner, n_owner : owner_t(31 DOWNTO 0);

    TYPE fetcher_state_t IS (FETCHER_STATE_S0, FETCHER_STATE_S1);
    SIGNAL fetcher_state, n_fetcher_state : fetcher_state_t;

    TYPE dispatcher_state_t IS (DISPATCHER_STATE_S0, DISPATCHER_STATE_S1);
    SIGNAL dispatcher_state, n_dispatcher_state : dispatcher_state_t;

    -- funct7 <= instruction(31 DOWNTO 25);
    -- rs2 <= instruction(24 DOWNTO 20);
    -- rs1 <= instruction(19 DOWNTO 15);
    -- funct3 <= instruction(14 DOWNTO 12);
    -- rd <= instruction(11 DOWNTO 7);
    -- opcode <= instruction(6 DOWNTO 0);

    --SIGNAL instruction : STD_LOGIC_VECTOR(31 DOWNTO 0);

    --SIGNAL pc : STD_LOGIC_VECTOR(31 DOWNTO 0);
    --SIGNAL imm_i, imm_s, imm_b, imm_u, imm_j, imm_jalr : STD_LOGIC_VECTOR(31 DOWNTO 0);

    --ATTRIBUTE syn_encoding : STRING;
    --ATTRIBUTE syn_encoding OF state_t : TYPE IS "one-hot";

    --ATTRIBUTE syn_keep : BOOLEAN;

    SIGNAL update_pc, update_pc_branch, update_pc_branch_r : STD_LOGIC;
    SIGNAL branch_next_pc                                  : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL writeback_rd, writeback_rs1, writeback_rs2      : opcode_group_word_t;
    SIGNAL eu_we, eu_we_r, eu_busy                         : opcode_group_bit_t;
    SIGNAL allready                                        : STD_LOGIC;

    SIGNAL rd_out : opcode_group_regidx_t;

    SIGNAL updates_rd : STD_LOGIC;

    SIGNAL regfile_rd                                                                                                                                              : STD_LOGIC_VECTOR(4 DOWNTO 0);
    SIGNAL rd_data_in                                                                                                                                              : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL eu_rdy                                                                                                                                                  : opcode_group_bit_t;
    SIGNAL rs1_data, rs1_data_r, rs2_data, rs2_data_r, next_pc, regfile_pc, regfile_pc_r, regfile_pc_r_r, inst_rdata_r, inst_rdata_r_r, rs1_data_out, rs2_data_out : STD_LOGIC_VECTOR(31 DOWNTO 0); -- n_pc
    SIGNAL rs1_data_in, rs2_data_in, instruction_in, pc_in                                                                                                         : opcode_group_word_t;

    SIGNAL pc_locked : STD_LOGIC;

    SIGNAL inst_rdy_r, update_pc_r : STD_LOGIC;

    ALIAS funct7 : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata(31 DOWNTO 25);
    ALIAS rs2    : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata(24 DOWNTO 20);
    ALIAS rs1    : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata(19 DOWNTO 15);
    ALIAS funct3 : STD_LOGIC_VECTOR(2 DOWNTO 0) IS inst_rdata(14 DOWNTO 12);
    ALIAS rd     : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata(11 DOWNTO 7);
    ALIAS opcode : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata(6 DOWNTO 0);

    SIGNAL update_rd   : opcode_group_bit_t;
    SIGNAL initialized : STD_LOGIC_VECTOR(7 DOWNTO 0);

    ALIAS funct7_r : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata_r(31 DOWNTO 25);
    ALIAS rs2_r    : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata_r(24 DOWNTO 20);
    ALIAS rs1_r    : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata_r(19 DOWNTO 15);
    ALIAS funct3_r : STD_LOGIC_VECTOR(2 DOWNTO 0) IS inst_rdata_r(14 DOWNTO 12);
    ALIAS rd_r     : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata_r(11 DOWNTO 7);
    ALIAS opcode_r : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata_r(6 DOWNTO 0);

    SIGNAL inst_valid, dispatch, update_pc_main : STD_LOGIC;

    SIGNAL rs1_owner, rs2_owner : opcode_group_t;
    -- dispatcher
    SIGNAL dispatcher_busy, issue, issue_r, issue_r_r : STD_LOGIC;
    SIGNAL eu_needs_writeback                         : STD_LOGIC;

    SIGNAL token, token_r, token_r_r : STD_LOGIC_VECTOR(31 DOWNTO 0);

BEGIN

    -- Fetcher statemachine:
    -- Fetches instruction and issues it to dispatcher

    PROCESS (fetcher_state, regfile_pc, dispatcher_busy, inst_rdata, branch_next_pc, update_pc_branch, issue, initialized, inst_rdy)
    BEGIN
        n_fetcher_state <= fetcher_state;
        next_pc         <= regfile_pc;
        update_pc       <= '0';
        CASE fetcher_state IS
            WHEN FETCHER_STATE_S0 =>
                next_pc <= regfile_pc + X"00000004";
                IF (inst_rdy = '1') AND (dispatcher_busy = '0') AND (initialized = X"FF") THEN
                    issue     <= '1';

                    IF f_updates_pc(inst_rdata) = '1' THEN
                        n_fetcher_state <= FETCHER_STATE_S1;
                    ELSE
                        update_pc <= '1';
                    END IF;
                END IF;
            WHEN FETCHER_STATE_S1 =>
                next_pc   <= branch_next_pc;
                update_pc <= update_pc_branch;
                IF update_pc_branch = '1' THEN
                    n_fetcher_state <= FETCHER_STATE_S0;
                END IF;
        END CASE;
    END PROCESS;
    -- Dispatcher statemachine:
    -- Registers data in flight to execution units, handles owners of registers

    PROCESS (dispatcher_state, owner, inst_rdata_r, owner, eu_needs_writeback, eu_busy, rs1_r, rs2_r, issue, rd_r)
    BEGIN
        n_dispatcher_state <= dispatcher_state;
        n_owner            <= owner;
        dispatcher_busy    <= '0';
        eu_we              <= (OTHERS => '0');
        updates_rd         <= '0';
        CASE dispatcher_state IS
            WHEN DISPATCHER_STATE_S0 =>
                -- if everything is running smoothly, eu_we = issue, otherwise go to S1
                IF ((f_uses_rs1(inst_rdata_r) = '1') AND (eu_busy(owner(to_integer(unsigned(rs1_r)))) = '1'))
                    OR ((f_uses_rs2(inst_rdata_r) = '1') AND (eu_busy(owner(to_integer(unsigned(rs2_r)))) = '1'))
                    OR (eu_busy(f_decode_exec_unit(inst_rdata_r)) = '1') THEN
                    n_dispatcher_state <= DISPATCHER_STATE_S1;
                    dispatcher_busy <= '1';
                ELSE
                    eu_we(f_decode_exec_unit(inst_rdata_r)) <= issue;
                    IF eu_needs_writeback = '1' THEN
                        n_owner(to_integer(unsigned(rd_out(f_decode_exec_unit(inst_rdata_r))))) <= OPCODE_INVALID;
                        updates_rd                                                              <= update_rd(f_decode_exec_unit(inst_rdata_r));
                    END IF;

                    IF (f_updates_rd(inst_rdata_r) = '1') THEN
                        n_owner(to_integer(unsigned(rd_r))) <= f_decode_exec_unit(inst_rdata_r);
                    END IF;

                END IF;
            WHEN DISPATCHER_STATE_S1 =>
                -- busy state; return to S0 when all inputs are ready
                dispatcher_busy <= '1';
                IF ((f_uses_rs1(inst_rdata_r) = '0') OR ((f_uses_rs1(inst_rdata_r) = '1') AND (eu_busy(owner(to_integer(unsigned(rs1_r)))) = '0')))
                    AND ((f_uses_rs2(inst_rdata_r) = '0') OR ((f_uses_rs2(inst_rdata_r) = '1') AND (eu_busy(owner(to_integer(unsigned(rs2_r)))) = '0')))
                    AND (eu_busy(f_decode_exec_unit(inst_rdata_r)) = '0') THEN
                    n_dispatcher_state <= DISPATCHER_STATE_S0;
                END IF;
        END CASE;
    END PROCESS;
    eu_needs_writeback <= '1' WHEN (owner(to_integer(unsigned(rd_out(f_decode_exec_unit(inst_rdata_r))))) = f_decode_exec_unit(inst_rdata_r)) AND (update_rd(f_decode_exec_unit(inst_rdata_r)) = '1') ELSE
        '0';

    update_pc_main <= issue;

    inst_addr  <= regfile_pc;
    inst_re    <= '1';
    inst_width <= "10"; -- unused

    rd_out(OPCODE_INVALID)  <= "00000";
    eu_rdy(OPCODE_INVALID)  <= '1';
    eu_busy(OPCODE_INVALID) <= '0';
    regfile_rd              <= rd_out(f_decode_exec_unit(inst_rdata_r));

    rd_data_in                   <= writeback_rd(f_decode_exec_unit(inst_rdata_r));
    writeback_rd(OPCODE_INVALID) <= X"DEADBEEF";
    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            owner <= (OTHERS => OPCODE_INVALID);
            regfile_pc_r     <= entry_point;
            inst_rdata_r     <= (OTHERS => '0');
            initialized      <= (OTHERS => '0');
            fetcher_state    <= FETCHER_STATE_S0;
            dispatcher_state <= DISPATCHER_STATE_S0;
        ELSIF rising_edge(clk) THEN
            owner <= n_owner;
            fetcher_state    <= n_fetcher_state;
            dispatcher_state <= n_dispatcher_state;

            initialized <= initialized(6 DOWNTO 0) & inst_rdy;

            IF (issue = '1') THEN

                regfile_pc_r <= regfile_pc;
                inst_rdata_r <= inst_rdata;
            END IF;
        END IF;
    END PROCESS;

    i_regfile_half : regfile_half
    GENERIC MAP(entry_point => entry_point)
    PORT MAP(

        clk => clk, rst => rst,
        rs1 => rs1_r, rs2 => rs2_r, rd => regfile_rd,
        rs1_data_out => rs1_data_out, rs2_data_out => rs2_data_out, pc => regfile_pc,
        update_rd => updates_rd, update_pc => update_pc,
        rd_data_in => rd_data_in, next_pc => next_pc

    );
    rs1_data <= writeback_rs1(owner(to_integer(unsigned(rs1_r)))) WHEN rs1_r /= "00000" ELSE
        (OTHERS => '0');
    rs2_data <= writeback_rs2(owner(to_integer(unsigned(rs2_r)))) WHEN rs2_r /= "00000" ELSE
        (OTHERS => '0');

    writeback_rs1(OPCODE_INVALID) <= rs1_data_out;
    writeback_rs2(OPCODE_INVALID) <= rs2_data_out;

    i_eu_mem : eu_mem
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we(OPCODE_MEM_TYPE),
        rs1_data => rs1_data, rs2_data => rs2_data, instruction => inst_rdata_r,

        writeback_rd  => writeback_rd(OPCODE_MEM_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_MEM_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_MEM_TYPE),

        mem_wack => data_wack, mem_rdy => data_rdy, mem_rdata => data_rdata, mem_wdata => data_wdata, mem_addr => data_addr, mem_width => data_width,
        mem_re => data_re, mem_we => data_we,
        rd        => rd_out(OPCODE_MEM_TYPE),
        busy      => eu_busy(OPCODE_MEM_TYPE),
        update_rd => update_rd(OPCODE_MEM_TYPE)
    );
    i_eu_branch : eu_branch_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we(OPCODE_BRANCH_TYPE),
        rs1_data => rs1_data, rs2_data => rs2_data, instruction => inst_rdata_r, pc => regfile_pc_r,

        writeback_rd  => writeback_rd(OPCODE_BRANCH_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_BRANCH_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_BRANCH_TYPE),

        --update_pc => update_pc(OPCODE_BRANCH_TYPE),
        next_pc => branch_next_pc,

        rd        => rd_out(OPCODE_BRANCH_TYPE),
        busy      => eu_busy(OPCODE_BRANCH_TYPE),
        update_rd => update_rd(OPCODE_BRANCH_TYPE),
        update_pc => update_pc_branch
    );

    i_eu_i : eu_i_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we(OPCODE_I_TYPE),
        rs1_data => rs1_data, rs2_data => rs2_data, instruction => inst_rdata_r, pc => regfile_pc_r,

        writeback_rd  => writeback_rd(OPCODE_I_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_I_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_I_TYPE),

        rd        => rd_out(OPCODE_I_TYPE),
        busy      => eu_busy(OPCODE_I_TYPE),
        update_rd => update_rd(OPCODE_I_TYPE)
    );

    i_eu_r : eu_r_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we(OPCODE_R_TYPE),
        rs1_data => rs1_data, rs2_data => rs2_data, instruction => inst_rdata_r, pc => regfile_pc_r,

        writeback_rd  => writeback_rd(OPCODE_R_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_R_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_R_TYPE),

        rd        => rd_out(OPCODE_R_TYPE),
        busy      => eu_busy(OPCODE_R_TYPE),
        update_rd => update_rd(OPCODE_R_TYPE)
    );

    i_eu_u : eu_u_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we(OPCODE_U_TYPE),
        rs1_data => rs1_data, rs2_data => rs2_data, instruction => inst_rdata_r, pc => regfile_pc_r_r,

        writeback_rd  => writeback_rd(OPCODE_U_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_U_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_U_TYPE),

        rd        => rd_out(OPCODE_U_TYPE),
        busy      => eu_busy(OPCODE_U_TYPE),
        update_rd => update_rd(OPCODE_U_TYPE)
    );
END behavioural;