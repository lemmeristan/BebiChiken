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

    SIGNAL update_pc, update_pc_branch, update_pc_branch_r                                  : STD_LOGIC;
    SIGNAL branch_next_pc                                                                   : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL writeback_rs1, writeback_rs2, writeback_data, writeback_token, writeback_next_pc : opcode_group_word_t;
    SIGNAL eu_we, eu_we_r, eu_busy, writeback_we                                            : opcode_group_bit_t;
    SIGNAL allready                                                                         : STD_LOGIC;

    SIGNAL writeback_rd, rd_out : opcode_group_regidx_t;

    SIGNAL update_rd : STD_LOGIC;

    SIGNAL regfile_rd                                                                                                                                                                       : STD_LOGIC_VECTOR(4 DOWNTO 0);
    SIGNAL rd_data_in                                                                                                                                                                       : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL writeback_update_pc                                                                                                                                                      : opcode_group_bit_t;
    SIGNAL rs1_data, rs1_data_r, rs2_data, rs2_data_r, next_pc, regfile_pc, regfile_pc_r, regfile_pc_r_r, inst_rdata_r, inst_rdata_r_r, rs1_data_out, rs2_data_out, token_r, token, n_token : STD_LOGIC_VECTOR(31 DOWNTO 0); -- n_pc
    --SIGNAL rs1_data_in, rs2_data_in, instruction_in, pc_in                                                                                                         : opcode_group_word_t;

    --SIGNAL pc_locked : STD_LOGIC;

    --SIGNAL inst_rdy_r, update_pc_r : STD_LOGIC;

    ALIAS funct7 : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata(31 DOWNTO 25);
    ALIAS rs2    : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata(24 DOWNTO 20);
    ALIAS rs1    : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata(19 DOWNTO 15);
    ALIAS funct3 : STD_LOGIC_VECTOR(2 DOWNTO 0) IS inst_rdata(14 DOWNTO 12);
    ALIAS rd     : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata(11 DOWNTO 7);
    ALIAS opcode : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata(6 DOWNTO 0);


    SIGNAL initialized : STD_LOGIC_VECTOR(7 DOWNTO 0);

    ALIAS funct7_r : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata_r(31 DOWNTO 25);
    ALIAS rs2_r    : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata_r(24 DOWNTO 20);
    ALIAS rs1_r    : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata_r(19 DOWNTO 15);
    ALIAS funct3_r : STD_LOGIC_VECTOR(2 DOWNTO 0) IS inst_rdata_r(14 DOWNTO 12);
    ALIAS rd_r     : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata_r(11 DOWNTO 7);
    ALIAS opcode_r : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata_r(6 DOWNTO 0);

    -- dispatcher
    SIGNAL dispatcher_busy, issue, issue_r, issue_r_r               : STD_LOGIC;

    SIGNAL decoded, decoded_r, new_rd_lock_owner, new_pc_lock_owner : opcode_group_t;

    SIGNAL imm_decoded : STD_LOGIC_VECTOR(31 DOWNTO 0);

    SIGNAL lock_rd, lock_pc, rs1_locked, rs2_locked, pc_locked, pc_locked_r : STD_LOGIC;

BEGIN

    decoded   <= f_decode_exec_unit(inst_rdata);
    decoded_r <= f_decode_exec_unit(inst_rdata_r);
    -- Fetcher statemachine:
    -- Fetches instruction and issues it to dispatcher

    PROCESS (fetcher_state, token, regfile_pc, dispatcher_busy, inst_rdata, pc_locked, initialized, inst_rdy, pc_locked_r)
    BEGIN
        n_fetcher_state <= fetcher_state;
        writeback_next_pc(OPCODE_INVALID)         <= regfile_pc;
        writeback_update_pc(OPCODE_INVALID)       <= '0';
        --issue <= '0'; -- when uncommented, it gives a bad result ... ?????????
        n_token <= token;
        CASE fetcher_state IS
            WHEN FETCHER_STATE_S0 =>
                --issue <= '0';
                IF (inst_rdy = '1') AND (dispatcher_busy = '0') AND (initialized = X"FF") AND (pc_locked = '0') AND (pc_locked_r = '0') THEN
                    issue   <= '1';
                    writeback_next_pc(OPCODE_INVALID) <= regfile_pc + X"00000004";
                    n_token <= token + X"00000001";

                    --IF f_updates_pc(inst_rdata) = '1' THEN
                    --    n_fetcher_state <= FETCHER_STATE_S1;
                    --ELSE
                        writeback_update_pc(OPCODE_INVALID) <= '1';
                    --END IF;
                END IF;
            WHEN OTHERS =>
                n_fetcher_state <= FETCHER_STATE_S0;
        END CASE;
    END PROCESS;
    -- Dispatcher statemachine:
    -- Registers data in flight to execution units, handles owners of registers

    eu_busy(OPCODE_U_TYPE) <= '0';

    writeback_token(OPCODE_INVALID) <= token_r;

    PROCESS (dispatcher_state, inst_rdata_r, rs1_locked, token_r, rs2_locked, eu_busy, rs1_r, rs2_r, issue, rd_r, regfile_pc_r, imm_decoded, writeback_rd)
    BEGIN
        n_dispatcher_state <= dispatcher_state;
        dispatcher_busy    <= '1';
        eu_we              <= (OTHERS => '0');

        rd_data_in                   <= (OTHERS => '0'); --writeback_rd(f_decode_exec_unit(inst_rdata_r));
        writeback_rd(OPCODE_INVALID) <= (OTHERS => '0');
        writeback_we(OPCODE_INVALID) <= '0';
        --lock_pc <= '0';
        lock_rd <= '0';
        new_pc_lock_owner <= f_decode_exec_unit(inst_rdata_r);
        writeback_token(OPCODE_INVALID) <= token_r;
        lock_pc <= '0';

        CASE dispatcher_state IS
            WHEN DISPATCHER_STATE_S0 =>
                -- if everything is running smoothly, eu_we = issue, otherwise go to S1
                IF ((f_uses_rs1(inst_rdata_r) = '1') AND (rs1_locked = '1'))
                    OR ((f_uses_rs2(inst_rdata_r) = '1') AND (rs2_locked = '1'))
                    OR (eu_busy(f_decode_exec_unit(inst_rdata_r)) = '1') THEN
                    --n_dispatcher_state <= DISPATCHER_STATE_S1;

                ELSE
                    dispatcher_busy <= '0';

                    CASE f_decode_opcode(inst_rdata_r) IS
                        WHEN OPCODE_U_TYPE_LUI =>
                            writeback_data(OPCODE_INVALID)                   <= imm_decoded;
                            writeback_rd(OPCODE_INVALID) <= rd_r;
                            writeback_we(OPCODE_INVALID) <= '1';
                            writeback_token(OPCODE_INVALID) <= token_r;
                            new_pc_lock_owner <= OPCODE_INVALID;
                            lock_rd <= '0';
                        WHEN OPCODE_U_TYPE_AUIPC =>
                            writeback_data(OPCODE_INVALID)                   <= regfile_pc_r + imm_decoded;
                            writeback_rd(OPCODE_INVALID) <= rd_r;
                            writeback_we(OPCODE_INVALID) <= '1';
                            writeback_token(OPCODE_INVALID) <= token_r;
                            new_pc_lock_owner <= OPCODE_INVALID;
                            lock_rd <= '0';
                        WHEN OTHERS =>
                            IF (f_decode_exec_unit(inst_rdata_r) = OPCODE_I_TYPE) AND (f_uses_rs1(inst_rdata_r) = '0') THEN
                                writeback_data(OPCODE_INVALID)                   <= f_calculate_i_type_zero(inst_rdata_r);
                                writeback_rd(OPCODE_INVALID) <= rd_r;
                                writeback_we(OPCODE_INVALID) <= '1';
                                writeback_token(OPCODE_INVALID) <= token_r;
                                new_pc_lock_owner <= OPCODE_INVALID;
                                lock_rd <= '0';

                            ELSE
                                lock_rd <= f_updates_rd(inst_rdata_r);
                                new_pc_lock_owner <= f_decode_exec_unit(inst_rdata_r);
                                lock_pc <= f_updates_pc(inst_rdata_r);

                                eu_we(f_decode_exec_unit(inst_rdata_r)) <= issue;
                            END IF;
                    END CASE;

                END IF;
            WHEN OTHERS =>
                n_dispatcher_state <= DISPATCHER_STATE_S0;
        END CASE;
    END PROCESS;


    inst_addr  <= regfile_pc;
    inst_re    <= '1';
    inst_width <= "10"; -- unused

    rd_out(OPCODE_INVALID)  <= "00000";
    eu_busy(OPCODE_INVALID) <= '0';
    eu_busy(OPCODE_U_TYPE) <= '0';
    

    --writeback_rd(OPCODE_INVALID) <= X"DEADBEEF";
    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            regfile_pc_r     <= entry_point - X"00000004";
            inst_rdata_r     <= (OTHERS => '0');
            initialized      <= (OTHERS => '0');
            fetcher_state    <= FETCHER_STATE_S0;
            dispatcher_state <= DISPATCHER_STATE_S0;
            regfile_pc_r_r   <= (OTHERS => '0');
            inst_rdata_r_r   <= (OTHERS => '0');
            eu_we_r          <= (OTHERS => '0');
            rs1_data_r       <= (OTHERS => '0');
            rs2_data_r       <= (OTHERS => '0');
            issue_r          <= '0';
            imm_decoded      <= (OTHERS => '0');
            token <= (others => '0');
            pc_locked_r <= '1';
        ELSIF rising_edge(clk) THEN
        pc_locked_r <= pc_locked;
            imm_decoded      <= f_decode_imm(inst_rdata);
            issue_r          <= issue;
            fetcher_state    <= n_fetcher_state;
            dispatcher_state <= n_dispatcher_state;

            regfile_pc_r_r <= regfile_pc_r;
            inst_rdata_r_r <= inst_rdata_r;
            eu_we_r        <= eu_we;
            rs1_data_r     <= rs1_data;
            rs2_data_r     <= rs2_data;

            initialized <= initialized(6 DOWNTO 0) & inst_rdy;
            token <= n_token;


            IF (issue = '1') THEN
            token_r <= token;
            regfile_pc_r <= regfile_pc;
            inst_rdata_r <= inst_rdata;
            END IF;
        END IF;
    END PROCESS;


    i_regfile_reduced : regfile_reduced
    GENERIC MAP(entry_point => entry_point)
    PORT MAP(

        clk => clk, rst => rst,
        rs1 => rs1_r, rs2 => rs2_r, rd => rd_r,

        lock_rd => lock_rd, lock_pc => lock_pc,
        new_rd_lock_owner => new_rd_lock_owner, new_pc_lock_owner => new_pc_lock_owner,
        lock_token => token_r,

        writeback_we => writeback_we, writeback_update_pc => writeback_update_pc,
        writeback_data => writeback_data,
        writeback_next_pc => writeback_next_pc, writeback_token => writeback_token,
        writeback_rd   => writeback_rd,

        rs1_data_out => rs1_data_out, rs2_data_out => rs2_data_out, pc => regfile_pc,
        rs1_locked => rs1_locked, rs2_locked => rs2_locked, pc_locked => pc_locked
    );


    writeback_rs1(OPCODE_INVALID) <= rs1_data_out;
    writeback_rs2(OPCODE_INVALID) <= rs2_data_out;

    i_eu_mem : eu_mem
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we(OPCODE_MEM_TYPE),
        rs1_data => rs1_data_out, rs2_data => rs2_data_out, instruction => inst_rdata_r, token => token_r, imm => imm_decoded,

        writeback_we    => writeback_we(OPCODE_MEM_TYPE),
        writeback_data  => writeback_data(OPCODE_MEM_TYPE),
        writeback_token => writeback_token(OPCODE_MEM_TYPE),
        writeback_rd    => writeback_rd(OPCODE_MEM_TYPE),
        mem_wack => data_wack, mem_rdy => data_rdy, mem_rdata => data_rdata, mem_wdata => data_wdata, mem_addr => data_addr, mem_width => data_width,
        mem_re => data_re, mem_we => data_we,
        busy            => eu_busy(OPCODE_MEM_TYPE)
    );
    i_eu_branch : eu_branch_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we(OPCODE_BRANCH_TYPE),
        rs1_data => rs1_data_out, rs2_data => rs2_data_out, instruction => inst_rdata_r, pc => regfile_pc_r, token => token_r, imm => imm_decoded,

        writeback_we      => writeback_we(OPCODE_BRANCH_TYPE),
        writeback_next_pc => writeback_next_pc(OPCODE_BRANCH_TYPE),
        writeback_data    => writeback_data(OPCODE_BRANCH_TYPE),
        writeback_token   => writeback_token(OPCODE_BRANCH_TYPE),
        writeback_rd      => writeback_rd(OPCODE_BRANCH_TYPE),
        writeback_update_pc => writeback_update_pc(OPCODE_BRANCH_TYPE),
        busy              => eu_busy(OPCODE_BRANCH_TYPE)
    );

    i_eu_i : eu_i_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we(OPCODE_I_TYPE),
        rs1_data => rs1_data_out, instruction => inst_rdata_r, token => token_r, imm => imm_decoded,

        writeback_we    => writeback_we(OPCODE_I_TYPE),
        writeback_data  => writeback_data(OPCODE_I_TYPE),
        writeback_token => writeback_token(OPCODE_I_TYPE),
        writeback_rd    => writeback_rd(OPCODE_I_TYPE),
        busy            => eu_busy(OPCODE_I_TYPE)

    );

    i_eu_r : eu_r_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we(OPCODE_R_TYPE),
        rs1_data => rs1_data_out, rs2_data => rs2_data_out, instruction => inst_rdata_r, token => token_r,

        writeback_we    => writeback_we(OPCODE_R_TYPE),
        writeback_data  => writeback_data(OPCODE_R_TYPE),
        writeback_token => writeback_token(OPCODE_R_TYPE),

        writeback_rd => writeback_rd(OPCODE_R_TYPE),
        busy         => eu_busy(OPCODE_R_TYPE)
    );

END behavioural;