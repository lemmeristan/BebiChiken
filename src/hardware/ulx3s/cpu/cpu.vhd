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
        inst_addr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        inst_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        inst_re : OUT STD_LOGIC;
        inst_rdy : IN STD_LOGIC;

        -- Data memory bus
        data_width : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
        data_addr, data_wdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_re, data_we : OUT STD_LOGIC;
        data_rdy, data_wack : IN STD_LOGIC

    );
END cpu;

ARCHITECTURE behavioural OF cpu IS

    TYPE owner_t IS ARRAY(NATURAL RANGE <>) OF opcode_group_t;
    SIGNAL owner : owner_t(32 DOWNTO 0);

    FUNCTION f_allready (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        inst_rdy : IN STD_LOGIC;
        owners : IN owner_t(32 DOWNTO 0);
        eu_rdy, eu_busy : IN opcode_group_bit_t)
        RETURN STD_LOGIC IS
        VARIABLE rs1, rs2 : INTEGER RANGE 0 TO 31;
    BEGIN

        rs2 := to_integer(unsigned(instruction(24 DOWNTO 20)));
        rs1 := to_integer(unsigned(instruction(19 DOWNTO 15)));

        IF (inst_rdy = '1') AND (owners(32) = OPCODE_INVALID)
            AND ((f_uses_rs1(instruction) = '0') OR ((f_uses_rs1(instruction) = '1') AND (eu_rdy(owners(rs1)) = '1')))
            AND ((f_uses_rs2(instruction) = '0') OR ((f_uses_rs2(instruction) = '1') AND (eu_rdy(owners(rs2)) = '1')))
            AND (eu_busy(f_decode_exec_unit(instruction)) = '0')
            THEN
            RETURN '1';
        END IF;

        RETURN '0';
    END;
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

    ATTRIBUTE syn_keep : BOOLEAN;
    SIGNAL update_pc, update_pc_branch : STD_LOGIC;
    SIGNAL branch_next_pc : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL writeback_rd, writeback_rs1, writeback_rs2 : opcode_group_word_t;
    SIGNAL eu_we, eu_we_r, eu_busy : opcode_group_bit_t;
    SIGNAL allready : STD_LOGIC;

    SIGNAL rd_out : opcode_group_regidx_t;

    SIGNAL updates_rd : STD_LOGIC;

    SIGNAL regfile_rd : STD_LOGIC_VECTOR(4 DOWNTO 0);
    SIGNAL rd_data_in : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL eu_rdy : opcode_group_bit_t;
    SIGNAL rs1_data, rs1_data_r, rs2_data, rs2_data_r, next_pc, regfile_pc, regfile_pc_r, regfile_pc_r_r, inst_rdata_r, rs1_data_out, rs2_data_out : STD_LOGIC_VECTOR(31 DOWNTO 0); -- n_pc
    SIGNAL rs1_data_in, rs2_data_in, instruction_in, pc_in : opcode_group_word_t;

    SIGNAL pc_locked : STD_LOGIC;

    SIGNAL inst_rdy_r, update_pc_r : STD_LOGIC;

    ALIAS funct7 : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata(31 DOWNTO 25);
    ALIAS rs2 : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata(24 DOWNTO 20);
    ALIAS rs1 : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata(19 DOWNTO 15);
    ALIAS funct3 : STD_LOGIC_VECTOR(2 DOWNTO 0) IS inst_rdata(14 DOWNTO 12);
    ALIAS rd : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata(11 DOWNTO 7);
    ALIAS opcode : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata(6 DOWNTO 0);

    SIGNAL update_rd : opcode_group_bit_t;
    SIGNAL initialized : STD_LOGIC_VECTOR(7 DOWNTO 0);

    ALIAS funct7_r : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata_r(31 DOWNTO 25);
    ALIAS rs2_r : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata_r(24 DOWNTO 20);
    ALIAS rs1_r : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata_r(19 DOWNTO 15);
    ALIAS funct3_r : STD_LOGIC_VECTOR(2 DOWNTO 0) IS inst_rdata_r(14 DOWNTO 12);
    ALIAS rd_r : STD_LOGIC_VECTOR(4 DOWNTO 0) IS inst_rdata_r(11 DOWNTO 7);
    ALIAS opcode_r : STD_LOGIC_VECTOR(6 DOWNTO 0) IS inst_rdata_r(6 DOWNTO 0);

    signal inst_valid, start_decode, start_decode_r, update_pc_main : std_logic;

BEGIN
    start_decode <= '1' WHEN (inst_rdy = '1')
        AND ((f_uses_rs1(inst_rdata) = '0') OR ((f_uses_rs1(inst_rdata) = '1') AND (eu_busy(owner(to_integer(unsigned(rs1)))) = '0')))
        AND ((f_uses_rs2(inst_rdata) = '0') OR ((f_uses_rs2(inst_rdata) = '1') AND (eu_busy(owner(to_integer(unsigned(rs2)))) = '0')))
        AND (eu_busy(f_decode_exec_unit(inst_rdata)) = '0')
        ELSE
        '0';

    update_pc_main <= not eu_busy(f_decode_exec_unit(inst_rdata_r)); --start_decode;

    inst_addr <= regfile_pc;
    inst_re <= '1';
    inst_width <= "10"; -- unused

    rd_out(OPCODE_INVALID) <= "00000";
    eu_rdy(OPCODE_INVALID) <= '1';
    eu_busy(OPCODE_INVALID) <= '0';
    regfile_rd <= rd_out(f_decode_exec_unit(inst_rdata)); -- when turned into inst_rdata_r, it stops working entirely, not even slow

    rd_data_in <= writeback_rd(f_decode_exec_unit(inst_rdata));
    --updates_rd                   <= update_rd(f_decode_exec_unit(inst_rdata)); --'1' WHEN f_decode_exec_unit(inst_rdata) /= OPCODE_INVALID AND eu_rdy(f_decode_exec_unit(inst_rdata)) = '1' AND owner(to_integer(unsigned(rd_out(f_decode_exec_unit(inst_rdata))))) = f_decode_exec_unit(inst_rdata) ELSE '0';
    writeback_rd(OPCODE_INVALID) <= X"DEADBEEF";

    PROCESS (inst_rdata, inst_rdata_r, update_pc_main, eu_rdy, branch_next_pc, owner, regfile_pc, initialized, update_pc_branch, start_decode_r, start_decode)
    BEGIN
        next_pc <= regfile_pc;
        update_pc <= '0';
        CASE owner(32) IS
            WHEN OPCODE_BRANCH_TYPE =>
                next_pc <= branch_next_pc;
                update_pc <= update_pc_branch;
            WHEN OPCODE_INVALID =>
                IF initialized = X"FF" THEN
                    IF f_updates_pc(inst_rdata_r) = '0' THEN
                        next_pc <= regfile_pc_r + X"00000004";
                        update_pc <= update_pc_main;
                    END IF;
                END IF;
            WHEN OTHERS =>
        END CASE;

        eu_we <= (OTHERS => '0');
        if start_decode = '1' then
        eu_we(f_decode_exec_unit(inst_rdata)) <= not eu_busy(f_decode_exec_unit(inst_rdata));
        end if;
    END PROCESS;

    pc_locked <= '0' WHEN owner(32) = OPCODE_INVALID ELSE
        '1';

    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            owner <= (OTHERS => OPCODE_INVALID);
            --eu_we          <= (OTHERS => '0');
            regfile_pc_r <= entry_point;
            regfile_pc_r_r <= entry_point;
            inst_rdata_r <= (OTHERS => '0');
            inst_rdy_r <= '0';
            update_pc_r <= '0';
            rs1_data_r <= (OTHERS => '0');
            rs2_data_r <= (OTHERS => '0');
            initialized <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            initialized <= initialized(6 DOWNTO 0) & inst_rdy;

            start_decode_r <= start_decode;
            if start_decode = '1' then
                rs1_data_r <= rs1_data;
                rs2_data_r <= rs2_data;
                update_pc_r <= update_pc;

                regfile_pc_r <= regfile_pc;

                inst_rdata_r <= inst_rdata;
                inst_rdy_r <= inst_rdy;
            end if;

            eu_we_r <= eu_we;


            IF eu_busy(f_decode_exec_unit(inst_rdata)) = '0' THEN
                IF (owner(to_integer(unsigned(rd_out(f_decode_exec_unit(inst_rdata))))) = f_decode_exec_unit(inst_rdata)) AND (update_rd(f_decode_exec_unit(inst_rdata)) = '1') THEN
                    owner(to_integer(unsigned(rd_out(f_decode_exec_unit(inst_rdata))))) <= OPCODE_INVALID;
                    updates_rd <= update_rd(f_decode_exec_unit(inst_rdata));
                END IF;

                IF (f_updates_rd(inst_rdata) = '1') THEN
                    owner(to_integer(unsigned(rd))) <= f_decode_exec_unit(inst_rdata);
                END IF;
            END IF;

            IF (owner(32) = OPCODE_INVALID) THEN
                IF (f_updates_pc(inst_rdata) = '1') THEN
                    owner(32) <= f_decode_exec_unit(inst_rdata);
                END IF;
            ELSIF update_pc_branch = '1' THEN
                owner(32) <= OPCODE_INVALID;
            END IF;
        END IF;
    END PROCESS;
    i_regfile_half : regfile_half
    GENERIC MAP(entry_point => entry_point)
    PORT MAP(

        clk => clk, rst => rst,
        rs1 => rs1, rs2 => rs2, rd => regfile_rd,
        rs1_data_out => rs1_data_out, rs2_data_out => rs2_data_out, pc => regfile_pc,
        update_rd => updates_rd, update_pc => update_pc,
        rd_data_in => rd_data_in, next_pc => next_pc

    );
    rs1_data <= writeback_rs1(owner(to_integer(unsigned(rs1)))) WHEN rs1 /= "00000" ELSE
        (OTHERS => '0'); -- have to do it like this or else ghdl won't synth :(
    rs2_data <= writeback_rs2(owner(to_integer(unsigned(rs2)))) WHEN rs2 /= "00000" ELSE
        (OTHERS => '0');

    writeback_rs1(OPCODE_INVALID) <= rs1_data_out;
    writeback_rs2(OPCODE_INVALID) <= rs2_data_out;

    i_eu_mem : eu_mem
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we_r(OPCODE_MEM_TYPE),
        rs1_data => rs1_data_r, rs2_data => rs2_data_r, instruction => inst_rdata_r,

        writeback_rd => writeback_rd(OPCODE_MEM_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_MEM_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_MEM_TYPE),

        mem_wack => data_wack, mem_rdy => data_rdy, mem_rdata => data_rdata, mem_wdata => data_wdata, mem_addr => data_addr, mem_width => data_width,
        mem_re => data_re, mem_we => data_we,
        rd => rd_out(OPCODE_MEM_TYPE),
        busy => eu_busy(OPCODE_MEM_TYPE),
        update_rd => update_rd(OPCODE_MEM_TYPE)
    );
    i_eu_branch : eu_branch_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we_r(OPCODE_BRANCH_TYPE),
        rs1_data => rs1_data_r, rs2_data => rs2_data_r, instruction => inst_rdata_r, pc => regfile_pc_r,

        writeback_rd => writeback_rd(OPCODE_BRANCH_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_BRANCH_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_BRANCH_TYPE),

        --update_pc => update_pc(OPCODE_BRANCH_TYPE),
        next_pc => branch_next_pc,

        rd => rd_out(OPCODE_BRANCH_TYPE),
        busy => eu_busy(OPCODE_BRANCH_TYPE),
        update_rd => update_rd(OPCODE_BRANCH_TYPE),
        update_pc => update_pc_branch
    );

    i_eu_i : eu_i_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we_r(OPCODE_I_TYPE),
        rs1_data => rs1_data_r, rs2_data => rs2_data_r, instruction => inst_rdata_r, pc => regfile_pc_r,

        writeback_rd => writeback_rd(OPCODE_I_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_I_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_I_TYPE),

        rd => rd_out(OPCODE_I_TYPE),
        busy => eu_busy(OPCODE_I_TYPE),
        update_rd => update_rd(OPCODE_I_TYPE)
    );

    i_eu_r : eu_r_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we_r(OPCODE_R_TYPE),
        rs1_data => rs1_data_r, rs2_data => rs2_data_r, instruction => inst_rdata_r, pc => regfile_pc_r,

        writeback_rd => writeback_rd(OPCODE_R_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_R_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_R_TYPE),

        rd => rd_out(OPCODE_R_TYPE),
        busy => eu_busy(OPCODE_R_TYPE),
        update_rd => update_rd(OPCODE_R_TYPE)
    );

    i_eu_u : eu_u_type
    PORT MAP(
        rst => rst, clk => clk,

        we => eu_we_r(OPCODE_U_TYPE),
        rs1_data => rs1_data_r, rs2_data => rs2_data_r, instruction => inst_rdata_r, pc => regfile_pc_r,

        writeback_rd => writeback_rd(OPCODE_U_TYPE),
        writeback_rs1 => writeback_rs1(OPCODE_U_TYPE),
        writeback_rs2 => writeback_rs2(OPCODE_U_TYPE),

        rd => rd_out(OPCODE_U_TYPE),
        busy => eu_busy(OPCODE_U_TYPE),
        update_rd => update_rd(OPCODE_U_TYPE)
    );
END behavioural;