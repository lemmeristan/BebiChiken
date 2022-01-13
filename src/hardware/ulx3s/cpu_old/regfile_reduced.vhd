LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

LIBRARY work;
USE work.bebichiken.ALL;

ENTITY regfile_reduced IS
    GENERIC (
        entry_point : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00000000"
    );
    PORT (
        rst, clk                             : IN STD_LOGIC;
        rs1, rs2, rd                         : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        lock_rd, lock_pc                     : IN STD_LOGIC;
        new_rd_lock_owner, new_pc_lock_owner : IN opcode_group_t;
        token_for_rd, token_for_pc           : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_we, writeback_update_pc  : IN opcode_group_bit_t;
        writeback_data                     : IN opcode_group_word_t;
        writeback_next_pc, writeback_token : IN opcode_group_word_t;
        writeback_rd                       : IN opcode_group_regidx_t;

        rs1_data_out, rs2_data_out, pc    : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        rs1_locked, rs2_locked, pc_locked : OUT STD_LOGIC

    );
END regfile_reduced;

ARCHITECTURE behavioural OF regfile_reduced IS

    SIGNAL owner : lock_owner_t(31 DOWNTO 0);
    TYPE registers_t IS ARRAY (31 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0);

    SIGNAL pc_owner : opcode_group_t;
    TYPE registers_of_eu_t IS ARRAY(opcode_group_t) OF registers_t;
    SIGNAL registers, wb_rd_token : registers_of_eu_t; -- := (OTHERS => (OTHERS => '0'));
    SIGNAL token_of_register : registers_t;

    SIGNAL token_of_pc : STD_LOGIC_VECTOR(31 DOWNTO 0);

    SIGNAL i_rs1, i_rs2, i_rd : INTEGER RANGE 0 TO 31;

    -- Registers
    ATTRIBUTE syn_ramstyle                     : STRING;
    ATTRIBUTE syn_ramstyle OF registers, owner, wb_rd_token : SIGNAL IS "rw_check";

    SIGNAL rs1_data_out_of_op, rs2_data_out_of_op, r_pc, rs1_token_of_op, rs2_token_of_op, wb_pc_token : opcode_group_word_t;

    SIGNAL owner_for_rs1, owner_for_rs2 : opcode_group_t;
    TYPE rd_owner_t IS ARRAY(opcode_group_t) OF opcode_group_t;
    SIGNAL rd_owner_of_op : rd_owner_t;

BEGIN

    i_rs1 <= to_integer(unsigned(rs1));
    i_rs2 <= to_integer(unsigned(rs2));
    i_rd  <= to_integer(unsigned(rd));
    pc    <= r_pc(pc_owner);

    pc_locked <= '0' WHEN wb_pc_token(pc_owner) = token_of_pc else '1'; --pc_owner = OPCODE_INVALID ELSE '1';
    rs1_locked <= '0' WHEN rs1_token_of_op(owner_for_rs1) = token_of_register(i_rs1) ELSE
        '1';
    rs2_locked <= '0' WHEN rs2_token_of_op(owner_for_rs2) = token_of_register(i_rs2) ELSE
        '1';
    rs1_data_out <= rs1_data_out_of_op(owner_for_rs1) WHEN rs1 /= "00000" ELSE
        (OTHERS => '0');
    rs2_data_out <= rs2_data_out_of_op(owner_for_rs2) WHEN rs2 /= "00000" ELSE
        (OTHERS => '0');
    owner_for_rs1 <= owner(i_rs1);
    owner_for_rs2 <= owner(i_rs2);

    PROCESS (owner, writeback_rd, i_rs1, i_rs2)
    BEGIN
        FOR x IN opcode_group_t LOOP
            rd_owner_of_op(x)     <= owner(to_integer(unsigned(writeback_rd(x))));
            rs1_data_out_of_op(x) <= registers(x)(i_rs1);
            rs2_data_out_of_op(x) <= registers(x)(i_rs2);

            rs1_token_of_op(x) <= wb_rd_token(x)(i_rs1);
            rs2_token_of_op(x) <= wb_rd_token(x)(i_rs2);
        END LOOP;
    END PROCESS;

    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            owner    <= (OTHERS => OPCODE_INVALID);
            pc_owner <= OPCODE_INVALID;
            r_pc     <= (OTHERS => entry_point);
            --wb_rd_token <= (others => X"00000000");
        ELSIF rising_edge(clk) THEN

            IF (lock_rd = '1') then
                owner(i_rd)             <= new_rd_lock_owner;
                token_of_register(i_rd) <= token_for_rd;
            END IF;
            IF (lock_pc = '1') then
                pc_owner    <= new_pc_lock_owner;
                token_of_pc <= token_for_pc;
            END IF;
            FOR x IN opcode_group_t LOOP
                IF writeback_we(x) = '1' THEN
                    registers(x)(to_integer(unsigned(writeback_rd(x)))) <= writeback_data(x);
                    wb_rd_token(x)(to_integer(unsigned(writeback_rd(x))))    <= writeback_token(x);
                END IF;

                IF writeback_update_pc(x) = '1' THEN
                    r_pc(x) <= writeback_next_pc(x);
                    wb_pc_token(x) <= writeback_token(x);
                END IF;
            END LOOP;
        END IF;
    END PROCESS;

END behavioural;