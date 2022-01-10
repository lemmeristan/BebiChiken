LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY eu_branch_type IS

    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        rd : out std_logic_vector(4 downto 0);
        busy, update_rd, update_pc : out std_logic

    );
END eu_branch_type;

ARCHITECTURE behavioural OF eu_branch_type IS
    SIGNAL r_rs1_data, r_rs2_data, r_instruction, r_pc, i_writeback_result, i_next_pc : STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal r_we : std_logic_vector(1 downto 0);
    signal i_update_pc : std_logic;

BEGIN


PROCESS (rst, clk)
BEGIN
    if rst = '1' then
        r_we <= "00";
        r_rs1_data <= (others => '0');
        r_rs2_data <= (others => '0');
        r_instruction <= (others => '0');
        r_pc <= (others => '0');
    elsIF rising_edge(clk) THEN

        next_pc <= i_next_pc;
        writeback_rd <= i_writeback_result;
        writeback_rs1 <= i_writeback_result;
        writeback_rs2 <= i_writeback_result;
        rd <= r_instruction(11 DOWNTO 7);
        update_rd <= f_updates_rd(r_instruction);
        update_pc <= i_update_pc;

        r_we         <= r_we(0) & we;
        IF we = '1' THEN
            r_rs1_data    <= rs1_data;
            r_rs2_data    <= rs2_data;
            r_instruction <= instruction;
            r_pc          <= pc;
        END IF;
    END IF;
END PROCESS;

busy <= '0' when r_we = "00" else '1';



    PROCESS (r_rs1_data, r_rs2_data, we, r_pc, r_instruction, r_we)
    BEGIN
        i_writeback_result <= (OTHERS => '0');
        i_next_pc <= r_pc + X"00000004";

        i_update_pc <= '0';
        if r_we(0) = '0' and we = '1' then
            i_update_pc <= '1';
        end if;

        CASE f_decode_opcode(r_instruction) IS

            WHEN OPCODE_J_TYPE_JAL =>
                i_writeback_result <= r_pc + X"00000004";
                i_next_pc <= r_pc + f_decode_imm(r_instruction);

            WHEN OPCODE_J_TYPE_JALR =>
                i_writeback_result <= r_pc + X"00000004";
                i_next_pc <= (f_decode_imm(r_instruction) + r_rs1_data) AND X"FFFFFFFE";

            WHEN OPCODE_B_TYPE_BEQ =>
                IF signed(r_rs1_data) = signed(r_rs2_data) THEN
                    i_next_pc <= r_pc + f_decode_imm(r_instruction);
                END IF;
            WHEN OPCODE_B_TYPE_BNE =>
                IF signed(r_rs1_data) /= signed(r_rs2_data) THEN
                    i_next_pc <= r_pc + f_decode_imm(r_instruction);
                END IF;
            WHEN OPCODE_B_TYPE_BLT =>
                IF signed(r_rs1_data) < signed(r_rs2_data) THEN
                    i_next_pc <= r_pc + f_decode_imm(r_instruction);
                END IF;
            WHEN OPCODE_B_TYPE_BGE =>
                IF signed(r_rs1_data) >= signed(r_rs2_data) THEN
                    i_next_pc <= r_pc + f_decode_imm(r_instruction);
                END IF;
            WHEN OPCODE_B_TYPE_BLTU =>
                IF unsigned(r_rs1_data) < unsigned(r_rs2_data) THEN
                    i_next_pc <= r_pc + f_decode_imm(r_instruction);
                END IF;
            WHEN OPCODE_B_TYPE_BGEU =>
                IF unsigned(r_rs1_data) >= unsigned(r_rs2_data) THEN
                    i_next_pc <= r_pc + f_decode_imm(r_instruction);
                END IF;



            WHEN OTHERS =>
            i_next_pc <= r_pc;
            i_update_pc <= '0';
        END CASE;
    END PROCESS;

END behavioural;