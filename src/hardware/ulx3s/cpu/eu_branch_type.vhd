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
        rs1_data, rs2_data, instruction, pc, token, imm : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_next_pc, writeback_data, writeback_token : out std_logic_vector(31 downto 0);
        writeback_we, writeback_update_pc  : out std_logic;
        writeback_rd                       : out std_logic_vector(4 downto 0);

        busy : out std_logic

    );
END eu_branch_type;

ARCHITECTURE behavioural OF eu_branch_type IS
    SIGNAL r_rs1_data, r_rs2_data, r_instruction, r_pc, r_token, i_writeback_result, i_next_pc, r_imm : STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal r_we : std_logic_vector(1 downto 0);

BEGIN


PROCESS (rst, clk)
BEGIN
    if rst = '1' then
        r_we <= "00";
        r_rs1_data <= (others => '0');
        r_rs2_data <= (others => '0');
        r_instruction <= (others => '0');
        r_pc <= (others => '0');
        r_token <= (others => '0');
        r_imm <= (others => '0');
        writeback_next_pc <= (others => '0');
    elsIF rising_edge(clk) THEN
    writeback_next_pc <= i_next_pc;

        r_we         <= r_we(0) & we;
        IF we = '1' THEN
            r_rs1_data    <= rs1_data;
            r_rs2_data    <= rs2_data;
            r_instruction <= instruction;
            r_pc          <= pc;
            r_imm <= imm;
            r_token <= token;
        END IF;
    END IF;
END PROCESS;

busy <= '0' when r_we = "00" else '1';

writeback_we <= r_we(1) when f_updates_rd(r_instruction) = '1' else '0';
writeback_update_pc <= r_we(1) when f_updates_pc(r_instruction) = '1' else '0';
writeback_token <= r_token;
writeback_data <= i_writeback_result;
writeback_rd <= r_instruction(11 DOWNTO 7);

    PROCESS (r_rs1_data, r_rs2_data, r_pc, r_imm, r_instruction)
    BEGIN
        i_writeback_result <= (OTHERS => '0');
        i_next_pc <= r_pc + X"00000004";


        CASE f_decode_opcode(r_instruction) IS

            WHEN OPCODE_J_TYPE_JAL =>
                i_writeback_result <= r_pc + X"00000004";
                i_next_pc <= r_pc + r_imm;

            WHEN OPCODE_J_TYPE_JALR =>
                i_writeback_result <= r_pc + X"00000004";
                i_next_pc <= (r_imm + r_rs1_data) AND X"FFFFFFFE";

            WHEN OPCODE_B_TYPE_BEQ =>
                IF signed(r_rs1_data) = signed(r_rs2_data) THEN
                    i_next_pc <= r_pc + r_imm;
                END IF;
            WHEN OPCODE_B_TYPE_BNE =>
                IF signed(r_rs1_data) /= signed(r_rs2_data) THEN
                    i_next_pc <= r_pc + r_imm;
                END IF;
            WHEN OPCODE_B_TYPE_BLT =>
                IF signed(r_rs1_data) < signed(r_rs2_data) THEN
                    i_next_pc <= r_pc + r_imm;
                END IF;
            WHEN OPCODE_B_TYPE_BGE =>
                IF signed(r_rs1_data) >= signed(r_rs2_data) THEN
                    i_next_pc <= r_pc + r_imm;
                END IF;
            WHEN OPCODE_B_TYPE_BLTU =>
                IF unsigned(r_rs1_data) < unsigned(r_rs2_data) THEN
                    i_next_pc <= r_pc + r_imm;
                END IF;
            WHEN OPCODE_B_TYPE_BGEU =>
                IF unsigned(r_rs1_data) >= unsigned(r_rs2_data) THEN
                    i_next_pc <= r_pc + r_imm;
                END IF;



            WHEN OTHERS =>
            i_next_pc <= r_pc;
        END CASE;
    END PROCESS;

END behavioural;