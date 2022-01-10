LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY eu_i_type IS
    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc, imm : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        rd : out std_logic_vector(4 downto 0);
        busy, update_rd : out std_logic

    );
END eu_i_type;

ARCHITECTURE behavioural OF eu_i_type IS



    SIGNAL r_rs1_data, r_rs2_data, r_instruction, r_pc, i_writeback_result, i_next_pc, r_imm : STD_LOGIC_VECTOR(31 DOWNTO 0);
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
        r_imm <= (others => '0');
    elsIF rising_edge(clk) THEN


        r_we         <= r_we(0) & we;
        IF we = '1' THEN
            r_imm <= imm;
            r_rs1_data    <= rs1_data;
            r_rs2_data    <= rs2_data;
            r_instruction <= instruction;
            r_pc          <= pc;
        END IF;
    END IF;
END PROCESS;

busy <= r_we(0); --'0' when r_we = "00" else '1';

next_pc <= i_next_pc;
writeback_rd <= i_writeback_result;
writeback_rs1 <= i_writeback_result;
writeback_rs2 <= i_writeback_result;
rd <= r_instruction(11 DOWNTO 7);
update_rd <= f_updates_rd(r_instruction);


    PROCESS (r_rs1_data, r_rs2_data, r_pc, r_instruction, r_imm)
    BEGIN
        i_writeback_result <= (OTHERS => '0');
        i_next_pc <= r_pc + X"00000004";

        --updates_pc <= '0';
        --updates_rd <= '0';
        --uses_rs1 <= '1';
        --uses_rs2 <= '0';

        CASE f_decode_opcode(r_instruction) IS


            WHEN OPCODE_I_TYPE_ADDI =>
                i_writeback_result <= r_rs1_data + r_imm;--f_decode_imm(r_instruction);
            WHEN OPCODE_I_TYPE_SLLI =>
                i_writeback_result <= DoShift(r_rs1_data, r_imm(4 DOWNTO 0),  false, true); --f_decode_imm(r_instruction)(4 DOWNTO 0), false, true);
            WHEN OPCODE_I_TYPE_SLTI =>
                IF signed(r_rs1_data) < signed(r_imm) THEN --signed(f_decode_imm(r_instruction)) THEN
                    i_writeback_result <= X"00000001";
                ELSE
                    i_writeback_result <= (OTHERS => '0');
                END IF;
            WHEN OPCODE_I_TYPE_SLTIU =>
                IF unsigned(r_rs1_data) < unsigned(r_imm) THEN --unsigned(f_decode_imm(r_instruction)) THEN
                    i_writeback_result <= X"00000001";
                ELSE
                    i_writeback_result <= (OTHERS => '0');
                END IF;
            WHEN OPCODE_I_TYPE_XORI =>
                i_writeback_result <= r_rs1_data XOR r_imm; --f_decode_imm(r_instruction);

            WHEN OPCODE_I_TYPE_SRLI =>
                i_writeback_result <= DoShift(r_rs1_data, r_imm(4 DOWNTO 0), false, false); --f_decode_imm(r_instruction)(4 DOWNTO 0), false, false);

            WHEN OPCODE_I_TYPE_SRAI =>
                i_writeback_result <= DoShift(r_rs1_data, r_imm(4 DOWNTO 0), true, false); --f_decode_imm(r_instruction)(4 DOWNTO 0), true, false);

            WHEN OPCODE_I_TYPE_ORI =>
                i_writeback_result <= r_rs1_data OR r_imm; --f_decode_imm(r_instruction);

            WHEN OPCODE_I_TYPE_ANDI =>
                i_writeback_result <= r_rs1_data AND r_imm; --f_decode_imm(r_instruction);

            WHEN OTHERS =>
        END CASE;
    END PROCESS;

END behavioural;