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
        rs1_data, instruction, token, imm : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_data, writeback_token : out std_logic_vector(31 downto 0);
        writeback_we  : out std_logic;
        writeback_rd                       : out std_logic_vector(4 downto 0);

        busy : out std_logic

    );
END eu_i_type;

ARCHITECTURE behavioural OF eu_i_type IS
    SIGNAL r_rs1_data, r_instruction, r_token, i_writeback_result, r_imm : STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal r_we : std_logic_vector(1 downto 0);

BEGIN


PROCESS (rst, clk)
BEGIN
    if rst = '1' then
        r_we <= "00";
        r_rs1_data <= (others => '0');
        r_instruction <= (others => '0');
        r_token <= (others => '0');
        r_imm <= (others => '0');
    elsIF rising_edge(clk) THEN

        r_we         <= r_we(0) & we;
        IF we = '1' THEN
            r_rs1_data    <= rs1_data;
            r_instruction <= instruction;
            r_token <= token;
            r_imm <= imm;
        END IF;
    END IF;
END PROCESS;

busy <= '0' when r_we = "00" else '1';

writeback_we <= r_we(1) when f_updates_rd(r_instruction) = '1' else '0';
writeback_token <= r_token;
writeback_data <= i_writeback_result;
writeback_rd <= r_instruction(11 DOWNTO 7);

    PROCESS (r_rs1_data, r_imm, r_instruction)
    BEGIN
        i_writeback_result <= (OTHERS => '0');


        CASE f_decode_opcode(r_instruction) IS


            WHEN OPCODE_I_TYPE_ADDI =>
                i_writeback_result <= r_rs1_data + r_imm;
            WHEN OPCODE_I_TYPE_SLLI =>
                i_writeback_result <= DoShift(r_rs1_data, r_imm(4 DOWNTO 0),  false, true);
            WHEN OPCODE_I_TYPE_SLTI =>
                IF signed(r_rs1_data) < signed(r_imm) THEN
                    i_writeback_result <= X"00000001";
                ELSE
                    i_writeback_result <= (OTHERS => '0');
                END IF;
            WHEN OPCODE_I_TYPE_SLTIU =>
                IF unsigned(r_rs1_data) < unsigned(r_imm) THEN
                    i_writeback_result <= X"00000001";
                ELSE
                    i_writeback_result <= (OTHERS => '0');
                END IF;
            WHEN OPCODE_I_TYPE_XORI =>
                i_writeback_result <= r_rs1_data XOR r_imm; 

            WHEN OPCODE_I_TYPE_SRLI =>
                i_writeback_result <= DoShift(r_rs1_data, r_imm(4 DOWNTO 0), false, false);

            WHEN OPCODE_I_TYPE_SRAI =>
                i_writeback_result <= DoShift(r_rs1_data, r_imm(4 DOWNTO 0), true, false);

            WHEN OPCODE_I_TYPE_ORI =>
                i_writeback_result <= r_rs1_data OR r_imm; 

            WHEN OPCODE_I_TYPE_ANDI =>
                i_writeback_result <= r_rs1_data AND r_imm; 

            WHEN OTHERS =>
        END CASE;
    END PROCESS;

END behavioural;