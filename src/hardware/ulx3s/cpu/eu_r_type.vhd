LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY eu_r_type IS

    PORT (
        rst, clk : IN STD_LOGIC;

        we                                     : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, token : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_data, writeback_token : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        writeback_we                    : OUT STD_LOGIC;
        writeback_rd                    : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);

        busy : OUT STD_LOGIC

    );
END eu_r_type;

ARCHITECTURE behavioural OF eu_r_type IS
    SIGNAL r_rs1_data, r_rs2_data, r_instruction, r_token, i_writeback_result : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL r_we                                                               : STD_LOGIC_VECTOR(1 DOWNTO 0);
    SIGNAL i_writeback_result_intermediate                                    : opcode_word_t;
    SIGNAL op                                                                 : opcode_t;
BEGIN
    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            r_we          <= "00";
            r_rs1_data    <= (OTHERS => '0');
            r_rs2_data    <= (OTHERS => '0');
            r_instruction <= (OTHERS => '0');
            r_token       <= (OTHERS => '0');
            op            <= OPCODE_INVALID;

        ELSIF rising_edge(clk) THEN
            op <= f_decode_opcode(r_instruction);

            r_we <= r_we(0) & we;
            IF we = '1' THEN
                r_rs1_data    <= rs1_data;
                r_rs2_data    <= rs2_data;
                r_instruction <= instruction;
                r_token       <= token;
            END IF;
        END IF;
    END PROCESS;

    busy <= '0' WHEN r_we = "00" ELSE
        '1';

    writeback_we <= r_we(1) WHEN f_updates_rd(r_instruction) = '1' ELSE
        '0';
    writeback_token <= r_token;
    writeback_data  <= i_writeback_result;
    writeback_rd    <= r_instruction(11 DOWNTO 7);

    PROCESS (r_rs1_data, r_rs2_data)
    BEGIN
        i_writeback_result_intermediate <= (OTHERS => (OTHERS => '0'));

        i_writeback_result_intermediate(OPCODE_R_TYPE_ADD) <= r_rs1_data + r_rs2_data;

        i_writeback_result_intermediate(OPCODE_R_TYPE_SUB) <= r_rs1_data - r_rs2_data;

        i_writeback_result_intermediate(OPCODE_R_TYPE_SLL) <= DoShift(r_rs1_data, r_rs2_data(4 DOWNTO 0), false, true);

        IF signed(r_rs1_data) < signed(r_rs2_data) THEN
            i_writeback_result_intermediate(OPCODE_R_TYPE_SLT) <= X"00000001";
        ELSE
            i_writeback_result_intermediate(OPCODE_R_TYPE_SLT) <= (OTHERS => '0');
        END IF;

        IF unsigned(r_rs1_data) < unsigned(r_rs2_data) THEN
            i_writeback_result_intermediate(OPCODE_R_TYPE_SLTU) <= X"00000001";
        ELSE
            i_writeback_result_intermediate(OPCODE_R_TYPE_SLTU) <= (OTHERS => '0');
        END IF;

        i_writeback_result_intermediate(OPCODE_R_TYPE_XOR) <= r_rs1_data XOR r_rs2_data;

        i_writeback_result_intermediate(OPCODE_R_TYPE_SRL) <= DoShift(r_rs1_data, r_rs2_data(4 DOWNTO 0), false, false);

        i_writeback_result_intermediate(OPCODE_R_TYPE_SRA) <= DoShift(r_rs1_data, r_rs2_data(4 DOWNTO 0), true, false);

        i_writeback_result_intermediate(OPCODE_R_TYPE_OR) <= r_rs1_data OR r_rs2_data;

        i_writeback_result_intermediate(OPCODE_R_TYPE_AND) <= r_rs1_data AND r_rs2_data;
    END PROCESS;

    i_writeback_result <= i_writeback_result_intermediate(op);

END behavioural;