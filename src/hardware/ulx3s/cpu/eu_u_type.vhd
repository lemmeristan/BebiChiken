LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY eu_u_type IS
    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        --uses_rs1, uses_rs2, updates_rd, updates_pc, 
        rd : out std_logic_vector(4 downto 0);
        busy, update_rd : out std_logic

    );
END eu_u_type;

ARCHITECTURE behavioural OF eu_u_type IS

    SIGNAL r_rs1_data, r_rs2_data, r_instruction, r_pc, i_writeback_result, i_next_pc : STD_LOGIC_VECTOR(31 DOWNTO 0);
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
    elsIF rising_edge(clk) THEN



        r_we         <= r_we(0) & we;
        IF we = '1' THEN
            r_rs1_data    <= rs1_data;
            r_rs2_data    <= rs2_data;
            r_instruction <= instruction;
            r_pc          <= pc;
        END IF;
    END IF;
END PROCESS;

busy <= '0'; --r_we(0); -- when r_we = "00" else '1';
next_pc <= i_next_pc;
writeback_rd <= i_writeback_result;
writeback_rs1 <= i_writeback_result;
writeback_rs2 <= i_writeback_result;
rd <= r_instruction(11 DOWNTO 7);
update_rd <= f_updates_rd(r_instruction);



    PROCESS (r_rs1_data, r_rs2_data, r_pc, r_instruction)
    BEGIN
        i_writeback_result <= (OTHERS => '0');
        i_next_pc <= r_pc + X"00000004";

        CASE f_decode_opcode(r_instruction) IS
            WHEN OPCODE_U_TYPE_LUI =>
                i_writeback_result <= f_decode_imm(r_instruction);
            WHEN OPCODE_U_TYPE_AUIPC =>
                i_writeback_result <= r_pc + f_decode_imm(r_instruction);
            WHEN OTHERS =>
        END CASE;
    END PROCESS;

END behavioural;