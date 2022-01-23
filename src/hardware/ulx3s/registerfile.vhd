LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY registerfile IS
    PORT (
        clk : IN STD_LOGIC;
        rs1, rs2, rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        data_out_rs1, data_out_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_in_rd : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        we : IN STD_LOGIC
    );
END registerfile;

ARCHITECTURE behavioural OF registerfile IS
    -- Registers
    TYPE registers_t IS ARRAY (31 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL registers : registers_t := (OTHERS => (OTHERS => '0'));

    --    ATTRIBUTE syn_ramstyle : STRING;
    --    ATTRIBUTE syn_ramstyle OF registers : SIGNAL IS "rw_check";
    --attribute noprune: boolean; attribute noprune of data_in_r: signal is true;

BEGIN

    PROCESS (clk)
    BEGIN
        IF rising_edge(clk) THEN
            IF we = '1' THEN
                registers(to_integer(unsigned(rd))) <= data_in_rd;
            END IF;
        END IF;
    END PROCESS;

    data_out_rs1 <= registers(to_integer(unsigned(rs1))) WHEN rs1 /= "00000" ELSE
        (OTHERS => '0');
    data_out_rs2 <= registers(to_integer(unsigned(rs2))) WHEN rs2 /= "00000" ELSE
        (OTHERS => '0');

END behavioural;