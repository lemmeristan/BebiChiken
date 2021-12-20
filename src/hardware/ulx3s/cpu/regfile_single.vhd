LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY regfile_single IS
    PORT (
        clk, rst : IN STD_LOGIC;
        rs1, rs2 : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        rs1_data_out, rs2_data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        update_rd : IN STD_LOGIC;
        rd_data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
        
    );
END regfile_single;

ARCHITECTURE behavioural OF regfile_single IS
    -- Registers
    TYPE registers_t IS ARRAY (31 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL registers : registers_t := (OTHERS => (OTHERS => '0'));

        -- ATTRIBUTE syn_ramstyle : STRING;
        -- ATTRIBUTE syn_ramstyle OF registers : SIGNAL IS "rw_check";
    --attribute noprune: boolean; attribute noprune of data_in_r: signal is true;

BEGIN

    PROCESS (rst, clk)
    BEGIN
--    	if rst = '1' then
--    		registers <= (others => (others => '0'));
--        els
        IF rising_edge(clk) THEN
            IF update_rd = '1' THEN
                registers(to_integer(unsigned(rd))) <= rd_data_in;
            END IF;
          
        END IF;
    END PROCESS;



     rs1_data_out <= registers(to_integer(unsigned(rs1))) WHEN rs1 /= "00000" ELSE
        (OTHERS => '0');
    rs2_data_out <= registers(to_integer(unsigned(rs2))) WHEN rs2 /= "00000" ELSE
        (OTHERS => '0');

END behavioural;