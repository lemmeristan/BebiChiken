LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

LIBRARY work;
USE work.bebichiken.ALL;

ENTITY regfile_half IS
    GENERIC(
        entry_point : std_logic_vector(31 downto 0) := X"00000000"
    );
    PORT (

        clk, rst                   : IN STD_LOGIC;
        rs1, rs2, rd                   : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        rs1_data_out, rs2_data_out, pc : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        update_rd, update_pc                  : IN STD_LOGIC;
        rd_data_in, next_pc                 : IN STD_LOGIC_VECTOR(31 DOWNTO 0)

    );
END regfile_half;

ARCHITECTURE behavioural OF regfile_half IS
    -- Registers

    

    TYPE registers_t IS ARRAY (31 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL registers1, registers2 : registers_t; -- := (OTHERS => (OTHERS => '0'));

    --    ATTRIBUTE syn_ramstyle : STRING;
    --    ATTRIBUTE syn_ramstyle OF registers : SIGNAL IS "rw_check";
    --attribute noprune: boolean; attribute noprune of data_in_r: signal is true;

BEGIN

   rs1_data_out <= registers1(to_integer(unsigned(rs1))) WHEN rs1 /= "00000" ELSE
       (OTHERS => '0');
   rs2_data_out <= registers2(to_integer(unsigned(rs2))) WHEN rs2 /= "00000" ELSE
       (OTHERS => '0');


    --    process(rst,clk)
    --    begin
    --     if rst = '1' then
    --                     rs1_data_out <= (others => '0');
    --         rs2_data_out <= (others => '0');
    --         elsif falling_edge(clk) then
    --                         rs1_data_out <= (others => '0');
    --         if rs1 /= "00000" then
    --             rs1_data_out <= registers1(to_integer(unsigned(rs1)));
    --         end if;

    --         rs2_data_out <= (others => '0');
    --         if rs2 /= "00000" then
    --             rs2_data_out <= registers2(to_integer(unsigned(rs2)));
    --         end if;
    --     end if;
    --     end process;

    process(rst, clk)
    begin
        if rst = '1' then
            registers1 <= (others => (others => '0'));
            registers2 <= (others => (others => '0'));
            pc <= entry_point;
            --rs1_data_out <= (others => '0');
            --rs2_data_out <= (others => '0');

        elsif rising_edge(clk) then
            if update_rd = '1' then
                registers1(to_integer(unsigned(rd))) <= rd_data_in;
                registers2(to_integer(unsigned(rd))) <= rd_data_in;
            end if;
            
            if update_pc = '1' then
                pc <= next_pc;
            end if;

            -- rs1_data_out <= (others => '0');
            -- if rs1 /= "00000" then
            --     rs1_data_out <= registers1(to_integer(unsigned(rs1)));
            -- end if;

            -- rs2_data_out <= (others => '0');
            -- if rs2 /= "00000" then
            --     rs2_data_out <= registers2(to_integer(unsigned(rs2)));
            -- end if;

            
                


        end if;
    end process;



END behavioural;