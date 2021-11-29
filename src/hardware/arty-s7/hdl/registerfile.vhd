library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.std_logic_unsigned.all;

entity registerFile is
  Port (
    clk : in std_logic;
    rs1, rs2, rd : in std_logic_vector(4 downto 0);
    data_out_rs1, data_out_rs2 : out std_logic_vector(31 downto 0);
    data_in_rd : in std_logic_vector(31 downto 0);
    we : in std_logic
  );
end registerFile;

architecture behavioural of registerFile is
    -- Registers
    type registers_t is array (31 downto 0) of std_logic_vector(31 downto 0);
    signal registers : registers_t := (others => (others => '0'));

    attribute syn_ramstyle : string;
    attribute syn_ramstyle of registers : signal is "rw_check";
    --attribute noprune: boolean; attribute noprune of data_in_r: signal is true;

begin

    process(clk)
    begin
        if rising_edge(clk) then
            if we = '1' then
                registers(to_integer(unsigned(rd))) <= data_in_rd;
            end if;
        end if;
    end process;

    data_out_rs1 <= registers(to_integer(unsigned(rs1))) when rs1 /= "00000" else (others => '0');
    data_out_rs2 <= registers(to_integer(unsigned(rs2))) when rs2 /= "00000" else (others => '0');

end behavioural;