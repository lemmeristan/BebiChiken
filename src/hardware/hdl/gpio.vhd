library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;

entity gpio is
  generic (base_address : std_logic_vector(31 downto 0) := X"C0002000");
  Port (
    rst, clk : in std_logic;
    mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
    mem_rdata : out std_logic_vector(31 downto 0);
    mem_we, mem_re : in std_logic;
    mem_wack, mem_rdy : out std_logic;
    address_valid : out std_logic;
    
    gpio : inout std_logic_vector(31 downto 0)

  );
end gpio;


architecture Behavioural of gpio is

  signal gpio_value, gpio_dir : std_logic_vector(31 downto 0);

begin

process(rst, clk)
begin
  if rst = '1' then
    gpio_dir <= (others => '0');
    gpio_value <= (others => '0');
  elsif rising_edge(clk) then
    if (mem_addr = base_address) and (mem_we = '1') then -- direction
      gpio_dir <= mem_wdata;
    end if;


    if (mem_addr = (base_address+X"00000004")) and (mem_we = '1') then
      gpio_value <= mem_wdata;
    end if;

  end if;
end process;

process(mem_addr, gpio_dir, gpio)
begin
  mem_rdy <= '1';
  mem_wack <= '1';
  address_valid <= '1';
  case mem_addr is
    when base_address => -- direction values
      mem_rdata <= gpio_dir;
    when base_address + X"00000004" => -- gpio state
      mem_rdata <= gpio;
    when others =>
      address_valid <= '0';
      mem_rdy <= 'Z';
      mem_wack <= 'Z';
      mem_rdata <= (others => 'Z');
  end case;
end process;

process(gpio_dir, gpio_value)
begin
    for i in 0 to 31 loop
        if gpio_dir(i) = '1' then -- set as output
            gpio(i) <= gpio_value(i);
        else
            gpio(i) <= 'Z';
        end if;
    end loop;
end process;

end Behavioural;