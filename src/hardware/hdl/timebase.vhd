library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.std_logic_unsigned.all;

entity timebase is
  generic (base_address : std_logic_vector(31 downto 0) := X"C0000000");
  Port (
    rst, clk : in std_logic;
    mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
    mem_rdata : out std_logic_vector(31 downto 0);
    mem_we, mem_re : in std_logic;
    mem_wack, mem_rdy : out std_logic;

    address_valid : out std_logic
  );
end timebase;


architecture Behavioural of timebase is

  signal counter, n_counter, 
  time_ns, n_time_ns, time_s, n_time_s : std_logic_vector(31 downto 0);

  constant ADDR_TIMEBASE_NANOS : std_logic_vector(31 downto 0) := base_address + X"00000000";
  constant ADDR_TIMEBASE_SEC : std_logic_vector(31 downto 0) := base_address + X"00000004";
  constant ADDR_TIMEBASE_DELAY_NS : std_logic_vector(31 downto 0) := base_address + X"00000008";

  signal state, n_state : integer range 0 to 1 := 0;

begin

process(rst, clk)
begin
  if rst = '1' then
    time_ns <= (others => '0');
    time_s <= (others => '0');
    counter <= (others => '0');
    state <= 0;
  elsif rising_edge(clk) then
    time_ns <= n_time_ns;
    time_s <= n_time_s;
    counter <= n_counter;
    state <= n_state;
  end if;
end process;


process(state, time_ns, time_s, mem_addr, mem_we, mem_wdata, counter)
begin
  n_state <= state;
  mem_rdata <= (others => '0');
  mem_rdy <= '1';
  mem_wack <= '1';
  address_valid <= '1';
  n_counter <= counter;

  n_time_ns <= time_ns + X"0000000A";
  n_time_s <= time_s;
  if time_ns >= X"3B9ACA00" then
    n_time_ns <= (others => '0');
    n_time_s <= time_s + X"00000001";
  end if;

  case state is
    when 0 =>
      case mem_addr is
        when ADDR_TIMEBASE_NANOS =>
          mem_rdata <= time_ns;
          if mem_we = '1' then
            n_time_ns <= mem_wdata;
          end if;

        when ADDR_TIMEBASE_SEC =>
          mem_rdata <= time_s;
          if mem_we = '1' then
            n_time_s <= mem_wdata;
          end if;

        when ADDR_TIMEBASE_DELAY_NS =>
          n_counter <= mem_wdata;
          mem_wack <= '0';
          if mem_we = '1' then
            n_state <= 1;
          end if;
          
        when others =>
          address_valid <= '0';
          mem_rdy <= 'Z';
          mem_wack <= 'Z';
          mem_rdata <= (others => 'Z');
      end case;

    when 1 =>
      mem_wack <= '0';
      if counter < X"0000000A" then
        mem_wack <= '1';
        if mem_we = '0' then
          n_state <= 0;
        end if;
      else
        n_counter <= counter - X"0000000A";
      end if;

    when others =>
      n_state <= 0;
  end case;
end process;

end Behavioural;