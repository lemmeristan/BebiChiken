LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY timebase IS
  GENERIC (base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"C0000000");
  PORT (
    rst, clk : IN STD_LOGIC;
    mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_we, mem_re : IN STD_LOGIC;
    mem_wack, mem_rdy : OUT STD_LOGIC;

    address_valid : OUT STD_LOGIC
  );
END timebase;
ARCHITECTURE Behavioural OF timebase IS

  SIGNAL counter, n_counter,
  time_ns, n_time_ns, time_s, n_time_s : STD_LOGIC_VECTOR(31 DOWNTO 0);

  CONSTANT ADDR_TIMEBASE_NANOS : STD_LOGIC_VECTOR(31 DOWNTO 0) := base_address + X"00000000";
  CONSTANT ADDR_TIMEBASE_SEC : STD_LOGIC_VECTOR(31 DOWNTO 0) := base_address + X"00000004";
  CONSTANT ADDR_TIMEBASE_DELAY_NS : STD_LOGIC_VECTOR(31 DOWNTO 0) := base_address + X"00000008";

  SIGNAL state, n_state : INTEGER RANGE 0 TO 1 := 0;

BEGIN

  PROCESS (rst, clk)
  BEGIN
    IF rst = '1' THEN
      time_ns <= (OTHERS => '0');
      time_s <= (OTHERS => '0');
      counter <= (OTHERS => '0');
      state <= 0;
    ELSIF rising_edge(clk) THEN
      time_ns <= n_time_ns;
      time_s <= n_time_s;
      counter <= n_counter;
      state <= n_state;
    END IF;
  END PROCESS;
  PROCESS (state, time_ns, time_s, mem_addr, mem_we, mem_wdata, counter)
  BEGIN
    n_state <= state;
    mem_rdata <= (OTHERS => '0');
    mem_rdy <= '1';
    mem_wack <= '1';
    address_valid <= '1';
    n_counter <= counter;

    n_time_ns <= time_ns + X"0000000A";
    n_time_s <= time_s;
    IF time_ns >= X"3B9ACA00" THEN
      n_time_ns <= (OTHERS => '0');
      n_time_s <= time_s + X"00000001";
    END IF;

    CASE state IS
      WHEN 0 =>
        IF mem_addr = ADDR_TIMEBASE_NANOS THEN
          mem_rdata <= time_ns;
          IF mem_we = '1' THEN
            n_time_ns <= mem_wdata;
          END IF;

        ELSIF mem_addr = ADDR_TIMEBASE_SEC THEN
          mem_rdata <= time_s;
          IF mem_we = '1' THEN
            n_time_s <= mem_wdata;
          END IF;

        ELSIF mem_addr = ADDR_TIMEBASE_DELAY_NS THEN
          n_counter <= mem_wdata;
          mem_wack <= '0';
          IF mem_we = '1' THEN
            n_state <= 1;
          END IF;

        ELSE
          address_valid <= '0';
          mem_rdy <= 'Z';
          mem_wack <= 'Z';
          mem_rdata <= (OTHERS => 'Z');
        END IF;

      WHEN 1 =>
        mem_wack <= '0';
        IF counter < X"0000000A" THEN
          mem_wack <= '1';
          IF mem_we = '0' THEN
            n_state <= 0;
          END IF;
        ELSE
          n_counter <= counter - X"0000000A";
        END IF;

      WHEN OTHERS =>
        n_state <= 0;
    END CASE;
  END PROCESS;

END Behavioural;