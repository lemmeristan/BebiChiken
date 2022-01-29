LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY gpio IS
  GENERIC (base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"C0002000");
  PORT (
    rst, clk : IN STD_LOGIC;
    mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_we, mem_re : IN STD_LOGIC;
    mem_wack, mem_rdy : OUT STD_LOGIC;
    address_valid : OUT STD_LOGIC;

    gpio_value, gpio_dir : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    gpio_input : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END gpio;
ARCHITECTURE Behavioural OF gpio IS

  --  SIGNAL gpio_value : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"AAAAAAAA";
  SIGNAL i_gpio_dir : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '1');

BEGIN

  PROCESS (rst, clk)
  BEGIN
    IF rst = '1' THEN
      i_gpio_dir <= (OTHERS => '1');
      gpio_value <= X"AAAAAAAA";
    ELSIF rising_edge(clk) THEN
      IF (mem_addr = base_address) AND (mem_we = '1') THEN -- direction
        i_gpio_dir <= mem_wdata;
      END IF;
      IF (mem_addr = (base_address + X"00000004")) AND (mem_we = '1') THEN
        gpio_value <= mem_wdata;
      END IF;

    END IF;
  END PROCESS;

  PROCESS (mem_addr, i_gpio_dir) --, io)
  BEGIN
    mem_rdy <= '1';
    mem_wack <= '1';
    address_valid <= '1';
    IF mem_addr = base_address THEN -- direction values
      mem_rdata <= i_gpio_dir;
    ELSIF mem_addr = base_address + X"00000004" THEN -- gpio state
      mem_rdata <= gpio_input;
    ELSE
      address_valid <= '0';
      mem_rdy <= 'Z';
      mem_wack <= 'Z';
      mem_rdata <= (OTHERS => 'Z');
    END IF;
  END PROCESS;

  gpio_dir <= i_gpio_dir;

  -- PROCESS (gpio_dir, gpio_value)
  -- BEGIN
  --   FOR i IN 0 TO 31 LOOP
  --     IF gpio_dir(i) = '1' THEN -- set as output
  --       io(i) <= gpio_value(i);
  --     ELSE
  --       io(i) <= 'Z';
  --     END IF;
  --   END LOOP;
  -- END PROCESS;

END Behavioural;