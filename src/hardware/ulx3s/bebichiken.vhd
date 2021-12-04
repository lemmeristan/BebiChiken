LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY bebichiken IS
  PORT (
    --rst, clk : in std_logic;

    clk_25mhz : IN STD_LOGIC;
    btn : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
    led : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    ftdi_rxd : OUT STD_LOGIC;
    --gpdi_dp : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
    --wifi_gpio0 : OUT STD_LOGIC;

    flash_csn : OUT STD_LOGIC;
    flash_mosi : INOUT STD_LOGIC; -- io(0)
    flash_miso : IN STD_LOGIC; -- io(1)
    flash_wpn : INOUT STD_LOGIC; -- io(2)
    flash_holdn : INOUT STD_LOGIC -- io(3)

    --SD_DAT3, SD_CMD, SD_CLK : out std_logic;
    --SD_DAT0 : in std_logic := '0'

    --OLED_CS, OLED_MOSI, OLED_SCK, OLED_DC, OLED_RES, OLED_VCCEN, OLED_PMODEN : OUT STD_LOGIC;

    --ck_scl, ck_sda : INOUT STD_LOGIC

  );
END bebichiken;
ARCHITECTURE behavioural OF bebichiken IS

  COMPONENT registerfile PORT (
    clk : IN STD_LOGIC;
    rs1, rs2, rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
    data_out_rs1, data_out_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    data_in_rd : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    we : IN STD_LOGIC
    );
  END COMPONENT;
  COMPONENT cpu PORT (
    rst, clk : IN STD_LOGIC;

    -- Instruction memory bus
    inst_width : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
    inst_addr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    inst_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    inst_re : OUT STD_LOGIC;
    inst_rdy : IN STD_LOGIC;

    -- Data memory bus
    data_width : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
    data_addr, data_wdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    data_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    data_re, data_we : OUT STD_LOGIC;
    data_rdy, data_wack : IN STD_LOGIC;

    -- Register file
    registerfile_rs1, registerfile_rs2, registerfile_rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
    registerfile_wdata_rd : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    registerfile_rdata_rs1, registerfile_rdata_rs2 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    registerfile_we : OUT STD_LOGIC;

    err : OUT STD_LOGIC
    );
  END COMPONENT;

  COMPONENT block_ram PORT (
    rst, clk : IN STD_LOGIC;
    mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_we, mem_re : IN STD_LOGIC;
    mem_width : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    mem_rdy, mem_wack : OUT STD_LOGIC;
    address_valid : OUT STD_LOGIC
    );
  END COMPONENT;

  COMPONENT uart PORT (rst, clk : IN STD_LOGIC;
    txd : OUT STD_LOGIC;
    mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_we, mem_re : IN STD_LOGIC;
    mem_wack, mem_rdy : OUT STD_LOGIC;
    address_valid : OUT STD_LOGIC
    );
  END COMPONENT;

  COMPONENT gpio PORT (
    rst, clk : IN STD_LOGIC;
    mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_we, mem_re : IN STD_LOGIC;
    mem_wack, mem_rdy : OUT STD_LOGIC;

    gpio_dir, gpio_value : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    gpio_input : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    address_valid : OUT STD_LOGIC
    );
  END COMPONENT;

  COMPONENT timebase PORT (
    rst, clk : IN STD_LOGIC;
    mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_we, mem_re : IN STD_LOGIC;
    mem_wack, mem_rdy : OUT STD_LOGIC;
    address_valid : OUT STD_LOGIC
    );
  END COMPONENT;

  COMPONENT quadflash_cache
    GENERIC (
      vendor : STD_LOGIC; -- 0 => xilinx, 1 => lattice

      base_address : STD_LOGIC_VECTOR(31 DOWNTO 0)
    );

    PORT (
      reset : IN STD_LOGIC;
      clk : IN STD_LOGIC;

      mem_clk : IN STD_LOGIC;
      mem_re : IN STD_LOGIC;
      mem_addr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      mem_rdy : OUT STD_LOGIC;
      spi_csn, spi_sck, spi_di, spi_wpn, spi_holdn : OUT STD_LOGIC;
      spi_do : IN STD_LOGIC;

      spi_io : IN STD_LOGIC_VECTOR(3 DOWNTO 0);

      spi_reading : OUT STD_LOGIC;
      led : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
  END COMPONENT;

  COMPONENT USRMCLK
    PORT (
      USRMCLKI : IN STD_ULOGIC;
      USRMCLKTS : IN STD_ULOGIC
    );
  END COMPONENT;
  ATTRIBUTE syn_noprune : BOOLEAN;
  ATTRIBUTE syn_noprune OF USRMCLK : COMPONENT IS true;
  COMPONENT spimaster PORT (
    rst, clk : IN STD_LOGIC;
    sck, mosi : OUT STD_LOGIC;
    miso : IN STD_LOGIC;
    mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_we, mem_re : IN STD_LOGIC;
    mem_wack, mem_rdy : OUT STD_LOGIC;
    address_valid : OUT STD_LOGIC
    );
  END COMPONENT;

  COMPONENT i2cmaster PORT (
    rst, clk : IN STD_LOGIC;
    scl, sda : INOUT STD_LOGIC;
    mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_we, mem_re : IN STD_LOGIC;
    mem_wack, mem_rdy : OUT STD_LOGIC;
    address_valid : OUT STD_LOGIC
    );
  END COMPONENT;
  COMPONENT hdmi PORT (

    rst, clk : IN STD_LOGIC;
    mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    mem_we, mem_re : IN STD_LOGIC;
    mem_width : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    mem_rdy, mem_wack : OUT STD_LOGIC;

    address_valid : OUT STD_LOGIC;

    gpdi_dp : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
    --gpdi_dn : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
    );
  END COMPONENT;

  -- component uart PORT (
  --   rst, CLK100MHZ : in std_logic;
  --   txd : out std_logic
  -- );
  -- end component;

  -- component ila_0 PORT (
  --   clk : in std_logic;
  --   probe0, probe1 : in std_logic_vector(31 downto 0)
  -- );
  -- end component;

  SIGNAL registerfile_rs1, registerfile_rs2, registerfile_rd : STD_LOGIC_VECTOR(4 DOWNTO 0);
  SIGNAL registerfile_rdata_rs1, registerfile_rdata_rs2, registerfile_wdata_rd : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL registerfile_we : STD_LOGIC;

  SIGNAL inst_re, inst_rdy : STD_LOGIC;

  SIGNAL inst_addr, inst_rdata : STD_LOGIC_VECTOR(31 DOWNTO 0);

  SIGNAL inst_width, mem_width : STD_LOGIC_VECTOR(1 DOWNTO 0);

  SIGNAL rst, clk : STD_LOGIC;

  SIGNAL mem_addr, mem_wdata, mem_rdata, int_gpio : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL mem_rdy, mem_wack, mem_we, mem_re : STD_LOGIC;

  ---------------------------------------------------------------------------
  -- Peripherals
  ---------------------------------------------------------------------------

  CONSTANT PERIPHERAL_RAM : INTEGER := 0;
  CONSTANT PERIPHERAL_TIMEBASE : INTEGER := 3;
  CONSTANT PERIPHERAL_UART : INTEGER := 4;
  CONSTANT PERIPHERAL_GPIO : INTEGER := 5;
  CONSTANT PERIPHERAL_SPIMASTER : INTEGER := 6;
  CONSTANT PERIPHERAL_I2CMASTER : INTEGER := 7;
  CONSTANT PERIPHERAL_HDMI : INTEGER := 8;

  CONSTANT PERIPHERAL_MAX : INTEGER := 9;

  SIGNAL i_mem_rdy, i_mem_wack, i_address_valid : STD_LOGIC_VECTOR(PERIPHERAL_MAX - 1 DOWNTO 0);
  TYPE mem_rdata_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
  TYPE addr_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(19 DOWNTO 0);

  SIGNAL i_mem_rdata : mem_rdata_t(PERIPHERAL_MAX - 1 DOWNTO 0) := (OTHERS => (OTHERS => '0'));

  SIGNAL gpio_dir, gpio_value : STD_LOGIC_VECTOR(31 DOWNTO 0);

  --- ILA

  -- COMPONENT ila_1 PORT (
  --   clk : IN STD_LOGIC;
  --   probe0, probe1, probe2, probe3, probe4, probe5, probe6 : IN STD_LOGIC
  --   );
  -- END COMPONENT;

  SIGNAL spi_csn, spi_clk, spi_di, spi_do, spi_wpn, spi_holdn, spi_reading : STD_LOGIC;

  SIGNAL spi_io : STD_LOGIC_VECTOR(3 DOWNTO 0);

  SIGNAL debouncectr : INTEGER;
BEGIN

  u1 : USRMCLK PORT MAP(
    USRMCLKI => spi_clk,
    USRMCLKTS => rst);
  --clk <= CLK100MHZ;
  --rst <= (NOT btn(0)) OR btn(1) OR btn(2) OR btn(3) OR btn(4) OR btn(5) OR btn(6);
  PROCESS (clk_25mhz)
  BEGIN
    IF rising_edge(clk_25mhz) THEN
      IF btn(0) = '1' THEN

        IF debouncectr < 100000 THEN
          debouncectr <= debouncectr + 1;
        ELSE
          rst <= '0';
        END IF;
      ELSE
        rst <= '1';
        debouncectr <= 0;
      END IF;
    END IF;
  END PROCESS;

  --inst_rdy <= '1';

  clk <= clk_25mhz;
  i_gpio : gpio PORT MAP(
    rst => rst, clk => clk,
    mem_addr => mem_addr, mem_wdata => mem_wdata,
    mem_rdata => i_mem_rdata(PERIPHERAL_GPIO),
    mem_we => mem_we, mem_re => mem_re,
    mem_wack => i_mem_wack(PERIPHERAL_GPIO), mem_rdy => i_mem_rdy(PERIPHERAL_GPIO),
    address_valid => i_address_valid(PERIPHERAL_GPIO),
    gpio_dir => gpio_dir, gpio_value => gpio_value, gpio_input => (OTHERS => '0')
  );

  --led <= inst_addr(7 DOWNTO 0);
  led <= rst & int_gpio(6 DOWNTO 0);
  --led <= mem_addr(7 DOWNTO 0);

  PROCESS (gpio_dir, gpio_value)
  BEGIN
    FOR i IN 0 TO 31 LOOP
      IF gpio_dir(i) = '1' THEN -- set as output
        int_gpio(i) <= gpio_value(i);
      ELSE
        int_gpio(i) <= 'Z';
      END IF;
    END LOOP;
  END PROCESS;

  i_uart : uart PORT MAP(
    rst => rst, clk => clk,
    txd => ftdi_rxd,
    mem_addr => mem_addr, mem_wdata => mem_wdata,
    mem_rdata => i_mem_rdata(PERIPHERAL_UART),
    mem_we => mem_we, mem_re => mem_re,
    mem_wack => i_mem_wack(PERIPHERAL_UART),
    mem_rdy => i_mem_rdy(PERIPHERAL_UART),
    address_valid => i_address_valid(PERIPHERAL_UART)
  );

  i_timebase : timebase PORT MAP(
    rst => rst, clk => clk,
    mem_addr => mem_addr, mem_wdata => mem_wdata,
    mem_rdata => i_mem_rdata(PERIPHERAL_TIMEBASE),
    mem_we => mem_we, mem_re => mem_re,
    mem_wack => i_mem_wack(PERIPHERAL_TIMEBASE),
    mem_rdy => i_mem_rdy(PERIPHERAL_TIMEBASE),
    address_valid => i_address_valid(PERIPHERAL_TIMEBASE)
  );

  i_ram : block_ram PORT MAP(
    rst => rst, clk => clk,
    mem_addr => mem_addr, mem_wdata => mem_wdata,
    mem_width => mem_width,
    mem_rdata => i_mem_rdata(PERIPHERAL_RAM),
    mem_we => mem_we, mem_re => mem_re,
    mem_wack => i_mem_wack(PERIPHERAL_RAM),
    mem_rdy => i_mem_rdy(PERIPHERAL_RAM),
    address_valid => i_address_valid(PERIPHERAL_RAM)
  );

  -- i_spimaster : spimaster PORT MAP(
  --   rst => rst, clk => clk,
  --   sck => sck, mosi => mosi, miso => miso,
  --   mem_addr => mem_addr, mem_wdata => mem_wdata,
  --   mem_rdata => i_mem_rdata(PERIPHERAL_SPIMASTER),
  --   mem_we => mem_we, mem_re => mem_re,
  --   mem_rdy => i_mem_rdy(PERIPHERAL_SPIMASTER), mem_wack => i_mem_wack(PERIPHERAL_SPIMASTER),
  --   address_valid => i_address_valid(PERIPHERAL_SPIMASTER)
  -- );

  -- i_i2cmaster : i2cmaster PORT MAP(
  --   rst => rst, clk => clk,
  --   scl => ck_scl, sda => ck_sda,
  --   mem_addr => mem_addr, mem_wdata => mem_wdata,
  --   mem_rdata => i_mem_rdata(PERIPHERAL_I2CMASTER),
  --   mem_we => mem_we, mem_re => mem_re,
  --   mem_rdy => i_mem_rdy(PERIPHERAL_I2CMASTER), mem_wack => i_mem_wack(PERIPHERAL_I2CMASTER),
  --   address_valid => i_address_valid(PERIPHERAL_I2CMASTER)
  -- );

  i_quadflash_cache : quadflash_cache GENERIC MAP(
    vendor => '1',
    base_address => X"00000000"
    ) PORT MAP(
    reset => rst,
    clk => clk_25mhz,

    mem_clk => clk_25mhz,
    mem_re => inst_re,
    mem_addr => inst_addr,
    mem_rdata => inst_rdata,
    mem_rdy => inst_rdy,

    spi_csn => spi_csn, spi_sck => spi_clk,
    spi_di => spi_di, spi_do => spi_do, spi_wpn => spi_wpn, spi_holdn => spi_holdn,

    spi_io => spi_io, spi_reading => spi_reading --, led => led
  );

  spi_io <= flash_holdn & flash_wpn & flash_miso & flash_mosi;

  PROCESS (spi_reading, spi_holdn, spi_wpn, spi_di)
  BEGIN
    IF spi_reading = '1' THEN
      flash_mosi <= 'Z';
      flash_wpn <= 'Z';
      flash_holdn <= 'Z';
    ELSE
      flash_mosi <= spi_di;
      flash_wpn <= spi_wpn;
      flash_holdn <= spi_holdn;
    END IF;
  END PROCESS;

  flash_csn <= spi_csn;
  spi_do <= flash_miso;

  i_cpu : cpu PORT MAP(
    rst => rst, clk => clk,

    -- Instruction memory bus
    inst_width => inst_width, inst_addr => inst_addr, inst_rdata => inst_rdata, inst_re => inst_re, inst_rdy => inst_rdy,

    data_width => mem_width, data_addr => mem_addr, data_wdata => mem_wdata,
    data_rdata => mem_rdata, data_re => mem_re, data_we => mem_we, data_rdy => mem_rdy, data_wack => mem_wack,

    -- Register file
    registerfile_rs1 => registerfile_rs1, registerfile_rs2 => registerfile_rs2, registerfile_rd => registerfile_rd,
    registerfile_wdata_rd => registerfile_wdata_rd,
    registerfile_rdata_rs1 => registerfile_rdata_rs1, registerfile_rdata_rs2 => registerfile_rdata_rs2,
    registerfile_we => registerfile_we
  );
  regfile : registerfile PORT MAP(
    clk => clk,
    rs1 => registerfile_rs1, rs2 => registerfile_rs2, rd => registerfile_rd,
    data_out_rs1 => registerfile_rdata_rs1, data_out_rs2 => registerfile_rdata_rs2,
    data_in_rd => registerfile_wdata_rd,
    we => registerfile_we
  );
  -- comp_hdmi : hdmi PORT MAP(

  --   rst => rst, clk => clk,
  --   mem_addr => mem_addr, mem_wdata => mem_wdata,
  --   mem_rdata => i_mem_rdata(PERIPHERAL_HDMI),
  --   mem_we => mem_we, mem_re => mem_re,
  --   mem_width => mem_width,
  --   mem_rdy => i_mem_rdy(PERIPHERAL_HDMI), mem_wack => i_mem_wack(PERIPHERAL_HDMI),

  --   address_valid => i_address_valid(PERIPHERAL_HDMI),

  --   gpdi_dp => gpdi_dp
  --   --gpdi_dn : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
  -- );

  PROCESS (i_address_valid, i_mem_rdata, i_mem_rdy, i_mem_wack)
  BEGIN
    mem_rdata <= (OTHERS => '0');
    mem_rdy <= '0';
    mem_wack <= '0';
    FOR i IN PERIPHERAL_MAX - 1 DOWNTO 0 LOOP
      IF i_address_valid(i) = '1' THEN
        mem_rdata <= i_mem_rdata(i);
        mem_rdy <= i_mem_rdy(i);
        mem_wack <= i_mem_wack(i);
      END IF;
    END LOOP;
  END PROCESS;

END behavioural;