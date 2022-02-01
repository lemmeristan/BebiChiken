LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

LIBRARY work;
USE work.bebichiken.ALL;

ENTITY top IS
  PORT (
    --rst, clk : in std_logic;

    clk_25mhz : IN STD_LOGIC;
    btn : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
    led : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    ftdi_rxd : OUT STD_LOGIC;
    wifi_gpio0 : OUT STD_LOGIC;

    flash_csn : OUT STD_LOGIC;
    flash_mosi : INOUT STD_LOGIC; -- io(0)
    flash_miso : IN STD_LOGIC; -- io(1)
    flash_wpn : INOUT STD_LOGIC; -- io(2)
    flash_holdn : INOUT STD_LOGIC; -- io(3)

  -- SDRAM
    sdram_a : OUT STD_LOGIC_VECTOR(12 DOWNTO 0);
    sdram_ba : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    sdram_d : INOUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    sdram_cke : OUT STD_LOGIC;
    sdram_csn : OUT STD_LOGIC;
    sdram_rasn : OUT STD_LOGIC;
    sdram_casn : OUT STD_LOGIC;
    sdram_wen : OUT STD_LOGIC;
    sdram_dqm : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);

    -- HDMI
    gpdi_dp : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)


    --SD_DAT3, SD_CMD, SD_CLK : out std_logic;
    --SD_DAT0 : in std_logic := '0'

    --OLED_CS, OLED_MOSI, OLED_SCK, OLED_DC, OLED_RES, OLED_VCCEN, OLED_PMODEN : OUT STD_LOGIC;

    --ck_scl, ck_sda : INOUT STD_LOGIC

  );
END top;
ARCHITECTURE behavioural OF top IS






  CONSTANT num_hosts : INTEGER := 1;
  CONSTANT num_peripherals : INTEGER := 1;



  SIGNAL miso, mosi, sck : STD_LOGIC;



  ATTRIBUTE syn_keep : BOOLEAN;

  SIGNAL registerfile_rs1, registerfile_rs2, registerfile_rd : STD_LOGIC_VECTOR(4 DOWNTO 0);
  ATTRIBUTE syn_keep OF registerfile_rs1, registerfile_rs2, registerfile_rd : SIGNAL IS true;

  SIGNAL registerfile_rdata_rs1, registerfile_rdata_rs2, registerfile_wdata_rd : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL registerfile_we : STD_LOGIC;
  ATTRIBUTE syn_keep OF registerfile_rdata_rs1, registerfile_rdata_rs2, registerfile_wdata_rd, registerfile_we : SIGNAL IS true;
  SIGNAL we_0, data_we, data_re, inst_re, inst_rdy, i_inst_rdy, r_inst_rdy, data_rdy, data_wack, hasrdy : STD_LOGIC;
  ATTRIBUTE syn_keep OF we_0, data_we, data_re, inst_re, inst_rdy, i_inst_rdy, r_inst_rdy, data_rdy, data_wack, hasrdy : SIGNAL IS true;

  SIGNAL data_wdata, data_addr, inst_addr, inst_rdata, i_inst_rdata, data_rdata : STD_LOGIC_VECTOR(31 DOWNTO 0);
  ATTRIBUTE syn_keep OF data_wdata, data_addr, inst_addr, inst_rdata, i_inst_rdata, data_rdata : SIGNAL IS true;
  SIGNAL inst_width, mem_width : STD_LOGIC_VECTOR(1 DOWNTO 0);
  ATTRIBUTE syn_keep OF inst_width, mem_width : SIGNAL IS true;
  SIGNAL rst, clk : STD_LOGIC;
  ATTRIBUTE syn_keep OF rst, clk : SIGNAL IS true;
  SIGNAL int_gpio : STD_LOGIC_VECTOR(31 DOWNTO 0);

  SIGNAL mem_addr, mem_wdata, mem_rdata : STD_LOGIC_VECTOR(31 DOWNTO 0);
  ATTRIBUTE syn_keep OF mem_addr, mem_wdata, mem_rdata : SIGNAL IS true;

  SIGNAL mem_rdy, mem_wack, mem_we, mem_re : STD_LOGIC;
  ATTRIBUTE syn_keep OF mem_rdy, mem_wack, mem_we, mem_re : SIGNAL IS true;
  SIGNAL mem_width_uart : STD_LOGIC_VECTOR(1 DOWNTO 0);
  ATTRIBUTE syn_keep OF mem_width_uart : SIGNAL IS true;

  SIGNAL mem_addr_uart, mem_wdata_uart, mem_rdata_uart : STD_LOGIC_VECTOR(31 DOWNTO 0);
  ATTRIBUTE syn_keep OF mem_addr_uart, mem_wdata_uart, mem_rdata_uart : SIGNAL IS true;

  SIGNAL mem_rdy_uart, mem_wack_uart, mem_we_uart, mem_re_uart : STD_LOGIC;
  ATTRIBUTE syn_keep OF mem_rdy_uart, mem_wack_uart, mem_we_uart, mem_re_uart : SIGNAL IS true;
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

  SIGNAL flash_clk : STD_LOGIC;

  SIGNAL spi_csn, spi_clk, spi_di, spi_do, spi_wpn, spi_holdn, spi_reading : STD_LOGIC;

  SIGNAL spi_io : STD_LOGIC_VECTOR(3 DOWNTO 0);

  signal periph_we, periph_re, periph_wack, periph_rdy : peripheral_bit_t := (others => '1');
  signal periph_wdata, periph_rdata, periph_address : peripheral_word_t;
  signal periph_width : peripheral_width_t;


  -- SDRAM
  SIGNAL sdram_mem_clk, sdram_mem_we, sdram_mem_re, sdram_mem_rdy, sdram_mem_wack : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL sdram_mem_addr, n_sdram_mem_addr, sdram_mem_wdata, sdram_mem_rdata : word_array_t(1 DOWNTO 0);
  SIGNAL sdram_mem_width : width_array_t(1 DOWNTO 0);





  -- HDMI
  signal hdmi_mem_addr, hdmi_mem_wdata, hdmi_mem_rdata : std_logic_vector(31 downto 0);
  signal hdmi_mem_we, hdmi_mem_re, hdmi_mem_rdy, hdmi_mem_wack, pixclk, half_clk_TMDS : std_logic;
  signal hdmi_mem_width : std_logic_vector(1 downto 0);
  SIGNAL X, Y : STD_LOGIC_VECTOR(9 DOWNTO 0);
  SIGNAL current_pixel : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"0000F000";



  signal cpu_clk : std_logic;


BEGIN

  u1 : USRMCLK PORT MAP(
    USRMCLKI => spi_clk,
    USRMCLKTS => rst);

  wifi_gpio0 <= '1'; -- Tie GPIO0, keep board from rebooting

  --clk <= CLK100MHZ;
  rst <= (NOT btn(0)) OR btn(1) OR btn(2) OR btn(3) OR btn(4) OR btn(5) OR btn(6);

  --inst_rdy <= '1';

  -- i_gpio : gpio PORT MAP(
  --   rst => rst, clk => clk,
  --   mem_addr => mem_addr, mem_wdata => mem_wdata,
  --   mem_rdata => i_mem_rdata(PERIPHERAL_GPIO),
  --   mem_we => mem_we, mem_re => mem_re,
  --   mem_wack => i_mem_wack(PERIPHERAL_GPIO), mem_rdy => i_mem_rdy(PERIPHERAL_GPIO),
  --   address_valid => i_address_valid(PERIPHERAL_GPIO),
  --   gpio_dir => gpio_dir, gpio_value => gpio_value, gpio_input => (OTHERS => '0')
  -- );

  --led <= rst & int_gpio(6 DOWNTO 0);
  --led <= mem_addr(7 DOWNTO 0);
  --led <= inst_rdata(31 DOWNTO 24);
  -- PROCESS (gpio_dir, gpio_value)
  -- BEGIN
  --   FOR i IN 0 TO 31 LOOP
  --     IF gpio_dir(i) = '1' THEN -- set as output
  --       int_gpio(i) <= gpio_value(i);
  --     ELSE
  --       int_gpio(i) <= 'Z';
  --     END IF;
  --   END LOOP;
  -- END PROCESS;



  -- i_timebase : timebase PORT MAP(
  --   rst => rst, clk => clk,
  --   mem_addr => mem_addr, mem_wdata => mem_wdata,
  --   mem_rdata => i_mem_rdata(PERIPHERAL_TIMEBASE),
  --   mem_we => mem_we, mem_re => mem_re,
  --   mem_wack => i_mem_wack(PERIPHERAL_TIMEBASE),
  --   mem_rdy => i_mem_rdy(PERIPHERAL_TIMEBASE),
  --   address_valid => i_address_valid(PERIPHERAL_TIMEBASE)
  -- );

  -- i_ram : block_ram PORT MAP(
  --   rst => rst, clk => clk,
  --   mem_addr => mem_addr, mem_wdata => mem_wdata,
  --   mem_width => mem_width,
  --   mem_rdata => i_mem_rdata(PERIPHERAL_RAM),
  --   mem_we => mem_we, mem_re => mem_re,
  --   mem_wack => i_mem_wack(PERIPHERAL_RAM),
  --   mem_rdy => i_mem_rdy(PERIPHERAL_RAM),
  --   address_valid => i_address_valid(PERIPHERAL_RAM)
  -- );

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
  inst_re <= '1';


  --periph_addresses <= (786433 => PERIPH_UART, 851968 to 860160 => PERIPH_SDRAM, others => PERIPH_INVALID);

  
  i_quadflash_cache : quadflash_cache GENERIC MAP(
    vendor => '1',
    base_address => X"00000000"
    ) PORT MAP(
    reset => rst,
    clk => clk,

    mem_clk => clk,
    mem_re => inst_re,
    mem_addr => inst_addr,
    mem_rdata => i_inst_rdata,
    mem_rdy => i_inst_rdy,

    spi_csn => spi_csn, spi_sck => spi_clk,
    spi_di => spi_di, spi_do => spi_do, spi_wpn => spi_wpn, spi_holdn => spi_holdn,

    spi_io => spi_io, spi_reading => spi_reading --, led => led
  );
  --clk <= clk_25mhz;

  -- PROCESS (rst, clk_25mhz)
  -- BEGIN
  --   IF rst = '1' THEN
  --     clk <= '0';
  --   ELSIF rising_edge(clk_25mhz) THEN
  --     clk <= NOT clk;
  --   END IF;
  -- END PROCESS;

  PROCESS (rst, clk_25mhz)
  BEGIN
    IF rst = '1' THEN
      hasrdy <= '0';

      r_inst_rdy <= '0';
      inst_rdata <= (OTHERS => '0');
      led <= (others => '0');
    ELSIF rising_edge(clk_25mhz) THEN
      IF r_inst_rdy = '1' THEN
        hasrdy <= '1';
      END IF;
      led <= inst_addr(7 DOWNTO 0); --hasrdy & inst_re & inst_addr(5 DOWNTO 0); --"000000"; --


      r_inst_rdy <= i_inst_rdy;
      inst_rdata <= i_inst_rdata;
      --clk <= NOT clk;
    END IF;
  END PROCESS;

  clk <= clk_25mhz;

  inst_rdy <= (i_inst_rdy AND r_inst_rdy);
  spi_io <= flash_holdn & flash_wpn & flash_miso & flash_mosi;

  -- PROCESS (spi_reading, spi_holdn, spi_wpn, spi_di)
  -- BEGIN
  --   IF spi_reading = '1' THEN
  --     flash_mosi <= 'Z';
  --     flash_wpn <= 'Z';
  --     flash_holdn <= 'Z';
  --   ELSE
  flash_mosi <= spi_di;
  flash_wpn <= spi_wpn;
  flash_holdn <= spi_holdn;
  --   END IF;
  -- END PROCESS;

  flash_csn <= spi_csn;
  spi_do <= flash_miso;

  cpu_clk <= clk_25mhz;

  i_uart : uart PORT MAP(
    rst => rst, clk => clk,
    txd => ftdi_rxd,
    mem_addr => mem_addr, mem_wdata => mem_wdata,
    mem_rdata => i_mem_rdata(PERIPHERAL_UART),
    mem_we => mem_we, mem_re => mem_re,
    mem_wack => i_mem_wack(PERIPHERAL_UART),
    mem_rdy => i_mem_rdy(PERIPHERAL_UART)
  );

  mem_wack <= i_mem_wack(PERIPHERAL_UART);
  mem_rdy <= i_mem_rdy(PERIPHERAL_UART);


  i_cpu : cpu PORT MAP(
    rst => rst, clk => clk_25mhz,

    -- Instruction memory bus
    inst_width => inst_width, inst_addr => inst_addr, inst_rdata => inst_rdata,
    --inst_re => inst_re, 
    inst_rdy => inst_rdy,

    data_width => mem_width, data_addr => mem_addr, data_wdata => mem_wdata,
    data_rdata => mem_rdata, data_re => mem_re, data_we => mem_we, data_rdy => mem_rdy, data_wack => mem_wack

  );
  


  sdram_mem_addr <= (0 => mem_addr, 1 => hdmi_mem_addr);
  sdram_mem_wdata <= (0 => mem_wdata, 1 => hdmi_mem_wdata);


  sdram_mem_we <= (0 => periph_we(PERIPH_SDRAM), 1 => hdmi_mem_we);
  sdram_mem_re <= (0 => periph_re(PERIPH_SDRAM), 1 => hdmi_mem_re);
  sdram_mem_width <= (0 => periph_width(PERIPH_SDRAM), 1 => hdmi_mem_width);
  periph_wack(PERIPH_SDRAM) <= sdram_mem_wack(0);
  sdram_mem_clk <= (0 => clk, 1 => pixclk);


  process(clk)
  begin
    if rising_edge(clk) then
        --periph_rdata(PERIPH_SDRAM) <= sdram_mem_rdata(0);
        periph_rdy(PERIPH_SDRAM) <= sdram_mem_rdy(0);
    end if;
  end process;


  i_sdram_cache : sdram_cache

    GENERIC MAP(
        vendor => '1',
        num_ports => 2,
        clk_freq => 125,
        base_address => X"00000000"
    )

    PORT MAP(
        reset => rst, clk => pixclk, -- clk_25mhz,
        mem_addr => sdram_mem_addr, mem_wdata => sdram_mem_wdata,
        mem_rdata => sdram_mem_rdata,
        mem_we => sdram_mem_we, mem_re => sdram_mem_re,
        mem_width => sdram_mem_width,
        mem_rdy => sdram_mem_rdy, mem_wack => sdram_mem_wack,
        mem_clk => sdram_mem_clk,

        sdram_a => sdram_a,
        sdram_ba => sdram_ba,
        sdram_dq => sdram_d,
        sdram_cke => sdram_cke,
        sdram_cs_n => sdram_csn,
        sdram_ras_n => sdram_rasn,
        sdram_cas_n => sdram_casn,
        sdram_we_n => sdram_wen,
        sdram_dqml => sdram_dqm(0),
        sdram_dqmh => sdram_dqm(1) --,
        --addr_valid => addr_valid

    );


    i_hdmi : HDMI_test_hires PORT MAP(
      pclk => clk_25mhz,
      gpdi_dp => gpdi_dp,
      GFX_X => X,
      GFX_Y => Y,
      red => current_pixel(23 DOWNTO 16),
      green => current_pixel(15 DOWNTO 8),
      blue => current_pixel(7 DOWNTO 0),
      half_clk_TMDS => half_clk_TMDS,
      pixclk => pixclk);


      hdmi_mem_wack <= sdram_mem_wack(1);
      hdmi_mem_rdata <= sdram_mem_rdata(1);
      hdmi_mem_rdy <= sdram_mem_rdy(1);
      hdmi_mem_addr <= "00000" & X"0000" & X(8 DOWNTO 0) & "00";
      current_pixel <= hdmi_mem_rdata when hdmi_mem_rdy = '1' else X"00FF0000";
      

        hdmi_mem_re <= '1';
        hdmi_mem_width <= "10";
        hdmi_mem_we <= '0';



END behavioural;