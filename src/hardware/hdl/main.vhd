library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.std_logic_unsigned.all;

entity main is
  Port (
    --rst, clk : in std_logic;

    CLK100MHZ : in std_logic;
    btn : in std_logic_vector(3 downto 0);
    led : out std_logic_vector(3 downto 0);
    uart_rxd_out : out std_logic;

    --SD_DAT3, SD_CMD, SD_CLK : out std_logic;
    --SD_DAT0 : in std_logic := '0'

    OLED_CS, OLED_MOSI, OLED_SCK, OLED_DC, OLED_RES, OLED_VCCEN, OLED_PMODEN : out std_logic;

    ck_scl, ck_sda : inout std_logic

  );
end main;


architecture behavioural of main is

      component registerFile PORT (
        clk : in std_logic;
        rs1, rs2, rd : in std_logic_vector(4 downto 0);
        data_out_rs1, data_out_rs2 : out std_logic_vector(31 downto 0);
        data_in_rd : in std_logic_vector(31 downto 0);
        we : in std_logic
      );
      end component;


      component cpu PORT (
        rst, clk : in std_logic;

        -- Instruction memory bus
        inst_width : out std_logic_vector(1 downto 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
        inst_addr : out std_logic_vector(31 downto 0);
        inst_rdata : in std_logic_vector(31 downto 0);
        inst_re : out std_logic;
        inst_rdy : in std_logic;
    
        -- Data memory bus
        data_width : out std_logic_vector(1 downto 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
        data_addr, data_wdata : out std_logic_vector(31 downto 0);
        data_rdata : in std_logic_vector(31 downto 0);
        data_re, data_we : out std_logic;
        data_rdy, data_wack : in std_logic;
    
        -- Register file
        registerfile_rs1, registerfile_rs2, registerfile_rd : out std_logic_vector(4 downto 0);
        registerfile_wdata_rd : out std_logic_vector(31 downto 0);
        registerfile_rdata_rs1, registerfile_rdata_rs2 : in std_logic_vector(31 downto 0);
        registerfile_we : out std_logic;
    
        err : out std_logic
      );
      end component;

      component block_ram PORT ( 
        rst, clk : in std_logic;
        mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
        mem_rdata : out std_logic_vector(31 downto 0);
        mem_we, mem_re : in std_logic;
        mem_width : in std_logic_vector(1 downto 0);
        mem_rdy, mem_wack : out std_logic;
        address_valid : out std_logic
    );
    end component;

    component uart PORT ( rst, clk : in STD_LOGIC;
    txd : out STD_LOGIC;
    mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
    mem_rdata : out std_logic_vector(31 downto 0);
    mem_we, mem_re : in std_logic;
    mem_wack, mem_rdy : out std_logic;
    address_valid : out std_logic
    );
    end component;

    component gpio PORT (
      rst, clk : in std_logic;
      mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
      mem_rdata : out std_logic_vector(31 downto 0);
      mem_we, mem_re : in std_logic;
      mem_wack, mem_rdy : out std_logic;
  
      gpio : inout std_logic_vector(31 downto 0);
      address_valid : out std_logic
    );
    end component; 

    component timebase PORT (
        rst, clk : in std_logic;
        mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
        mem_rdata : out std_logic_vector(31 downto 0);
        mem_we, mem_re : in std_logic;
        mem_wack, mem_rdy : out std_logic;
        address_valid : out std_logic
      );
    end component;


    component rom PORT (
      rst, clk : in std_logic;
      addr : in std_logic_vector(31 downto 0);
      data_out : out std_logic_vector(31 downto 0)
    );
    end component;


      component spimaster PORT ( 
        rst, clk : in std_logic;
        sck, mosi : out std_logic;
        miso : in std_logic;
        mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
        mem_rdata : out std_logic_vector(31 downto 0);
        mem_we, mem_re : in std_logic;
        mem_wack, mem_rdy : out std_logic;
        address_valid : out std_logic
    );
    end component;

    component i2cmaster PORT ( 
      rst, clk : in std_logic;
      scl, sda : inout std_logic;
      mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
      mem_rdata : out std_logic_vector(31 downto 0);
      mem_we, mem_re : in std_logic;
      mem_wack, mem_rdy : out std_logic;
      address_valid : out std_logic
  );
  end component;
    
    signal miso, mosi, sck : std_logic;
    
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

signal registerfile_rs1, registerfile_rs2, registerfile_rd : std_logic_vector(4 downto 0);
signal registerfile_rdata_rs1, registerfile_rdata_rs2, registerfile_wdata_rd : std_logic_vector(31 downto 0);
signal registerfile_we : std_logic;

signal we_0, data_we, data_re, inst_re, inst_rdy, data_rdy, data_wack : std_logic;

signal data_wdata, data_addr, inst_addr, inst_rdata, data_rdata : std_logic_vector(31 downto 0);

signal inst_width, mem_width : std_logic_vector(1 downto 0);

signal rst, clk : std_logic;

signal mem_addr, mem_wdata, mem_rdata, int_gpio : std_logic_vector(31 downto 0);
signal mem_rdy, mem_wack, mem_we, mem_re : std_logic;

---------------------------------------------------------------------------
-- Peripherals
---------------------------------------------------------------------------

constant PERIPHERAL_RAM : integer := 0;
constant PERIPHERAL_TIMEBASE : integer := 3;
constant PERIPHERAL_UART : integer := 4;
constant PERIPHERAL_GPIO : integer := 5;
constant PERIPHERAL_SPIMASTER : integer := 6;
constant PERIPHERAL_I2CMASTER : integer := 7;

constant PERIPHERAL_MAX : integer := 8;

signal i_mem_rdy, i_mem_wack, i_address_valid : std_logic_vector(PERIPHERAL_MAX-1 downto 0);
type mem_rdata_t is array (natural range <>) of std_logic_vector(31 downto 0);
type addr_t is array (natural range <>) of std_logic_vector(19 downto 0);

signal i_mem_rdata : mem_rdata_t(PERIPHERAL_MAX-1 downto 0) := (others => (others => '0'));



--- ILA

component ila_1 PORT (
    clk : in std_logic;
    probe0, probe1, probe2, probe3, probe4, probe5, probe6 : in std_logic
  );
  end component;

begin


  --clk <= CLK100MHZ;
  rst <= btn(0) or btn(1) or btn(2) or btn(3);

  inst_rdy <= '1';

    i_gpio : gpio PORT MAP (
    rst => rst, clk => clk,
    mem_addr => mem_addr, mem_wdata => mem_wdata,
    mem_rdata => i_mem_rdata(PERIPHERAL_GPIO),
     mem_we => mem_we, mem_re => mem_re,
     mem_wack => i_mem_wack(PERIPHERAL_GPIO), mem_rdy => i_mem_rdy(PERIPHERAL_GPIO),
    address_valid => i_address_valid(PERIPHERAL_GPIO),
    gpio => int_gpio
    );

    i_uart: uart PORT MAP (
      rst => rst, clk => clk,
      txd => uart_rxd_out,
      mem_addr => mem_addr, mem_wdata => mem_wdata,
      mem_rdata => i_mem_rdata(PERIPHERAL_UART),
      mem_we => mem_we, mem_re => mem_re,
      mem_wack => i_mem_wack(PERIPHERAL_UART), 
      mem_rdy => i_mem_rdy(PERIPHERAL_UART),
      address_valid => i_address_valid(PERIPHERAL_UART)
    );

    i_timebase: timebase PORT MAP (
      rst => rst, clk => clk,
      mem_addr => mem_addr, mem_wdata => mem_wdata,
      mem_rdata => i_mem_rdata(PERIPHERAL_TIMEBASE),
      mem_we => mem_we, mem_re => mem_re,
      mem_wack => i_mem_wack(PERIPHERAL_TIMEBASE), 
      mem_rdy => i_mem_rdy(PERIPHERAL_TIMEBASE),
      address_valid => i_address_valid(PERIPHERAL_TIMEBASE)
    );

    i_ram: block_ram PORT MAP(
      rst => rst, clk => clk,
      mem_addr => mem_addr, mem_wdata => mem_wdata,
      mem_width => mem_width,
      mem_rdata => i_mem_rdata(PERIPHERAL_RAM),
      mem_we => mem_we, mem_re => mem_re,
      mem_wack => i_mem_wack(PERIPHERAL_RAM), 
      mem_rdy => i_mem_rdy(PERIPHERAL_RAM),
      address_valid => i_address_valid(PERIPHERAL_RAM)
    );

    i_spimaster: spimaster PORT MAP (
        rst => rst, clk => clk,
        sck => sck, mosi => mosi, miso => miso,
        mem_addr => mem_addr, mem_wdata => mem_wdata,
        mem_rdata => i_mem_rdata(PERIPHERAL_SPIMASTER),
        mem_we => mem_we, mem_re => mem_re,
        mem_rdy => i_mem_rdy(PERIPHERAL_SPIMASTER), mem_wack => i_mem_wack(PERIPHERAL_SPIMASTER),
        address_valid => i_address_valid(PERIPHERAL_SPIMASTER)
    );

    i_i2cmaster: i2cmaster PORT MAP (
      rst => rst, clk => clk,
      scl => ck_scl, sda => ck_sda,
      mem_addr => mem_addr, mem_wdata => mem_wdata,
      mem_rdata => i_mem_rdata(PERIPHERAL_I2CMASTER),
      mem_we => mem_we, mem_re => mem_re,
      mem_rdy => i_mem_rdy(PERIPHERAL_I2CMASTER), mem_wack => i_mem_wack(PERIPHERAL_I2CMASTER),
      address_valid => i_address_valid(PERIPHERAL_I2CMASTER)
  );

    i_rom: rom PORT MAP( 
          rst => rst, clk => clk,
          addr => inst_addr,
          data_out => inst_rdata
      );

    i_cpu: cpu PORT MAP (
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


    regfile: registerFile PORT MAP(
        clk => clk,
        rs1 => registerfile_rs1, rs2 => registerfile_rs2, rd => registerfile_rd,
        data_out_rs1 => registerfile_rdata_rs1, data_out_rs2 => registerfile_rdata_rs2,
        data_in_rd => registerfile_wdata_rd,
        we => registerfile_we
      );

      clk <= CLK100MHZ;

led(3 downto 0) <= int_gpio(3 downto 0);

process(i_address_valid, i_mem_rdata, i_mem_rdy, i_mem_wack)
begin
  mem_rdata <= (others => '0');
  mem_rdy <= '0';
  mem_wack <= '0';
  for i in PERIPHERAL_MAX-1 downto 0 loop
    if i_address_valid(i) = '1' then
      mem_rdata <= i_mem_rdata(i);
      mem_rdy <= i_mem_rdy(i);
      mem_wack <= i_mem_wack(i);
    end if;
  end loop;
end process;


--SD_DAT3 <= int_gpio(0);
--SD_CMD <= mosi;
--miso <= SD_DAT0;
--SD_CLK <= sck;


    -- ila: ila_1 PORT MAP(
    --     clk => clk,
    --     probe0 => int_gpio(0), -- OLED_CS
    --     probe1 => sck,
    --     probe2 => mosi,
    --     probe3 => int_gpio(1),
    --     probe4 => int_gpio(2),
    --     probe5 => int_gpio(3),
    --     probe6 => int_gpio(4)
        
    --   );

OLED_CS <= int_gpio(0);
OLED_MOSI <= mosi;
OLED_SCK <= sck;
OLED_DC <= int_gpio(1);
OLED_RES <= int_gpio(2);
OLED_VCCEN <= int_gpio(3);
OLED_PMODEN <= int_gpio(4);

end behavioural;