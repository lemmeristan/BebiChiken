library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;

entity spimaster is
    generic (base_address : std_logic_vector(31 downto 0) := X"C0003000"
                );
    Port ( rst, clk : in std_logic;
           sck, mosi : out std_logic;
           miso : in std_logic;
           mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
           mem_rdata : out std_logic_vector(31 downto 0);
           mem_we, mem_re : in std_logic;
           mem_wack, mem_rdy : out std_logic;
           address_valid : out std_logic
           );
end spimaster;

architecture Behavioral of spimaster is
constant ADDR_SPI_DATA : std_logic_vector(31 downto 0) := base_address + X"00000000";
constant ADDR_SPI_DIVIDER : std_logic_vector(31 downto 0) := base_address + X"00000001";
constant ADDR_SPI_CONFIG : std_logic_vector(31 downto 0) := base_address + X"00000002";
constant ADDR_SPI_DATA_WAIT_MISO_LOW : std_logic_vector(31 downto 0) := base_address + X"00000003";

constant FLAG_OPEN_DRAIN_SCK : integer := 0;
constant FLAG_OPEN_DRAIN_MOSI : integer := 1;
constant FLAG_IDLE_SCK : integer := 2;
constant FLAG_IDLE_MOSI : integer := 3;


signal counter, spi_config, n_spi_config, spi_divider, n_spi_divider, spi_data, n_spi_data : std_logic_vector(7 downto 0);
signal state, n_state : integer range 0 to 31;

signal i_mosi, i_sck, enable_counter, reset_counter : std_logic;


-- component ila_1 PORT (
--     clk : in std_logic;
--     probe0, probe1, probe2, probe3, probe4, probe5, probe6, probe7 : in std_logic
--   );
--   end component;

begin

    process(i_mosi, i_sck, spi_config)
    begin
        if i_mosi = '1' then
            if spi_config(FLAG_OPEN_DRAIN_MOSI) = '1' then
                mosi <= 'Z';
            else
                mosi <= '1';
            end if;
        else
            mosi <= '0';
        end if;

        if i_sck = '1' then
            if spi_config(FLAG_OPEN_DRAIN_SCK) = '1' then
                sck <= 'Z';
            else
                sck <= '1';
            end if;
        else
            sck <= '0';
        end if;

    end process;

    process(rst, clk)
    begin
        if rst = '1' then
            counter <= X"00";
            spi_divider <= X"00";
            spi_config <= X"00";
            spi_data <= X"00";
            state <= 0;
        elsif rising_edge(clk) then
            state <= n_state;
            spi_divider <= n_spi_divider;
            spi_config <= n_spi_config;
            spi_data <= n_spi_data;

            if reset_counter = '1' then
                counter <= spi_divider;
            elsif enable_counter = '1' then
                if counter = X"00" then
                    counter <= spi_divider;
                else
                    counter <= counter - X"01";
                end if;
            end if;
        end if;
    end process;


    process(state, mem_we, mem_re, mem_addr, mem_wdata, counter, miso, spi_divider, spi_config, spi_data)
    begin

        reset_counter <= '0';
        enable_counter <= '1';

        
        mem_wack <= '0';
        mem_rdy <= '0';
        n_state <= state;
        mem_rdata <= (others => 'Z');

        n_spi_divider <= spi_divider;
        n_spi_config <= spi_config;
        n_spi_data <= spi_data;
        i_sck <= spi_config(FLAG_IDLE_SCK);
        i_mosi <= spi_config(FLAG_IDLE_MOSI);
        address_valid <= '1';

        case state is
            when 0 =>
                reset_counter <= '1';
                enable_counter <= '0';
                case mem_addr is 
                    when ADDR_SPI_DATA =>
                        if ((mem_we = '1') or (mem_re = '1')) then
                            mem_wack <= '0';
                            mem_rdy <= '0';
                            n_state <= 1;
                            n_spi_data <= mem_wdata(7 downto 0);
                        end if;
                    when ADDR_SPI_DIVIDER =>
                        mem_rdata <= X"000000" & spi_divider;
                        mem_wack <= '1';
                        mem_rdy <= '1';
                        if mem_we = '1' then
                            n_spi_divider <= mem_wdata(7 downto 0);
                        end if;
                    when ADDR_SPI_CONFIG =>
                        mem_rdata <= X"000000" & spi_config;
                        mem_wack <= '1';
                        mem_rdy <= '1';
                        if mem_we = '1' then
                            n_spi_config <= mem_wdata(7 downto 0);
                        end if;

                    when ADDR_SPI_DATA_WAIT_MISO_LOW =>
                        if ((mem_we = '1') or (mem_re = '1')) then
                            mem_wack <= '0';
                            mem_rdy <= '0';
                            n_state <= 18;
                            n_spi_data <= mem_wdata(7 downto 0);
                        end if;

                    when others =>
                        mem_wack <= 'Z';
                        mem_rdy <= 'Z';
                        address_valid <= '0';
                end case;

            when 1 | 3 | 5 | 7 | 9 | 11 | 13 | 15 =>
                i_mosi <= spi_data(7);
                i_sck <= '0';
                enable_counter <= '1';
                if counter = X"00" then
                    n_state <= state + 1;
                end if;
            when 2 | 4 | 6 | 8 | 10 | 12 | 14 | 16 =>
                i_mosi <= spi_data(7);    
                i_sck <= '1';
                enable_counter <= '1';
                if counter = X"00" then
                    n_state <= state + 1;
                    n_spi_data <= spi_data(6 downto 0) & miso;
                end if;
            when 17 =>
                reset_counter <= '1';
                enable_counter <= '0';

                mem_rdata <= X"000000" & spi_data;
                mem_wack <= '1';
                mem_rdy <= '1';
                if (mem_re = '0') and (mem_we = '0') then
                    n_state <= 0;
                end if;
            
            when 18 =>
                i_mosi <= spi_config(FLAG_IDLE_MOSI);
                i_sck <= '0';
                enable_counter <= '1';
                if counter = X"00" then
                    n_state <= state + 1;
                end if;
            when 19 =>
                i_mosi <= spi_config(FLAG_IDLE_MOSI);    
                i_sck <= '1';
                enable_counter <= '1';
                if counter = X"00" then
                    if miso = '0' then
                        n_state <= 3;
                        n_spi_data <= spi_data(6 downto 0) & miso;
                    else
                        n_state <= 18;
                    end if;
                end if;
            when others =>
                n_state <= 0;
        end case;
    end process;

    -- ila: ila_1 PORT MAP(
    --     clk => clk,
    --     probe0 => '0',
    --     probe1 => i_sck,
    --     probe2 => i_mosi,
    --     probe3 => miso,
    --     probe4 => '0',
    --     probe5 => '0',
    --     probe6 => '0',
    --     probe7 => '0'
    --   );

            
end Behavioral;
