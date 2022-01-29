library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;

entity i2cmaster is
    generic (base_address : std_logic_vector(31 downto 0) := X"C0004000"
                );
    Port ( rst, clk : in std_logic;
           scl, sda : inout std_logic;
           
           mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
           mem_rdata : out std_logic_vector(31 downto 0);
           mem_we, mem_re : in std_logic;
           mem_wack, mem_rdy : out std_logic;
           address_valid : out std_logic
           );
end i2cmaster;

architecture Behavioral of i2cmaster is
constant ADDR_I2C_DATA : std_logic_vector(31 downto 0) := base_address + X"00000000";
constant ADDR_I2C_ACK : std_logic_vector(31 downto 0) := base_address + X"00000001";
constant ADDR_I2C_PINS : std_logic_vector(31 downto 0) := base_address + X"00000002";

constant ADDR_I2C_DIVIDER : std_logic_vector(31 downto 0) := base_address + X"00000004";

constant PIN_SCL : integer := 0;
constant PIN_SDA : integer := 1;


signal counter, clk_divider, n_clk_divider : std_logic_vector(15 downto 0);
signal i2c_data, n_i2c_data : std_logic_vector(7 downto 0);
signal state, n_state : integer range 0 to 31;

signal i_sda, n_sda, i_scl, n_scl, enable_counter, reset_counter : std_logic;

signal i_mem_rdata : std_logic_vector(31 downto 0);

-- ILA

signal doing_ack : std_logic;

component ila_1 PORT (
    clk : in std_logic;
    probe0, probe1, probe2, probe3, probe4, probe5, probe6 : in std_logic --;
--    probe7 : in std_logic_vector(7 downto 0);
--    probe8 : in std_logic_vector(31 downto 0)
  );
  end component;

begin

    scl <= '0' when i_scl = '0' else 'Z';
    sda <= '0' when i_sda = '0' else 'Z';
    mem_rdata <= i_mem_rdata;

    process(rst, clk)
    begin
        if rst = '1' then
            counter <= X"0000";
            clk_divider <= X"0000";
            state <= 0;
            i_scl <= '1';
            i_sda <= '1';
            i2c_data <= X"00";
        elsif rising_edge(clk) then
            state <= n_state;
            clk_divider <= n_clk_divider;
            i_scl <= n_scl;
            i_sda <= n_sda;
            i2c_data <= n_i2c_data;

            if reset_counter = '1' then
                counter <= clk_divider;
            elsif enable_counter = '1' then
                if counter = X"0000" then
                    counter <= clk_divider;
                else
                    counter <= counter - X"0001";
                end if;
            end if;
        end if;
    end process;


    process(state, mem_we, mem_re, mem_addr, mem_wdata, counter, clk_divider, i2c_data, scl, sda, i_scl, i_sda)
    begin

        reset_counter <= '0';
        enable_counter <= '1';

        
        mem_wack <= '1';
        mem_rdy <= '1';
        n_state <= state;
        i_mem_rdata <= (others => '0');

        n_clk_divider <= clk_divider;
        n_i2c_data <= i2c_data;
        
        address_valid <= '1';

        n_scl <= i_scl;
        n_sda <= i_sda;

        doing_ack <= '0';

        case state is
            when 0 =>
                reset_counter <= '1';
                enable_counter <= '0';
                case mem_addr is 
                    when ADDR_I2C_DATA =>
                        if ((mem_we = '1') or (mem_re = '1')) then
                            mem_wack <= '0';
                            mem_rdy <= '0';
                            n_state <= 1;
                            n_i2c_data <= mem_wdata(7 downto 0);
                        end if;
                        
                    when ADDR_I2C_DIVIDER =>
                        i_mem_rdata <= X"0000" & clk_divider;
                        if mem_we = '1' then
                            n_clk_divider <= mem_wdata(15 downto 0);
                        end if;

                    when ADDR_I2C_ACK =>
                        if ((mem_we = '1') or (mem_re = '1')) then
                            mem_wack <= '0';
                            mem_rdy <= '0';
                            n_state <= 18;
                            n_scl <= '0';
                        end if;
                    
                    when ADDR_I2C_PINS =>
                        i_mem_rdata <= (PIN_SCL => scl, PIN_SDA => sda, others => '0');
                        if mem_we = '1' then
                            n_scl <= mem_wdata(PIN_SCL);
                            n_sda <= mem_wdata(PIN_SDA);
                        end if;

                    when others =>
                        mem_wack <= 'Z';
                        mem_rdy <= 'Z';
                        address_valid <= '0';
                end case;

-- ADDR_I2C_DATA
            when 1 | 3 | 5 | 7 | 9 | 11 | 13 | 15 =>
                mem_wack <= '0';
                mem_rdy <= '0';
                if mem_we = '1' then
                    n_sda <= i2c_data(7);
                else
                    n_sda <= '1';
                end if;

                n_scl <= '0';
                enable_counter <= '1';
                if counter = X"0000" then
                    n_state <= state + 1;
                    n_i2c_data <= i2c_data(6 downto 0) & sda;
                end if;
            when 2 | 4 | 6 | 8 | 10 | 12 | 14 | 16 =>
                mem_wack <= '0';
                mem_rdy <= '0';
                n_scl <= '1';

                enable_counter <= '1';
                if counter = X"0000" then
                    n_state <= state + 1;
                end if;
            when 17 =>
                reset_counter <= '1';
                enable_counter <= '0';

                i_mem_rdata <= X"000000" & i2c_data;
                if (mem_re = '0') and (mem_we = '0') then
                    n_state <= 0;
                end if;
-- ADDR_I2C_ACK
            when 18 =>
                mem_rdy <= '0';
                mem_wack <= '0';

                doing_ack <= '1';

                if mem_we = '1' then
                    n_sda <= mem_wdata(0);
                else
                    n_sda <= '1';
                end if;

                n_scl <= '0';
                enable_counter <= '1';
                if counter = X"0000" then
                    n_state <= state + 1;
                end if;
            when 19 =>
            mem_rdy <= '0';
            mem_wack <= '0';
                doing_ack <= '1';


                n_scl <= '1';

                enable_counter <= '1';
                if counter = X"0000" then
                    n_state <= state + 1;
                end if;

            when 20 =>
                doing_ack <= '1';


                reset_counter <= '1';
                enable_counter <= '0';

                i_mem_rdata <= (0 => sda, others => '0');
                if (mem_re = '0') and (mem_we = '0') then
                    n_state <= 0;
                end if;

            when others =>
                n_state <= 0;
        end case;
    end process;

--     ila: ila_1 PORT MAP(
--         clk => clk,
--         probe0 => doing_ack,
--         probe1 => i_scl,
--         probe2 => i_sda,
--         probe3 => scl,
--         probe4 => sda,
--         probe5 => n_scl,
--         probe6 => n_sda --,
-- --        probe7 => i2c_data,
-- --        probe8 => i_mem_rdata
--       );

            
end Behavioral;
