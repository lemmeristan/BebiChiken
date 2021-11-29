library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;

entity uart is
    generic (base_address : std_logic_vector(31 downto 0) := X"C0001000";
            baud_rate : integer := 115200;
            clk_freq : integer := 100000000;
            initial_data : std_logic_vector(7 downto 0) := X"21"
            );
    Port ( rst, clk : in STD_LOGIC;
           txd : out STD_LOGIC;
           mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
           mem_rdata : out std_logic_vector(31 downto 0);
           mem_we, mem_re : in std_logic;
           mem_wack, mem_rdy : out std_logic;

           address_valid : out std_logic
           );
end uart;

architecture Behavioral of uart is
constant COUNTER_MAX : integer := (clk_freq/baud_rate)-1;
constant TX_BUSY : integer := 0;

signal counter : integer := COUNTER_MAX; -- 115207 bps
signal charToBeSent : std_logic_vector(7 downto 0) := initial_data;
signal reg_status : std_logic_vector(7 downto 0);
signal tick : std_logic;

signal state, n_state : integer range 0 to 15;

begin

    tick <= '1' when counter = 0 else '0';

    process(rst, clk)
    begin
        if rst = '1' then
            counter <= COUNTER_MAX;
            charToBeSent <= initial_data;
            state <= 15;
        elsif rising_edge(clk) then

            if counter > COUNTER_MAX then
                counter <= COUNTER_MAX;
            elsif counter = 0 then
                state <= n_state;
                counter <= COUNTER_MAX;
            else
                counter <= counter - 1;
            end if;

            if (mem_we = '1') and (mem_addr = (base_address+X"00000001")) then
                charToBeSent <= mem_wdata(7 downto 0);
                state <= 0;
                counter <= COUNTER_MAX;
            end if;
        end if;
    end process;



    process(state, charToBeSent)
    begin
        n_state <= state + 1;
        reg_status(TX_BUSY) <= '1';
        
        case state is
            when 0 =>
                txd <= '0';
            when 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 =>
                txd <= charToBeSent(state-1);
            when 9 | 10 | 11 | 12 | 13 | 14 =>
                txd <= '1';
            when others =>
                txd <= '1';
                n_state <= state;
                reg_status(TX_BUSY) <= '0';
        end case;
    end process;

    process(mem_addr, reg_status, charToBeSent)
    begin
        mem_rdy <= '1';
        mem_wack <= '1';
        address_valid <= '1';
        case mem_addr is
            when base_address => -- status
                mem_rdata <= X"000000" & reg_status;
            when base_address + X"00000001" =>
                mem_rdata <= X"000000" & charToBeSent;
            when others =>
                mem_rdy <= 'Z';
                mem_wack <= 'Z';
                mem_rdata <= (others => 'Z');
                address_valid <= '0';
        end case;
    end process;

end Behavioral;
