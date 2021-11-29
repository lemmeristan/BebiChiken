LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY uart IS
    GENERIC (
        base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"C0001000";
        baud_rate : INTEGER := 115200;
        clk_freq : INTEGER := 100000000;
        initial_data : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"21"
    );
    PORT (
        rst, clk : IN STD_LOGIC;
        txd : OUT STD_LOGIC;
        mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        mem_we, mem_re : IN STD_LOGIC;
        mem_wack, mem_rdy : OUT STD_LOGIC;

        address_valid : OUT STD_LOGIC
    );
END uart;

ARCHITECTURE Behavioral OF uart IS
    CONSTANT COUNTER_MAX : INTEGER := (clk_freq/baud_rate) - 1;
    CONSTANT TX_BUSY : INTEGER := 0;

    SIGNAL counter : INTEGER := COUNTER_MAX; -- 115207 bps
    SIGNAL charToBeSent : STD_LOGIC_VECTOR(7 DOWNTO 0) := initial_data;
    SIGNAL reg_status : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL tick : STD_LOGIC;

    SIGNAL state, n_state : INTEGER RANGE 0 TO 15;

BEGIN

    tick <= '1' WHEN counter = 0 ELSE
        '0';

    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            counter <= COUNTER_MAX;
            charToBeSent <= initial_data;
            state <= 15;
        ELSIF rising_edge(clk) THEN

            IF counter > COUNTER_MAX THEN
                counter <= COUNTER_MAX;
            ELSIF counter = 0 THEN
                state <= n_state;
                counter <= COUNTER_MAX;
            ELSE
                counter <= counter - 1;
            END IF;

            IF (mem_we = '1') AND (mem_addr = (base_address + X"00000001")) THEN
                charToBeSent <= mem_wdata(7 DOWNTO 0);
                state <= 0;
                counter <= COUNTER_MAX;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (state, charToBeSent)
    BEGIN
        n_state <= state + 1;
        reg_status(TX_BUSY) <= '1';

        CASE state IS
            WHEN 0 =>
                txd <= '0';
            WHEN 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 =>
                txd <= charToBeSent(state - 1);
            WHEN 9 | 10 | 11 | 12 | 13 | 14 =>
                txd <= '1';
            WHEN OTHERS =>
                txd <= '1';
                n_state <= state;
                reg_status(TX_BUSY) <= '0';
        END CASE;
    END PROCESS;

    PROCESS (mem_addr, reg_status, charToBeSent)
    BEGIN
        mem_rdy <= '1';
        mem_wack <= '1';
        address_valid <= '1';
        IF mem_addr = base_address THEN -- status
            mem_rdata <= X"000000" & reg_status;
        ELSIF mem_addr = base_address + X"00000001" THEN
            mem_rdata <= X"000000" & charToBeSent;
        ELSE
            mem_rdy <= 'Z';
            mem_wack <= 'Z';
            mem_rdata <= (OTHERS => 'Z');
            address_valid <= '0';
        END IF;
    END PROCESS;

END Behavioral;