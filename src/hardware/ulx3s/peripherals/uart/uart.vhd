LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY uart IS
    GENERIC (
        base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"C0001000";
        baud_rate : INTEGER := 115200;
        clk_freq : INTEGER := 25000000;
        initial_data : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"21"
    );
    PORT (
        rst, clk : IN STD_LOGIC;
        txd : OUT STD_LOGIC;
        mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        mem_we, mem_re : IN STD_LOGIC;
        mem_wack, mem_rdy : OUT STD_LOGIC
    );
END uart;

ARCHITECTURE Behavioral OF uart IS
    CONSTANT COUNTER_MAX : INTEGER := (clk_freq/baud_rate) - 1;
    CONSTANT TX_BUSY : INTEGER := 0;

    SIGNAL counter : INTEGER; -- 115207 bps
    SIGNAL charToBeSent, n_charToBeSent : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL reg_status : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL tick : STD_LOGIC;

    SIGNAL state, n_state : INTEGER RANGE 0 TO 15;

    SIGNAL do_start : STD_LOGIC;

BEGIN

    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            counter <= COUNTER_MAX; -- counter counts down one bit length
            state <= 15; -- RS232 transmission finite state machine (11 states: start, b0..b7, stop)
            charToBeSent <= initial_data;
            mem_wack <= '0';
        ELSIF rising_edge(clk) THEN
            mem_wack <= '0';

            IF counter = 0 THEN
                counter <= COUNTER_MAX; -- reset counter
                state <= n_state; -- assign next state

                IF (state = 15) AND (mem_we = '1') THEN --AND (mem_addr = X"C0001000")  THEN
                    chartobesent <= mem_wdata(7 DOWNTO 0);
                    state <= 0;
                    mem_wack <= '1';
                END IF;
            ELSE
                counter <= counter - 1; -- continue counting down
            END IF;
        END IF;
    END PROCESS;
    PROCESS (state, do_start, charToBeSent, mem_we, mem_wdata)
    BEGIN
        n_state <= state + 1;
        reg_status <= (TX_BUSY => '1', OTHERS => '0');
        txd <= '1';

        CASE state IS
            WHEN 0 =>
                txd <= '0'; -- start bit
            WHEN 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 =>
                txd <= chartobesent(state - 1);
            WHEN 15 =>
                txd <= '1'; -- stop bit
                n_state <= state;
            WHEN OTHERS =>
        END CASE;
    END PROCESS;

    --    do_start <= '1';

    -- PROCESS (mem_addr, reg_status, charToBeSent)
    -- BEGIN
    --     mem_rdy <= '1';
    --     mem_wack <= '1';
    --     address_valid <= '1';
    --     IF mem_addr = base_address THEN -- status
    --         mem_rdata <= X"000000" & reg_status;
    --     ELSIF mem_addr = base_address + X"00000001" THEN
    --         mem_rdata <= X"000000" & charToBeSent;
    --     ELSE
    --         mem_rdy <= 'Z';
    --         mem_wack <= 'Z';
    --         mem_rdata <= (OTHERS => 'Z');
    --         address_valid <= '0';
    --     END IF;
    -- END PROCESS;

END Behavioral;