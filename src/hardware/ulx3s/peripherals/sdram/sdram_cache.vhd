-- vsg_off
----------------------------------------------------------------------------------
-- Company:
-- Engineer: Lemmer EL ASSAL
--
-- Create Date: 11/04/2021 06:03:48 PM
-- Design Name: SDRAM Cache
-- Module Name: sdram_cache - behavioural
-- Project Name: BebiChiken
-- Target Devices:
-- Tool Versions:
-- Description:
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
----------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

USE IEEE.STD_LOGIC_UNSIGNED.ALL; -- add std_logic_vectors together

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
USE IEEE.NUMERIC_STD.ALL;

USE ieee.math_real.ALL; -- ceil

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

LIBRARY work;
USE work.bebichiken.ALL;

ENTITY sdram_cache IS
    GENERIC (
        vendor : STD_LOGIC := '0'; -- 0 => xilinx, 1 => lattice
        base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00000000";
        clk_freq : NATURAL := 100;
        CAS_LATENCY : NATURAL := 2; -- 2=below 133MHz, 3=above 133MHz

        -- timing values (in nanoseconds)
        --
        -- These values can be adjusted to match the exact timing of your SDRAM
        -- chip (refer to the datasheet).
        T_DESL : real := 1000000.0; -- startup delay
        T_MRD : real := 12.0; -- mode register cycle time
        T_RC : real := 60.0; -- row cycle time
        T_RCD : real := 18.0; -- RAS to CAS delay
        T_RP : real := 18.0; -- precharge to activate delay
        T_WR : real := 12.0; -- write recovery time
        T_REFI : real := 100000.0; --7800.0; -- average refresh interval

        num_ports : INTEGER := 1

    );

    PORT (
        reset : IN STD_LOGIC;
        clk : IN STD_LOGIC;

        sdram_a : OUT STD_LOGIC_VECTOR(12 DOWNTO 0);
        sdram_ba : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
        sdram_dq : INOUT STD_LOGIC_VECTOR(15 DOWNTO 0);
        sdram_cke : OUT STD_LOGIC;
        sdram_cs_n : OUT STD_LOGIC;
        sdram_ras_n : OUT STD_LOGIC;
        sdram_cas_n : OUT STD_LOGIC;
        sdram_we_n : OUT STD_LOGIC;
        sdram_dqml : OUT STD_LOGIC;
        sdram_dqmh : OUT STD_LOGIC;


        --cpu_clk, cpu_we, cpu_re : in std_logic;
        --cpu_addr, cpu_wdata : in std_logic_vector(31 downto 0);
        
        mem_clk : IN STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
        mem_we : IN STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
        mem_re : IN STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
        mem_addr : IN word_array_t(num_ports - 1 DOWNTO 0);
        mem_width : IN width_array_t(num_ports - 1 DOWNTO 0);
        mem_wdata : IN word_array_t(num_ports - 1 DOWNTO 0);
        mem_rdata : OUT word_array_t(num_ports - 1 DOWNTO 0);
        mem_rdy : OUT STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
        mem_wack : OUT STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
        addr_valid : OUT STD_LOGIC
    );
END sdram_cache;

ARCHITECTURE behavioural OF sdram_cache IS

    SUBTYPE command_t IS STD_LOGIC_VECTOR(3 DOWNTO 0);

    -- commands
    CONSTANT CMD_DESELECT : command_t := "1---";
    CONSTANT CMD_LOAD_MODE : command_t := "0000";
    CONSTANT CMD_AUTO_REFRESH : command_t := "0001";
    CONSTANT CMD_PRECHARGE : command_t := "0010";
    CONSTANT CMD_ACTIVE : command_t := "0011";
    CONSTANT CMD_WRITE : command_t := "0100";
    CONSTANT CMD_READ : command_t := "0101";
    CONSTANT CMD_STOP : command_t := "0110";
    CONSTANT CMD_NOP : command_t := "0111";

    -- calculate the clock period (in nanoseconds)
    CONSTANT CLK_PERIOD : real := (1.0/Real(clk_freq)) * 1000.0;

    -- the number of clock cycles to wait before initialising the device
    CONSTANT INIT_WAIT : NATURAL := 2500; -- NATURAL(ceil(T_DESL/CLK_PERIOD));

    -- the number of clock cycles to wait while a LOAD MODE command is being
    -- executed
    CONSTANT LOAD_MODE_WAIT : NATURAL := NATURAL(ceil(T_MRD/CLK_PERIOD));

    -- the number of clock cycles to wait while an ACTIVE command is being
    -- executed
    CONSTANT ACTIVE_WAIT : NATURAL := NATURAL(ceil(T_RCD/CLK_PERIOD));

    -- the number of clock cycles to wait while a REFRESH command is being
    -- executed
    CONSTANT REFRESH_WAIT : NATURAL := 2; -- NATURAL(ceil(T_RC/CLK_PERIOD));

    -- the number of clock cycles to wait while a PRECHARGE command is being
    -- executed
    CONSTANT PRECHARGE_WAIT : NATURAL := 1; --NATURAL(ceil(T_RP/CLK_PERIOD));

    -- the number of clock cycles before the memory controller needs to refresh
    -- the SDRAM
    CONSTANT REFRESH_INTERVAL : NATURAL := 8192; --NATURAL(floor(T_REFI/CLK_PERIOD)) - 10;
    TYPE state_t IS (INIT, IDLE, REFRESH, FILL_CACHE, FLUSH_CACHE);

    -- state signals
    SIGNAL state, n_state, return_state, n_return_state : state_t;

    -- command signals
    SIGNAL cmd : command_t := CMD_NOP;

    -- control signals
    SIGNAL refresh_done : STD_LOGIC;
    SIGNAL should_refresh : STD_LOGIC;

    -- counters
    SIGNAL wait_counter : NATURAL RANGE 0 TO 16383;
    SIGNAL refresh_counter : NATURAL RANGE 0 TO 16383;
    SIGNAL n_fill_count_valid, fill_count_valid, n_DPRAM_WE_SDRAM, DPRAM_WE_SDRAM : STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);

    SIGNAL reset_dirty, dirty, DPRAM_WE_CPU, update_current_address, inc_fill_count, reset_fill_count : STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);
    SIGNAL reset_wait_counter : STD_LOGIC;
    SIGNAL DPRAM_ADDR_SDRAM, n_DPRAM_ADDR_SDRAM, n_fill_count, fill_count : dpram_address_array_t(num_ports - 1 DOWNTO 0); --std_logic_vector(10 downto 0); -- change this
    SIGNAL DPRAM_DOUT_SDRAM, DPRAM_DIN_SDRAM, n_DPRAM_DIN_SDRAM, DPRAM_DOUT_CPU, DPRAM_DIN_CPU : word_array_t(num_ports - 1 DOWNTO 0);
    SIGNAL current_address : word_array_t(num_ports - 1 DOWNTO 0); -- := (OTHERS => (OTHERS => '1')); -- 24 downto 13

    SIGNAL row, n_row : STD_LOGIC_VECTOR(2 DOWNTO 0);
    SIGNAL address_valid, cache_miss, n_mem_wack, n_dirty : STD_LOGIC_VECTOR(num_ports - 1 DOWNTO 0);

    SIGNAL current_port_selection, n_current_port_selection : INTEGER RANGE 0 TO num_ports - 1; -- := 0;

    signal skiptostop : std_logic;

    signal mem_addr_r :  word_array_t(num_ports - 1 DOWNTO 0);


    -- dualport ram
    COMPONENT dpram1 PORT (

        DataInA, DataInB : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        AddressA, AddressB : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
        ClockA, ClockB, ClockEnA, ClockEnB, WrA, WrB, ResetA, ResetB : IN STD_LOGIC;
        QA, QB : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)

        );
    END COMPONENT;

    COMPONENT blk_mem_gen_0 IS
        PORT (
            clka : IN STD_LOGIC;
            wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
            addra : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
            dina : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            douta : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            clkb : IN STD_LOGIC;
            web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
            addrb : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
            dinb : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            doutb : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

BEGIN
    async : PROCESS (state, cache_miss, wait_counter, refresh_done, should_refresh, return_state, fill_count, fill_count_valid, row, mem_addr_r, mem_we, current_address, dirty, DPRAM_DOUT_SDRAM, sdram_dq, DPRAM_DIN_SDRAM, current_port_selection)
    BEGIN
        sdram_a <= (OTHERS => '0');
        sdram_ba <= (OTHERS => '0');
        sdram_dq <= (OTHERS => 'Z');

        sdram_cke <= '1';
        cmd <= CMD_NOP;
        n_state <= state;
        reset_dirty <= (OTHERS => '0');
        n_DPRAM_WE_SDRAM <= (OTHERS => '0');
        n_return_state <= return_state;

        n_fill_count <= fill_count;
        n_fill_count_valid <= fill_count_valid;
        reset_wait_counter <= '0';
        n_row <= row;

        update_current_address <= (OTHERS => '0');
        reset_fill_count <= (OTHERS => '0');

        inc_fill_count <= (OTHERS => '0');

        n_DPRAM_DIN_SDRAM <= DPRAM_DIN_SDRAM;
        n_DPRAM_ADDR_SDRAM <= (OTHERS => (OTHERS => '0'));

        n_mem_wack <= (OTHERS => '0');

        n_dirty <= dirty;
        skiptostop <= '0';

        n_current_port_selection <= current_port_selection;

        CASE state IS
                -- execute the initialisation sequence
            WHEN INIT =>

                CASE wait_counter IS
                    WHEN 0 =>
                        sdram_cke <= '0';
                        cmd <= CMD_DESELECT;
                    WHEN INIT_WAIT - 1 =>
                        cmd <= CMD_PRECHARGE;
                        sdram_a(10) <= '1'; -- all banks
                    WHEN INIT_WAIT + PRECHARGE_WAIT - 1 =>
                        cmd <= CMD_AUTO_REFRESH;
                    WHEN INIT_WAIT + PRECHARGE_WAIT + REFRESH_WAIT - 1 =>
                        cmd <= CMD_AUTO_REFRESH;
                    WHEN INIT_WAIT + PRECHARGE_WAIT + REFRESH_WAIT + REFRESH_WAIT - 1 =>
                        cmd <= CMD_LOAD_MODE;
                        sdram_ba <= "00";
                        sdram_a(12 DOWNTO 10) <= "000";
                        sdram_a(9) <= '0'; -- write burst mode =  0 => programmed burst length, 1 => single location access
                        sdram_a(8 DOWNTO 7) <= "00"; -- operating mode = 0 => normal, all other states reserved
                        sdram_a(6 DOWNTO 4) <= "010"; -- CL = 2
                        sdram_a(3) <= '0'; -- burst type = 0 => sequential, 1 => interleaved
                        sdram_a(2 DOWNTO 0) <= "111"; -- burst length = 7 => full page/row
                    WHEN INIT_WAIT + PRECHARGE_WAIT + REFRESH_WAIT + REFRESH_WAIT =>
                        n_state <= IDLE;
                    WHEN OTHERS =>
                END CASE;

            WHEN IDLE =>
                IF should_refresh = '1' THEN
                    n_state <= REFRESH;
                    n_return_state <= IDLE;
                    -- best speed imo
                ELSIF mem_addr_r(current_port_selection)(24 DOWNTO 13) /= current_address(current_port_selection)(24 DOWNTO 13) THEN
                    n_row <= "000";
                    IF dirty(current_port_selection) = '1' THEN
                        n_state <= FLUSH_CACHE; -- to do: flush ALL caches at the same time
                    ELSE
                        n_state <= FILL_CACHE;
                    END IF;
                ELSIF mem_we /= "00" THEN

                    FOR j IN 0 TO num_ports - 1 LOOP
                        IF mem_we(j) = '1' THEN
                            FOR i IN 0 TO num_ports - 1 LOOP
                                IF mem_addr_r(j)(24 DOWNTO 13) = current_address(i)(24 DOWNTO 13) THEN
                                    n_dirty(i) <= '1';
                                    n_DPRAM_WE_SDRAM(i) <= '1';
                                    n_DPRAM_ADDR_SDRAM(i) <= mem_addr_r(j)(12 DOWNTO 2);
                                    n_DPRAM_DIN_SDRAM(i) <= mem_wdata(j);
                                    n_mem_wack(i) <= '1';
                                END IF;
                            END LOOP;
                        END IF;
                    END LOOP;

                ELSIF current_port_selection < num_ports - 1 THEN
                    n_current_port_selection <= current_port_selection + 1;

                ELSE
                    n_current_port_selection <= 0;

                END IF;

            WHEN FLUSH_CACHE =>
                cmd <= CMD_NOP;
                sdram_ba <= (OTHERS => '0');
                sdram_a <= (OTHERS => '0');
                n_row <= row;
                CASE wait_counter IS
                    WHEN 0 =>
                        cmd <= CMD_ACTIVE;
                        sdram_a <= current_address(current_port_selection)(22 DOWNTO 13) & row;
                        sdram_ba <= current_address(current_port_selection)(24 DOWNTO 23);

                    WHEN 2 =>
                        cmd <= CMD_WRITE;
                        sdram_a(10) <= '1'; -- auto precharge
                        sdram_ba <= current_address(current_port_selection)(24 DOWNTO 23);
                        sdram_dq <= DPRAM_DOUT_SDRAM(current_port_selection)(15 DOWNTO 0);
                        n_DPRAM_ADDR_SDRAM(current_port_selection)(10 DOWNTO 8) <= row;
                        n_DPRAM_ADDR_SDRAM(current_port_selection)(7 DOWNTO 0) <= X"00";
                    WHEN 3 =>
                        sdram_dq <= DPRAM_DOUT_SDRAM(current_port_selection)(31 DOWNTO 16);
                        n_DPRAM_ADDR_SDRAM(current_port_selection)(10 DOWNTO 8) <= row;
                        n_DPRAM_ADDR_SDRAM(current_port_selection)(7 DOWNTO 0) <= X"00";

                    WHEN 4 TO 513 =>
                        n_DPRAM_ADDR_SDRAM(current_port_selection)(10 DOWNTO 8) <= row;
                        n_DPRAM_ADDR_SDRAM(current_port_selection)(7 DOWNTO 0) <= STD_LOGIC_VECTOR(to_unsigned((wait_counter/2) - 1, 8));

                        IF wait_counter MOD 2 = 0 THEN
                            sdram_dq <= DPRAM_DOUT_SDRAM(current_port_selection)(15 DOWNTO 0);
                        ELSE
                            sdram_dq <= DPRAM_DOUT_SDRAM(current_port_selection)(31 DOWNTO 16);
                        END IF;
                    WHEN 514 =>
                        cmd <= CMD_STOP;
                        IF row = "111" THEN
                            n_state <= FILL_CACHE;
                            n_row <= "000";
                            --reset_fill_count(current_port_selection) <= '1';
                            n_fill_count(current_port_selection) <= (OTHERS => '0');
                            n_fill_count_valid(current_port_selection) <= '0';
                            FOR i IN 0 TO num_ports - 1 LOOP
                                IF current_address(current_port_selection)(24 DOWNTO 13) = current_address(i)(24 DOWNTO 13) THEN
                                    n_dirty(i) <= '0';
                                END IF;
                            END LOOP;
                        ELSE
                            n_row <= row + "001";
                            reset_wait_counter <= '1';
                            IF should_refresh = '1' THEN
                                n_return_state <= FLUSH_CACHE;
                                n_state <= REFRESH;
                            END IF;

                        END IF;
                    WHEN OTHERS =>
                        -- some error
                END CASE;

            WHEN FILL_CACHE =>
                cmd <= CMD_NOP;
                sdram_ba <= (OTHERS => '0');
                sdram_a <= (OTHERS => '0');
                n_row <= row;
                sdram_dq <= (OTHERS => 'Z');

                CASE wait_counter IS
                    WHEN 0 =>
                        cmd <= CMD_ACTIVE;
                        sdram_ba <= mem_addr_r(current_port_selection)(24 DOWNTO 23);
                        sdram_a <= mem_addr_r(current_port_selection)(22 DOWNTO 13) & row;

                    WHEN 2 =>
                        cmd <= CMD_READ;
                        sdram_a(10) <= '1'; -- auto precharge
                        --                       sdram_a <= (OTHERS => '0'); -- starting column - might be a good idea to use non-zero value for faster access
                        sdram_ba <= mem_addr_r(current_port_selection)(24 DOWNTO 23);

                    WHEN 4 TO 515 =>
                    update_current_address(current_port_selection) <= '1';

                        --DPRAM_ADDR_SDRAM(current_port_selection)(10 DOWNTO 8) <= row;
                        --DPRAM_ADDR_SDRAM(current_port_selection)(7 DOWNTO 0) <= STD_LOGIC_VECTOR(to_unsigned(((wait_counter - 4)/2), 8));

                        n_DPRAM_ADDR_SDRAM(current_port_selection) <= fill_count(current_port_selection);

                        if cache_miss(current_port_selection) = '1' then
                            skiptostop <= '1';
                        elsIF wait_counter MOD 2 = 0 THEN
                            n_DPRAM_DIN_SDRAM(current_port_selection)(15 DOWNTO 0) <= sdram_dq;
                            n_fill_count_valid(current_port_selection) <= '0';
                        ELSE
                            n_DPRAM_DIN_SDRAM(current_port_selection)(31 DOWNTO 16) <= sdram_dq;
                            n_DPRAM_WE_SDRAM(current_port_selection) <= '1';
                            IF fill_count(current_port_selection) < "11111111111" THEN
                                --inc_fill_count(current_port_selection) <= '1'; --fill_count_valid(current_port_selection);
                                n_fill_count(current_port_selection) <= fill_count(current_port_selection) + "00000000001";
                            END IF;
                            n_fill_count_valid(current_port_selection) <= '1';

                        END IF;
                    WHEN 516 =>
                        cmd <= CMD_STOP;
                        IF (row = "111") or (cache_miss(current_port_selection) = '1') THEN
                            n_return_state <= IDLE;
                            n_state <= IDLE;
                            n_row <= "000";

                        ELSE
                            n_row <= row + "001";
                            reset_wait_counter <= '1';
                            --IF should_refresh = '1' THEN
                            --    n_return_state <= FILL_CACHE;
                            --    n_state <= REFRESH;
                            --END IF;
                        END IF;
                    WHEN OTHERS =>
                        -- some error
                END CASE;
                -- execute an auto refresh
            WHEN REFRESH =>
                cmd <= CMD_AUTO_REFRESH;

                IF refresh_done = '1' THEN
                    n_state <= return_state;
                END IF;
        END CASE;
    END PROCESS;
    sync : PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            state <= INIT;
            current_address <= (OTHERS => (OTHERS => '0'));
            wait_counter <= 0;
            refresh_counter <= 0;
            fill_count <= (OTHERS => (OTHERS => '0'));
            fill_count_valid <= (OTHERS => '0');
            row <= "000";
            current_port_selection <= 0;
            DPRAM_WE_CPU <= (OTHERS => '0');
            --mem_rdy <= (OTHERS => '0');
            --mem_wack <= (OTHERS => '0');
            DPRAM_WE_SDRAM <= (OTHERS => '0');
            DPRAM_DIN_SDRAM <= (OTHERS => (OTHERS => '0'));
            return_state <= INIT;
            DPRAM_ADDR_SDRAM <= (OTHERS => (OTHERS => '0'));
            mem_wack <= (OTHERS => '0');
            dirty <= (OTHERS => '0');
                        mem_addr_r <= (others => (others => '0'));

        ELSIF rising_edge(clk) THEN
                mem_addr_r <= mem_addr;


            DPRAM_ADDR_SDRAM <= n_DPRAM_ADDR_SDRAM;

            row <= n_row;

            DPRAM_WE_SDRAM <= n_DPRAM_WE_SDRAM;

            state <= n_state;
            return_state <= n_return_state;
            DPRAM_DIN_SDRAM <= n_DPRAM_DIN_SDRAM;
            --mem_rdy <= (OTHERS => '0');
            DPRAM_WE_CPU <= (OTHERS => '0');
            mem_wack <= n_mem_wack;
            dirty <= n_dirty;
            --mem_wack <= (OTHERS => '0');
            FOR i IN 0 TO num_ports - 1 LOOP

            mem_rdata(i) <= DPRAM_DOUT_CPU(i);


                IF update_current_address(i) = '1' THEN
                    current_address(i) <= mem_addr_r(i);
                END IF;

                --                IF reset_fill_count(i) = '1' THEN
                --                    fill_count(i) <= (OTHERS => '0');
                --                ELSIF inc_fill_count(i) = '1' THEN
                --                    fill_count(i) <= fill_count(i) + "00000000001";
                --                END IF;

                fill_count <= n_fill_count;
                fill_count_valid(i) <= n_fill_count_valid(i);

                --                IF ((fill_count(i) > mem_addr_r(i)(12 DOWNTO 2)) OR ((fill_count(i) = mem_addr_r(i)(12 DOWNTO 2)) AND (fill_count_valid(i) = '1'))) AND (cache_miss(i) = '0') AND (address_valid(i) = '1') THEN

                --IF (cache_miss(i) = '0') AND (address_valid(i) = '1') THEN
                --IF (mem_addr_r(i)(12 DOWNTO 2) < fill_count(i)) AND (cache_miss(i) = '0') AND (address_valid(i) = '1') THEN
                --DPRAM_WE_CPU(i) <= mem_we(i); -- not coherent
                --mem_rdy(i) <= '1'; --mem_re(i);
                --END IF;

            END LOOP;

            IF (state /= n_state) OR (reset_wait_counter = '1') THEN -- state changing
                wait_counter <= 0;
            ELSif skiptostop = '1' then
                wait_counter <= 516;
            else
                wait_counter <= wait_counter + 1;
            END IF;

            IF state = REFRESH AND wait_counter = 0 THEN
                refresh_counter <= 0;
            ELSE
                refresh_counter <= refresh_counter + 1;
            END IF;

            current_port_selection <= n_current_port_selection;

        END IF;
    END PROCESS;

    rdygen : FOR i IN 0 TO num_ports - 1 GENERATE
        --mem_rdy(i) <= mem_re(i) WHEN (mem_addr_r(i)(12 DOWNTO 2) < fill_count(i)) AND (cache_miss(i) = '0') AND (address_valid(i) = '1') ELSE '0';
        -- mem_rdy(i) <= mem_re(i) WHEN (mem_addr_r(i)(12 DOWNTO 2) <= fill_count(i)) AND (cache_miss(i) = '0') AND (address_valid(i) = '1') ELSE '0';
        mem_rdy(i) <= mem_re(i) WHEN  (cache_miss(i) = '0') AND (address_valid(i) = '1') AND (fill_count(i) > mem_addr_r(i)(15 downto 5)) ELSE --(DPRAM_WE_SDRAM(i) = '0') 
        '0'; -- works but is not fully correct

    END GENERATE rdygen;

    -- set wait signals
    refresh_done <= '1' WHEN wait_counter = REFRESH_WAIT - 1 ELSE
        '0';

    -- the SDRAM should be refreshed when the refresh interval has elapsed
    should_refresh <= '1' WHEN refresh_counter >= REFRESH_INTERVAL - 1 ELSE
        '0';

    -- set SDRAM control signals
    (sdram_cs_n, sdram_ras_n, sdram_cas_n, sdram_we_n) <= cmd;

    sdram_dqmh <= '0';
    sdram_dqml <= '0';

    gen_dpram : FOR i IN 0 TO num_ports - 1 GENERATE

        lattice : IF vendor = '1' GENERATE
            inst_dpram : dpram1
            PORT MAP(
                DataInA => DPRAM_DIN_SDRAM(i), DataInB => DPRAM_DIN_CPU(i),
                AddressA => DPRAM_ADDR_SDRAM(i), AddressB => mem_addr_r(i)(12 DOWNTO 2),
                ClockA => clk, ClockB => mem_clk(i),
                ClockEnA => '1', ClockEnB => '1',
                WrA => DPRAM_WE_SDRAM(i), WrB => DPRAM_WE_CPU(i),
                ResetA => '0', ResetB => '0',
                QA => DPRAM_DOUT_SDRAM(i), QB => DPRAM_DOUT_CPU(i)
            );
        END GENERATE lattice;

        xilinx : IF vendor = '0' GENERATE
            inst_dpram : blk_mem_gen_0
            PORT MAP(
                clka => clk,
                wea(0) => DPRAM_WE_SDRAM(i),
                addra => DPRAM_ADDR_SDRAM(i),
                dina => DPRAM_DIN_SDRAM(i),
                douta => DPRAM_DOUT_SDRAM(i),
                clkb => mem_clk(i),
                web(0) => DPRAM_WE_CPU(i),
                addrb => mem_addr_r(i)(12 DOWNTO 2),
                dinb => DPRAM_DIN_CPU(i),
                doutb => DPRAM_DOUT_CPU(i)
            );
        END GENERATE xilinx;

        --mem_wack(i)      <= DPRAM_WE_CPU(i);
        address_valid(i) <= '1' WHEN mem_addr_r(i)(31 DOWNTO 25) = base_address(31 DOWNTO 25) ELSE
        '0'; -- 0xD0000000 to 0xD1FFFFFF
        cache_miss(i) <= '1' WHEN (current_address(i)(24 DOWNTO 13) /= mem_addr_r(i)(24 DOWNTO 13)) ELSE
        '0';

        PROCESS (DPRAM_DOUT_CPU, mem_width, mem_addr_r, mem_wdata)
        BEGIN
            DPRAM_DIN_CPU(i) <= DPRAM_DOUT_CPU(i);
            CASE mem_width(i) IS
                WHEN "00" => -- 1 byte access
                    CASE mem_addr_r(i)(1 DOWNTO 0) IS
                        WHEN "00" =>
                            DPRAM_DIN_CPU(i)(7 DOWNTO 0) <= mem_wdata(i)(7 DOWNTO 0);
                        WHEN "01" =>
                            DPRAM_DIN_CPU(i)(15 DOWNTO 8) <= mem_wdata(i)(7 DOWNTO 0);
                        WHEN "10" =>
                            DPRAM_DIN_CPU(i)(23 DOWNTO 16) <= mem_wdata(i)(7 DOWNTO 0);
                        WHEN "11" =>
                            DPRAM_DIN_CPU(i)(31 DOWNTO 24) <= mem_wdata(i)(7 DOWNTO 0);
                        WHEN OTHERS =>
                            -- xilinx sim is retarded
                    END CASE;
                WHEN "01" => -- 2 byte / halfword access
                    CASE mem_addr_r(i)(0) IS
                        WHEN '0' =>
                            DPRAM_DIN_CPU(i)(15 DOWNTO 0) <= mem_wdata(i)(15 DOWNTO 0);
                        WHEN '1' =>
                            DPRAM_DIN_CPU(i)(31 DOWNTO 16) <= mem_wdata(i)(15 DOWNTO 0);
                        WHEN OTHERS =>

                    END CASE;
                WHEN OTHERS =>
                    DPRAM_DIN_CPU(i) <= mem_wdata(i);

            END CASE;
        END PROCESS;
        -- PROCESS (DPRAM_WE_CPU, clk)
        -- BEGIN
        --     IF DPRAM_WE_CPU(i) = '1' THEN
        --         dirty(i) <= '1';
        --     ELSIF rising_edge(clk) THEN
        --         IF reset_dirty(i) = '1' THEN
        --             dirty(i) <= '0';
        --         END IF;
        --     END IF;
        -- END PROCESS;

    END GENERATE gen_dpram;
    addr_valid <= address_valid(0);

END behavioural;