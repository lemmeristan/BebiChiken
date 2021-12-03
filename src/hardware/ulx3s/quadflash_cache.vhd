-- vsg_off
----------------------------------------------------------------------------------
-- Company:
-- Engineer: Lemmer EL ASSAL
--
-- Create Date: 11/24/2021 06:03:48 PM
-- Design Name: Quad SPI Flash Cache (W25Q128JVSIQ)
-- Module Name: quadflash_cache - behavioural
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
ENTITY quadflash_cache IS
    GENERIC (
        vendor : STD_LOGIC := '0'; -- 0 => xilinx, 1 => lattice

        base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"C8000000"
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

        led : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        initializing : OUT STD_LOGIC

    );
END quadflash_cache;

ARCHITECTURE behavioural OF quadflash_cache IS
    -- dualport ram
    COMPONENT simple_dualport_ram_8k_lattice PORT (

        Data : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        WrAddress, RdAddress : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
        RdClock, WrClock, WrClockEn, RdClockEn, WE, Reset : IN STD_LOGIC;
        Q : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)

        );
    END COMPONENT;

    COMPONENT simple_dualport_ram_8k_xilinx IS
        PORT (
            clka : IN STD_LOGIC;
            wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
            addra : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
            dina : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            clkb : IN STD_LOGIC;
            addrb : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
            doutb : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    TYPE state_t IS (S_RESET, INIT, IDLE, FILL_CACHE);
    SIGNAL state, n_state : state_t;

    CONSTANT CMD_READ_QUAD : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"6B";
    CONSTANT CMD_WRITE_ENABLE : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"06";
    CONSTANT CMD_WRITE_ENABLE_VOLATILE : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"50";

    CONSTANT CMD_READ_STATUS_REG_1 : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"05";
    CONSTANT CMD_READ_STATUS_REG_2 : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"35";

    CONSTANT CMD_WRITE_STATUS_REG_1 : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"01";
    CONSTANT CMD_WRITE_STATUS_REG_2 : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"31";

    CONSTANT CMD_ENABLE_RESET : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"66";
    CONSTANT CMD_RESET : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"99";

    CONSTANT STATUS_REG_QUAD_ENABLE : STD_LOGIC_VECTOR(7 DOWNTO 0) := X"02";

    CONSTANT temp_offset : STD_LOGIC_VECTOR(12 DOWNTO 0) := '0' & X"290";

    SIGNAL waitcounter, n_waitcounter : INTEGER RANGE 0 TO 700000; -- turn into lfsr to mek it go fest
    SIGNAL n_fill_count, fill_count : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL cache_DIN : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL current_address, n_current_address : STD_LOGIC_VECTOR(10 DOWNTO 0);
    SIGNAL cache_WE, address_valid : STD_LOGIC;
    SIGNAL shift_reg, n_shift_reg : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL update_current_address, start_fill_cache : STD_LOGIC;

    SIGNAL initialized, n_initialized, sck_state, n_sck_state, n_wel, wel : STD_LOGIC;

    SIGNAL sr1, n_sr1, sr2, n_sr2 : STD_LOGIC_VECTOR(7 DOWNTO 0);

BEGIN

    cache_DIN <= n_shift_reg(7 DOWNTO 0) & n_shift_reg(15 DOWNTO 8) & n_shift_reg(23 DOWNTO 16) & n_shift_reg(31 DOWNTO 24);
    async : PROCESS (clk, sr1, sr2, sck_state, wel, initialized, spi_do, state, current_address, shift_reg, fill_count, waitcounter, start_fill_cache, spi_io, address_valid)
    BEGIN
        spi_csn <= '1';
        spi_di <= '0';
        spi_wpn <= '1';
        spi_holdn <= '1';
        cache_WE <= '0';
        spi_sck <= '0';
        spi_reading <= '0';
        n_current_address <= current_address;
        n_fill_count <= fill_count;
        n_shift_reg <= shift_reg;
        n_state <= state;
        update_current_address <= '0';
        n_waitcounter <= waitcounter;

        led <= (OTHERS => '0');
        --led <= sr2;
        --led(7) <= address_valid;
        --led(6) <= start_fill_cache;

        --led(5) <= cache_WE;

        n_initialized <= initialized;

        initializing <= '0';
        mem_rdy <= '0';

        n_sr1 <= sr1;
        n_sr2 <= sr2;

        --led(4) <= wel;
        CASE state IS

            WHEN S_RESET =>
                n_waitcounter <= waitcounter + 1;
                CASE waitcounter IS
                    WHEN 0 TO 7 =>

                    WHEN 8 TO 15 =>
                        spi_sck <= clk;
                        spi_csn <= '0';
                        spi_di <= CMD_ENABLE_RESET(7 - (waitcounter - 8));

                    WHEN 16 TO 23 =>

                    WHEN 24 TO 31 =>
                        spi_sck <= clk;
                        spi_csn <= '0';
                        spi_di <= CMD_ENABLE_RESET(7 - (waitcounter - 8));

                    WHEN 32 TO 999 => -- wait 30 us (actually 45 us here)

                    WHEN 1000 =>
                        n_waitcounter <= 0;
                        n_state <= INIT;
                    WHEN OTHERS =>
                        n_waitcounter <= 0;
                END CASE;

            WHEN INIT =>
                initializing <= '1';
                n_waitcounter <= waitcounter + 1;

                CASE waitcounter IS

                        -- read status reg 1 & 2
                        -- status reg 1: SRP | SEC | TB | BP2 | BP1 | BP0 | WEL | BUSY
                        -- status reg 2: SUS | CMP | LB3 | LB2 | LB1 | (R) | QE | SRL
                        -- if BUSY = '1', read status reg again
                        -- else if QE = '1': go straight to IDLE
                        -- else if QE = '0': issue WE; read status reg; wait until WEL = '1', BUSY = '0'; set QE flag; go back to start
                    WHEN 0 TO 7 =>

                        --led(0) <= '1';
                    WHEN 8 TO 15 =>
                        spi_sck <= clk;
                        spi_csn <= '0';
                        spi_di <= CMD_READ_STATUS_REG_1(7 - (waitcounter - 8));
                    WHEN 16 TO 23 =>
                        led <= (OTHERS => '0');
                        spi_sck <= clk;
                        spi_csn <= '0';
                        n_sr1 <= sr1(6 DOWNTO 0) & spi_do;
                    WHEN 24 =>
                        IF sr1(0) = '1' THEN -- busy?
                            n_waitcounter <= 0;
                        END IF;
                    WHEN 25 TO 39 =>

                        -- check for quad enable bit

                    WHEN 40 TO 47 =>
                        --led(1) <= '1';
                        spi_sck <= clk;
                        spi_csn <= '0';
                        spi_di <= CMD_READ_STATUS_REG_2(7 - (waitcounter - 40));
                    WHEN 48 TO 55 =>
                        led <= (OTHERS => '0');
                        spi_sck <= clk;
                        spi_csn <= '0';
                        n_sr2 <= sr2(6 DOWNTO 0) & spi_do;
                    WHEN 56 =>
                        IF sr2(1) = '1' THEN -- quad enable?
                            n_waitcounter <= 0;
                            n_state <= IDLE;
                        ELSIF sr1(1) = '1' THEN -- WEL already set
                            n_waitcounter <= 73;
                        END IF;

                        -- set WEL
                    WHEN 57 TO 63 =>
                        --led(2) <= '1';

                    WHEN 64 TO 71 =>
                        spi_sck <= clk;
                        spi_csn <= '0';
                        spi_di <= CMD_WRITE_ENABLE(7 - (waitcounter - 64));

                    WHEN 72 =>
                        n_waitcounter <= 0;

                    WHEN 73 TO 79 =>
                        --led(3) <= '1';

                    WHEN 80 TO 87 =>
                        spi_sck <= clk;
                        spi_csn <= '0';

                        spi_di <= CMD_WRITE_STATUS_REG_2(7 - (waitcounter - 80));
                    WHEN 88 TO 95 =>
                        spi_sck <= clk;
                        spi_csn <= '0';
                        spi_di <= STATUS_REG_QUAD_ENABLE(7 - (waitcounter - 88));
                        IF waitcounter = 95 THEN
                            n_waitcounter <= 0;
                        END IF;
                    WHEN OTHERS =>
                        n_waitcounter <= 0;
                END CASE;
            WHEN IDLE =>
                led(1) <= '1';
                IF (mem_re = '1') AND (address_valid = '1') THEN
                    IF current_address /= mem_addr(23 DOWNTO 13) THEN
                        n_state <= FILL_CACHE;
                        n_waitcounter <= 0;
                        n_fill_count <= (OTHERS => '0');
                    ELSE
                        mem_rdy <= '1';
                    END IF;
                END IF;

            WHEN FILL_CACHE =>
                n_initialized <= '1';
                led(2) <= '1';

                CASE waitcounter IS
                    WHEN 0 TO 7 =>
                        n_waitcounter <= waitcounter + 1;

                        spi_sck <= '0';
                        spi_di <= '0';
                    WHEN 8 TO 15 =>
                        n_waitcounter <= waitcounter + 1;

                        spi_sck <= clk;
                        spi_csn <= '0';
                        spi_di <= CMD_READ_QUAD(7 - (waitcounter MOD 8));

                    WHEN 16 TO 26 =>
                        n_waitcounter <= waitcounter + 1;

                        spi_sck <= clk;
                        spi_csn <= '0';
                        spi_di <= mem_addr(23 - (waitcounter - 16));
                        --16 => addr(10)
                        --17 => addr(9)
                        --18 => addr(8)
                        --...
                        --26 => addr(0)

                    WHEN 27 TO 39 => -- 13 zeroes = 8 kB boundary
                        n_waitcounter <= waitcounter + 1;

                        spi_sck <= clk;
                        spi_csn <= '0';
                        spi_di <= '0'; --temp_offset(12 - (waitcounter - 27)); --

                    WHEN 40 TO 47 => -- dummy byte
                        n_waitcounter <= waitcounter + 1;
                        spi_sck <= clk;
                        spi_csn <= '0';

                    WHEN 48 =>
                        spi_reading <= '1';
                        spi_sck <= clk;
                        spi_csn <= '0';

                        n_shift_reg <= shift_reg(27 DOWNTO 0) & spi_io(3 DOWNTO 0);
                        n_fill_count <= fill_count + "00000000000001";

                        IF fill_count(2 DOWNTO 0) = "111" THEN
                            cache_WE <= '1';
                            IF fill_count = "11111111111111" THEN
                                n_state <= IDLE;
                                update_current_address <= '1';
                            END IF;
                        END IF;

                    WHEN OTHERS =>
                        n_state <= INIT;
                        n_waitcounter <= 0;
                END CASE;
            WHEN OTHERS =>
                n_state <= INIT;
        END CASE;
    END PROCESS;

    sync : PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            fill_count <= (OTHERS => '0');
            shift_reg <= (OTHERS => '0');
            current_address <= (OTHERS => '1');
            state <= S_RESET;
            waitcounter <= 0;
            initialized <= '0';
            wel <= '0';
            sr1 <= (OTHERS => '0');
            sr2 <= (OTHERS => '0');

        ELSIF rising_edge(clk) THEN
            fill_count <= n_fill_count;
            shift_reg <= n_shift_reg;
            state <= n_state;
            waitcounter <= n_waitcounter;

            initialized <= n_initialized;
            wel <= n_wel;

            sr1 <= n_sr1;
            sr2 <= n_sr2;

            IF update_current_address = '1' THEN
                current_address <= mem_addr(23 DOWNTO 13);
            END IF;

        END IF;
    END PROCESS;
    xilinx : IF vendor = '0' GENERATE
        inst_dpram : simple_dualport_ram_8k_xilinx
        PORT MAP(
            clka => clk,
            wea(0) => cache_WE,
            addra => fill_count(13 DOWNTO 3),
            dina => cache_DIN,
            clkb => mem_clk,
            addrb => mem_addr(12 DOWNTO 2),
            doutb => mem_rdata
        );
    END GENERATE xilinx;
    lattice : IF vendor = '1' GENERATE
        inst_dpram : simple_dualport_ram_8k_lattice
        PORT MAP(
            Data => cache_DIN,
            WrAddress => fill_count(13 DOWNTO 3), RdAddress => mem_addr(12 DOWNTO 2),
            RdClock => mem_clk, WrClock => clk, WrClockEn => cache_WE, RdClockEn => mem_re, WE => cache_WE, Reset => '0', --reset,
            Q => mem_rdata
        );
    END GENERATE lattice;

    address_valid <= '1' WHEN mem_addr(31 DOWNTO 24) = base_address(31 DOWNTO 24) ELSE
        '0';
    -- mem_rdy <= mem_re WHEN (mem_addr(31 DOWNTO 24) = base_address(31 DOWNTO 24))
    --     AND (current_address = mem_addr(23 DOWNTO 13))
    --     AND ((fill_count(13 DOWNTO 3) > mem_addr(12 DOWNTO 2)) OR ((fill_count(13 DOWNTO 3) = mem_addr(12 DOWNTO 2)) AND (fill_count(2 DOWNTO 0) = "111")))
    --     ELSE
    --     '0';

END behavioural;