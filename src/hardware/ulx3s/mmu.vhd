-- vsg_off
----------------------------------------------------------------------------------
-- Company:
-- Engineer: Lemmer EL ASSAL
--
-- Create Date: 11/29/2021 15:52 PM
-- Design Name: 
-- Module Name: mmu - behavioural
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
USE IEEE.NUMERIC_STD.ALL; -- shift

USE ieee.math_real.ALL; -- ceil

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

LIBRARY work;
USE work.bebichiken.ALL;
ENTITY mmu IS
    -- GENERIC (
    --     peripheral_addresses : peripheral_address_t
    -- );

    PORT (
        rst : IN STD_LOGIC;
        sys_clk : IN STD_LOGIC;
        host_we : IN STD_LOGIC;
        host_re : IN STD_LOGIC;
        host_addr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        host_width : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        host_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        host_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        host_rdy : OUT STD_LOGIC;
        host_wack : OUT STD_LOGIC;
        host_address_invalid : OUT STD_LOGIC;
        peripheral_we : OUT peripheral_bit_t;
        peripheral_re : OUT peripheral_bit_t;
        peripheral_addr : OUT peripheral_word_t;
        peripheral_width : OUT peripheral_width_t;
        peripheral_wdata : OUT peripheral_word_t;
        peripheral_rdata : IN peripheral_word_t;
        peripheral_rdy : IN peripheral_bit_t;
        peripheral_wack : IN peripheral_bit_t

    );
END mmu;

ARCHITECTURE behavioural OF mmu IS

    -- CONSTANT IDX_UART : INTEGER := 0;
    -- CONSTANT IDX_SPI_ROM : INTEGER := 1;
    -- CONSTANT IDX_SDRAM : INTEGER := 2;
    -- CONSTANT IDX_SMB : INTEGER := 3;

    FUNCTION f_address_to_peripheral (
        address : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN peripherals_t IS
    BEGIN

        -- case address(31 downto 12) is
        --     when X"00000" to X"00FFF" => -- SPI Flash (ROM)
        --         return IDX_SPI_ROM;
        --     when X"01000" to X"01FFF" => -- SDRAM
        --         return IDX_SDRAM;
        --     when X"2000" =>
        --         return IDX_SMB;
        -- end case;

        IF (address(31 DOWNTO 12) = X"C0001") THEN
            RETURN PERIPH_UART;
        ELSIF (address(31 DOWNTO 12) >= X"D0000") AND (address(31 DOWNTO 12) <= X"D1FFF") THEN -- SDRAM
            RETURN PERIPH_SDRAM;
        ELSE
            RETURN PERIPH_INVALID;
        END IF;
    END;
    -- FUNCTION f_has_multiple_access (
    --     address : word_array_t(num_hosts - 1 DOWNTO 0);
    --     we, re : STD_LOGIC_VECTOR(num_hosts - 1 DOWNTO 0)
    -- )
    --     RETURN STD_LOGIC IS

    --     VARIABLE peripheral_enables : STD_LOGIC_VECTOR(num_peripherals - 1 DOWNTO 0);
    --     VARIABLE temp : INTEGER;
    -- BEGIN

    --     peripheral_enables := (OTHERS => '0');

    --     FOR i IN 0 TO num_hosts - 1 LOOP
    --         IF (we(i) = '1') OR (re(i) = '1') THEN
    --             temp := f_address_to_peripheral(address(i));
    --             IF temp < num_peripherals THEN
    --                 IF peripheral_enables(temp) = '1' THEN
    --                     RETURN '1';
    --                 ELSE
    --                     peripheral_enables(temp) := '1';
    --                 END IF;
    --             END IF;
    --         END IF;

    --     END LOOP;

    --     RETURN '0';
    -- END;
    TYPE state_t IS (GOOD, FIND_ALTERNATIVE);
    SIGNAL state, n_state : state_t;
    SIGNAL current_host, n_current_host : INTEGER RANGE 0 TO 0;
BEGIN

    -- PROCESS (state, host_addr, host_we, host_re, host_width, host_wdata, current_host, peripheral_rdata, peripheral_rdy, peripheral_wack)
    -- variable temp : peripherals_t;
    -- BEGIN
    --     n_state <= state;
    --     peripheral_we <= (others => '0');
    --     peripheral_re <= (others => '0');
    --     peripheral_addr <= (others => (OTHERS => '0')); --(others => host_addr(0)); --
    --     peripheral_width <= (others => (OTHERS => '0')); --(others => "10"); --
    --     peripheral_wdata <= (others => (OTHERS => '0')); --(others => host_wdata(0)); --
    --     host_rdata <= (OTHERS => '0'); --(others => peripheral_rdata(IDX_SDRAM));--
    --     host_rdy <= '0';
    --     host_wack <= '0';
    --     host_address_invalid <= '0';

    --     n_current_host <= current_host;

    --     CASE state IS
    --         WHEN GOOD =>
    --             --IF f_has_multiple_access(host_addr, host_we, host_re) = '1' THEN
    --             --    n_state <= FIND_ALTERNATIVE;
    --             --ELSE
    --             --FOR i IN 0 TO num_hosts - 1 LOOP
    --             --    IF temp < num_peripherals THEN

    --             temp := f_address_to_peripheral(host_addr);
    --             peripheral_we(temp) <= host_we;
    --             peripheral_re(temp) <= host_re;
    --             peripheral_addr(temp) <= host_addr;
    --             peripheral_width(temp) <= host_width;
    --             peripheral_wdata(temp) <= host_wdata;
    --             host_rdata <= peripheral_rdata(temp);
    --             host_rdy <= peripheral_rdy(temp);
    --             host_wack <= peripheral_wack(temp);
    --             --   ELSE

    --             --      host_address_invalid <= '1';
    --             --   END IF;
    --             --END LOOP;
    --             --END IF;
    --         WHEN FIND_ALTERNATIVE =>
    --             -- iterate current_host from 0 to num_hosts-1
    --             -- if there's a read: issue read, wait for read to go low
    --             -- if there's a write: issue write, wait for write to go low
    --             -- simple, no?
    --             n_state <= GOOD;
    --         WHEN OTHERS =>
    --             n_state <= GOOD;
    --     END CASE;

    -- END PROCESS;
    host_address_invalid <= '0';

    PROCESS (rst, sys_clk)
    variable temp : peripherals_t;
    BEGIN
        IF rst = '1' THEN
        peripheral_we <= (others => '0');
        peripheral_re <= (others => '0');
        peripheral_addr <= (others => (OTHERS => '0')); --(others => host_addr(0)); --
        peripheral_width <= (others => (OTHERS => '0')); --(others => "10"); --
        peripheral_wdata <= (others => (OTHERS => '0')); --(others => host_wdata(0)); --
        host_rdata <= (OTHERS => '0'); --(others => peripheral_rdata(IDX_SDRAM));--
        host_rdy <= '0';
        host_wack <= '0';
            state <= GOOD;
        ELSIF rising_edge(sys_clk) THEN
            state <= n_state;
            temp := f_address_to_peripheral(host_addr);
            peripheral_we(temp) <= host_we;
            peripheral_re(temp) <= host_re;
            peripheral_addr(temp) <= host_addr;
            peripheral_width(temp) <= host_width;
            peripheral_wdata(temp) <= host_wdata;
            host_rdata <= peripheral_rdata(temp);
            host_rdy <= peripheral_rdy(temp);
            host_wack <= peripheral_wack(temp);
        END IF;
    END PROCESS;

END behavioural;