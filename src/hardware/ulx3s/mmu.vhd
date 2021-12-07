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
    GENERIC (
        num_hosts       : INTEGER := 1;
        num_peripherals : INTEGER := 3
    );

    PORT (
        rst     : IN STD_LOGIC;
        sys_clk : IN STD_LOGIC;
        host_we              : IN STD_logic;
        host_re              : IN STD_logic;
        host_addr            : IN std_logic_vector(31 downto 0);
        host_width           : IN std_logic_Vector(1 downto 0);
        host_wdata           : IN std_logic_vector(31 downto 0);
        host_rdata           : OUT std_logic_vector(31 downto 0);
        host_rdy             : OUT STD_logic;
        host_wack            : OUT STD_logic;
        host_address_invalid : OUT STD_logic;
        peripheral_we        : OUT STD_LOGIC;
        peripheral_re        : OUT STD_LOGIC;
        peripheral_addr      : OUT std_logic_vector(31 downto 0);
        peripheral_width     : OUT std_logic_vector(1 downto 0);
        peripheral_wdata     : OUT std_logic_vector(31 downto 0);
        peripheral_rdata     : IN std_logic_vector(31 downto 0);
        peripheral_rdy       : IN STD_LOGIC;
        peripheral_wack      : IN STD_LOGIC

    );
END mmu;

ARCHITECTURE behavioural OF mmu IS

    CONSTANT IDX_SPI_ROM : INTEGER := 0;
    CONSTANT IDX_SDRAM   : INTEGER := 1;
    CONSTANT IDX_SMB     : INTEGER := 2;

    FUNCTION f_address_to_peripheral (
        address : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN INTEGER IS
    BEGIN

        -- case address(31 downto 12) is
        --     when X"00000" to X"00FFF" => -- SPI Flash (ROM)
        --         return IDX_SPI_ROM;
        --     when X"01000" to X"01FFF" => -- SDRAM
        --         return IDX_SDRAM;
        --     when X"2000" =>
        --         return IDX_SMB;
        -- end case;

        IF (address(31 DOWNTO 12) >= X"00000") AND (address(31 DOWNTO 12) <= X"00FFF") THEN -- SPI Flash (ROM)
            RETURN IDX_SPI_ROM;
        ELSIF (address(31 DOWNTO 12) >= X"01000") AND (address(31 DOWNTO 12) <= X"01FFF") THEN -- -- SDRAM        
            RETURN IDX_SDRAM;
        ELSIF address(31 DOWNTO 12) = X"2000" THEN
            RETURN IDX_SMB;
        ELSE
            RETURN num_peripherals;
        END IF;
    END;
    FUNCTION f_has_multiple_access (
        address : word_array_t(num_hosts - 1 DOWNTO 0);
        we, re  : STD_LOGIC_VECTOR(num_hosts - 1 DOWNTO 0)
    )
        RETURN STD_LOGIC IS

        VARIABLE peripheral_enables : STD_LOGIC_VECTOR(num_peripherals - 1 DOWNTO 0);
        VARIABLE temp               : INTEGER;
    BEGIN

        peripheral_enables := (OTHERS => '0');

        FOR i IN 0 TO num_hosts - 1 LOOP
            IF (we(i) = '1') OR (re(i) = '1') THEN
                temp := f_address_to_peripheral(address(i));
                IF temp < num_peripherals THEN
                    IF peripheral_enables(temp) = '1' THEN
                        RETURN '1';
                    ELSE
                        peripheral_enables(temp) := '1';
                    END IF;
                END IF;
            END IF;

        END LOOP;

        RETURN '0';
    END;
    TYPE state_t IS (GOOD, FIND_ALTERNATIVE);
    SIGNAL state, n_state               : state_t;
    SIGNAL current_host, n_current_host : INTEGER RANGE 0 TO num_hosts - 1;
BEGIN

    PROCESS (state, host_addr, host_we, host_re, host_width, host_wdata, current_host, peripheral_rdata, peripheral_rdy, peripheral_wack)
        VARIABLE temp : INTEGER RANGE 0 TO num_peripherals - 1;
    BEGIN
        n_state              <= state;
        peripheral_we        <= '0';
        peripheral_re        <= '0';
        peripheral_addr      <= (others => '0'); --(others => host_addr(0)); --
        peripheral_width     <= (others => '0'); --(others => "10"); --
        peripheral_wdata     <= (others => '0'); --(others => host_wdata(0)); --
        host_rdata           <= (others => '0'); --(others => peripheral_rdata(IDX_SDRAM));--
        host_rdy             <= '0';
        host_wack            <= '0';
        host_address_invalid <= '0';

        n_current_host <= current_host;

        CASE state IS
            WHEN GOOD =>
                --IF f_has_multiple_access(host_addr, host_we, host_re) = '1' THEN
                --    n_state <= FIND_ALTERNATIVE;
                --ELSE
                    FOR i IN 0 TO num_hosts - 1 LOOP
                        temp := f_address_to_peripheral(host_addr);
                        IF temp < num_peripherals THEN

                            peripheral_we    <= host_we;
                            peripheral_re    <= host_re;
                            peripheral_addr  <= host_addr;
                            peripheral_width <= host_width;
                            peripheral_wdata <= host_wdata;
                            host_rdata          <= peripheral_rdata;
                            host_rdy            <= peripheral_rdy;
                            host_wack           <= peripheral_wack;
                        ELSE

                            host_address_invalid <= '1';
                        END IF;
                    END LOOP;
                --END IF;
            WHEN FIND_ALTERNATIVE =>
                -- iterate current_host from 0 to num_hosts-1
                -- if there's a read: issue read, wait for read to go low
                -- if there's a write: issue write, wait for write to go low
                -- simple, no?
                n_state <= GOOD;
            WHEN OTHERS =>
                n_state <= GOOD;
        END CASE;

    END PROCESS;
    PROCESS (rst, sys_clk)
    BEGIN

        IF rst = '1' THEN
            state <= GOOD;
        ELSIF rising_edge(sys_clk) THEN
            state <= n_state;
        END IF;
    END PROCESS;

END behavioural;