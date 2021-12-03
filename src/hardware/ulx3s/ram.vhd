LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_textio.ALL;

LIBRARY std;
USE std.textio.ALL;

USE IEEE.std_logic_unsigned.ALL; -- for arithmetic
USE IEEE.NUMERIC_STD.ALL; -- for unsigned()
ENTITY block_ram IS
    GENERIC (
        base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"80060000";
        ram_size : INTEGER := 128; --65536; --STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00010000";
        ram_file : STRING := "RAM.txt"
    );
    PORT (
        rst, clk : IN STD_LOGIC;
        mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        mem_we, mem_re : IN STD_LOGIC;
        mem_width : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        mem_rdy, mem_wack : OUT STD_LOGIC;

        address_valid : OUT STD_LOGIC

    );
END block_ram;

ARCHITECTURE Behavioral OF block_ram IS
    --TYPE memory_t IS ARRAY ((to_integer(unsigned(ram_size))) - 1 DOWNTO 0) OF STD_LOGIC_VECTOR (7 DOWNTO 0);
    TYPE memory_t IS ARRAY (ram_size - 1 DOWNTO 0) OF STD_LOGIC_VECTOR (7 DOWNTO 0);

    -- IMPURE FUNCTION InitRamFromFile (RamFileName : IN STRING) RETURN memory_t IS
    --     FILE RamFile : text IS IN RamFileName;
    --     VARIABLE RamFileLine : line;
    --     VARIABLE tempIn : STD_LOGIC_VECTOR(7 DOWNTO 0);
    --     VARIABLE RAM : memory_t;
    --     VARIABLE good : BOOLEAN; -- Status of the read operations
    --     VARIABLE i : INTEGER;
    -- BEGIN
    --     i := 0;
    --     FOR i IN 0 TO to_integer(unsigned(ram_size)) - 1 LOOP
    --         readline (RamFile, RamFileLine);
    --         Hread (RamFileLine, tempIn);
    --         RAM(i) := tempIn;
    --     END LOOP;
    --     RETURN RAM;
    -- END FUNCTION;

    SIGNAL memory : memory_t; -- := (0 => X"FF", 1 => X"EE", OTHERS => X"00");-- InitRomFromFile(rom_file);
    --InitRamFromFile(ram_file);
    SIGNAL i_address_valid, i_we, i_re, start_fsm : STD_LOGIC;
    SIGNAL i_addr, i_mem_addr : INTEGER RANGE 0 TO ram_size - 1 := 0; --to_integer(unsigned(ram_size)) - 1 := 0;

    TYPE state_t IS (IDLE, READ_OR_WRITE_B0, READ_OR_WRITE_B1, READ_OR_WRITE_B2, READ_OR_WRITE_B3, READ_OR_WRITE_B4, DONE);
    SIGNAL state, n_state : state_t;

    SIGNAL i_width : STD_LOGIC_VECTOR(1 DOWNTO 0);

    SIGNAL i_rdata, n_rdata : STD_LOGIC_VECTOR(31 DOWNTO 0);

    SIGNAL i_mem_wdata, i_mem_rdata : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL set_b0, set_b1, set_b2, set_b3 : STD_LOGIC;
    ---------------------------
    SIGNAL blockram_we, blockram_re : STD_LOGIC;
    SIGNAL blockram_addr : INTEGER RANGE 0 TO ram_size - 1 := 0; --to_integer(unsigned(ram_size)) - 1 := 0;
    SIGNAL blockram_rdata, blockram_wdata : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL ramsize_bit : STD_LOGIC_VECTOR(31 DOWNTO 0);

BEGIN

    ramsize_bit <= STD_LOGIC_VECTOR(to_unsigned(ram_size, ramsize_bit'length));
    blockram : PROCESS (rst, clk)
    BEGIN
        IF rising_edge(clk) THEN
            IF blockram_we = '1' THEN
                memory(blockram_addr) <= blockram_wdata;
            ELSIF blockram_re = '1' THEN
                blockram_rdata <= memory(blockram_addr);
            END IF;
        END IF;
    END PROCESS;
    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            i_width <= "00";
            i_we <= '0';
            i_re <= '0';
            i_addr <= 0;
            i_rdata <= (OTHERS => '0');
            state <= IDLE;
            i_mem_rdata <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            i_rdata <= n_rdata;
            state <= n_state;
            IF start_fsm = '1' THEN
                i_rdata <= (OTHERS => '0');
                i_width <= mem_width;
                i_we <= mem_we;
                i_re <= mem_re;
                i_addr <= to_integer(unsigned((mem_addr - base_address) AND (ramsize_bit - X"00000001")));
            END IF;
        END IF;
    END PROCESS;

    PROCESS (state, i_rdata, mem_we, mem_re, i_address_valid, i_addr, i_width, i_re, i_we, mem_wdata, blockram_rdata)
    BEGIN
        n_state <= state;
        n_rdata <= i_rdata;
        mem_rdy <= '0';
        mem_wack <= '0';
        start_fsm <= '0';

        i_mem_wdata <= (OTHERS => '0');
        i_mem_addr <= i_addr;
        set_b0 <= '0';
        set_b1 <= '0';
        set_b2 <= '0';
        set_b3 <= '0';

        blockram_addr <= i_addr;
        blockram_wdata <= (OTHERS => '0');
        blockram_we <= '0';
        blockram_re <= '0';
        n_rdata <= i_rdata;

        CASE state IS
            WHEN IDLE =>
                mem_wack <= 'Z';
                mem_rdy <= 'Z';
                IF (mem_we = '1' OR mem_re = '1') AND (i_address_valid = '1') THEN
                    mem_wack <= '0';
                    mem_rdy <= '0';
                    start_fsm <= '1';
                    n_state <= READ_OR_WRITE_B0;
                END IF;
            WHEN READ_OR_WRITE_B0 =>
                IF i_we = '1' THEN
                    blockram_we <= '1';
                    blockram_wdata <= mem_wdata(7 DOWNTO 0);
                ELSE
                    blockram_re <= '1';
                END IF;

                n_state <= READ_OR_WRITE_B1;
            WHEN READ_OR_WRITE_B1 =>
                blockram_addr <= i_addr + 1;
                IF i_we = '1' THEN
                    blockram_we <= '1';
                    blockram_wdata <= mem_wdata(15 DOWNTO 8);
                ELSE
                    blockram_re <= '1';
                    n_rdata(7 DOWNTO 0) <= blockram_rdata;
                END IF;

                n_state <= READ_OR_WRITE_B2;
                IF i_width = "00" THEN
                    n_state <= DONE;
                END IF;

            WHEN READ_OR_WRITE_B2 =>
                blockram_addr <= i_addr + 2;
                IF i_we = '1' THEN
                    blockram_we <= '1';
                    blockram_wdata <= mem_wdata(23 DOWNTO 16);
                ELSE
                    blockram_re <= '1';
                    n_rdata(15 DOWNTO 8) <= blockram_rdata;
                END IF;
                n_state <= READ_OR_WRITE_B3;
                IF i_width = "01" THEN
                    n_state <= DONE;
                END IF;
            WHEN READ_OR_WRITE_B3 =>
                blockram_addr <= i_addr + 3;
                IF i_we = '1' THEN
                    blockram_we <= '1';
                    blockram_wdata <= mem_wdata(31 DOWNTO 24);
                    n_state <= DONE;
                ELSE
                    blockram_re <= '1';
                    n_rdata(23 DOWNTO 16) <= blockram_rdata;
                    n_state <= READ_OR_WRITE_B4;
                END IF;

            WHEN READ_OR_WRITE_B4 =>
                blockram_re <= '1';
                n_rdata(31 DOWNTO 24) <= blockram_rdata;
                n_state <= DONE;
            WHEN DONE =>
                mem_rdy <= i_re;
                mem_wack <= i_we;
                IF mem_we = '0' AND mem_re = '0' THEN
                    n_state <= IDLE;
                END IF;
            WHEN OTHERS =>
                n_state <= IDLE;
        END CASE;
    END PROCESS;

    PROCESS (mem_addr, mem_width)
    BEGIN
        i_address_valid <= '0';
        IF (mem_addr >= base_address) AND (mem_addr < (base_address + ram_size)) THEN
            CASE mem_width IS
                WHEN "00" =>
                    i_address_valid <= '1';
                WHEN "01" => -- halfword access
                    IF mem_addr < (base_address + ramsize_bit - X"00000001") THEN
                        i_address_valid <= '1';
                    END IF;
                WHEN "10" => -- word access
                    IF mem_addr < (base_address + ramsize_bit - X"00000003") THEN
                        i_address_valid <= '1';
                    END IF;
                WHEN OTHERS =>
            END CASE;
        END IF;
    END PROCESS;

    mem_rdata <= i_rdata WHEN i_address_valid = '1' ELSE
        (OTHERS => 'Z');
    address_valid <= i_address_valid;

END Behavioral;