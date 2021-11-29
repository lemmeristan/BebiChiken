library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_textio.all;

library std;
use std.textio.all;

use IEEE.std_logic_unsigned.all; -- for arithmetic
use IEEE.NUMERIC_STD.ALL; -- for unsigned()


entity block_ram is
	generic (
        base_address : std_logic_vector(31 downto 0) := X"80000000";
		ram_size : std_logic_vector(31 downto 0) := X"00010000";
		ram_file : string := "RAM.txt"
	);
    port ( 
        rst, clk : in std_logic;
        mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
        mem_rdata : out std_logic_vector(31 downto 0);
        mem_we, mem_re : in std_logic;
        mem_width : in std_logic_vector(1 downto 0);
        mem_rdy, mem_wack : out std_logic;

        address_valid : out std_logic

    );
    end block_ram;

architecture Behavioral of block_ram is
	type memory_t is array ((to_integer(unsigned(ram_size))) - 1 downto 0) of std_logic_vector (7 downto 0);
    impure function InitRamFromFile (RamFileName : in string) return memory_t is
        FILE RamFile : text is in RamFileName;
        variable RamFileLine : line;
        variable tempIn : std_logic_vector(7 downto 0);
        variable RAM : memory_t;
        variable good: boolean;   -- Status of the read operations
        variable i : integer;
    begin
        i := 0;
        for i in 0 to to_integer(unsigned(ram_size))-1 loop
            readline (RamFile, RamFileLine);
            hread (RamFileLine, tempIn);
            RAM(i) := tempIn;
        end loop;
        return RAM;
    end function;
	
    signal memory : memory_t := InitRamFromFile(ram_file);
    signal i_address_valid, i_we, i_re, start_fsm : std_logic;
    signal i_addr, i_mem_addr : integer range 0 to to_integer(unsigned(ram_size))-1 := 0;

    type state_t is (IDLE, READ_OR_WRITE_B0, READ_OR_WRITE_B1, READ_OR_WRITE_B2, READ_OR_WRITE_B3, READ_OR_WRITE_B4, DONE);
    signal state, n_state : state_t;
    
    signal i_width : std_logic_vector(1 downto 0);

    signal i_rdata, n_rdata : std_logic_vector(31 downto 0);
    
    signal i_mem_wdata, i_mem_rdata : std_logic_vector(7 downto 0);

    signal set_b0, set_b1, set_b2, set_b3 : std_logic;
---------------------------
    signal blockram_we, blockram_re : std_logic;
    signal blockram_addr : integer range 0 to to_integer(unsigned(ram_size))-1 := 0;
    signal blockram_rdata, blockram_wdata : std_logic_vector(7 downto 0);

begin


    blockram: process(rst, clk)
    begin
        if rising_edge(clk) then
            if blockram_we = '1' then
                memory(blockram_addr) <= blockram_wdata;
            elsif blockram_re = '1' then
                blockram_rdata <= memory(blockram_addr);
            end if;
        end if;
    end process;


    process(rst, clk)
    begin
        if rst = '1' then
            i_width <= "00";
            i_we <= '0';
            i_re <= '0';
            i_addr <= 0;
            i_rdata <= (others => '0');
            state <= IDLE;
            i_mem_rdata <= (others => '0');
        elsif rising_edge(clk) then
            i_rdata <= n_rdata;
            state <= n_state;
            if start_fsm = '1' then
                i_rdata <= (others => '0');
                i_width <= mem_width;
                i_we <= mem_we;
                i_re <= mem_re;
                i_addr <= to_integer(unsigned((mem_addr-base_address) and (ram_size-X"00000001")));
            end if;


        end if;
    end process;

    process(state, i_rdata, mem_we, mem_re, i_address_valid, i_addr, i_width, i_re, i_we, mem_wdata, blockram_rdata)
    begin
        n_state <= state;
        n_rdata <= i_rdata;
        mem_rdy <= '0';
        mem_wack <= '0';
        start_fsm <= '0';

        i_mem_wdata <= (others => '0');
        i_mem_addr <= i_addr;
        set_b0 <= '0';
        set_b1 <= '0';
        set_b2 <= '0';
        set_b3 <= '0';

        blockram_addr <= i_addr;
        blockram_wdata <= (others => '0');
        blockram_we <= '0';
        blockram_re <= '0';
        n_rdata <= i_rdata;

        case state is
            when IDLE =>
                mem_wack <= 'Z';
                mem_rdy <= 'Z';
                if (mem_we = '1' or mem_re = '1') and (i_address_valid = '1') then
                    mem_wack <= '0';
                    mem_rdy <= '0';
                    start_fsm <= '1';
                    n_state <= READ_OR_WRITE_B0;
                end if;
            when READ_OR_WRITE_B0 =>
                if i_we = '1' then
                    blockram_we <= '1';
                    blockram_wdata <= mem_wdata(7 downto 0);
                else
                    blockram_re <= '1';
                end if;

                n_state <= READ_OR_WRITE_B1;
            when READ_OR_WRITE_B1 =>
                blockram_addr <= i_addr+1;
                if i_we = '1' then
                    blockram_we <= '1';
                    blockram_wdata <= mem_wdata(15 downto 8);
                else
                    blockram_re <= '1';
                    n_rdata(7 downto 0) <= blockram_rdata;
                end if;
                
                n_state <= READ_OR_WRITE_B2;
                if i_width = "00" then
                    n_state <= DONE;
                end if;

            when READ_OR_WRITE_B2 =>
                blockram_addr <= i_addr+2;
                if i_we = '1' then
                    blockram_we <= '1';
                    blockram_wdata <= mem_wdata(23 downto 16);
                else
                    blockram_re <= '1';
                    n_rdata(15 downto 8) <= blockram_rdata;
                end if;
                n_state <= READ_OR_WRITE_B3;
                if i_width = "01" then
                    n_state <= DONE;
                end if;
            when READ_OR_WRITE_B3 =>
                blockram_addr <= i_addr+3;
                if i_we = '1' then
                    blockram_we <= '1';
                    blockram_wdata <= mem_wdata(31 downto 24);
                    n_state <= DONE;
                else
                    blockram_re <= '1';
                    n_rdata(23 downto 16) <= blockram_rdata;
                    n_state <= READ_OR_WRITE_B4;
                end if;
                
            when READ_OR_WRITE_B4 =>
                blockram_re <= '1';
                n_rdata(31 downto 24) <= blockram_rdata;
                n_state <= DONE;
            when DONE =>
                mem_rdy <= i_re;
                mem_wack <= i_we;
                if mem_we = '0' and mem_re = '0' then
                    n_state <= IDLE;
                end if;
            when others =>
                n_state <= IDLE;
        end case;
    end process;

    process(mem_addr, mem_width)
    begin
        i_address_valid <= '0';
        if (mem_addr >= base_address) and (mem_addr < (base_address + ram_size)) then
            case mem_width is
                when "00" =>
                    i_address_valid <= '1';
                when "01" => -- halfword access
                    if mem_addr < (base_address + ram_size - X"00000001") then
                        i_address_valid <= '1';
                    end if;
                when "10" => -- word access
                    if mem_addr < (base_address + ram_size - X"00000003") then
                        i_address_valid <= '1';
                    end if;
                when others =>
            end case;
        end if;
    end process;

mem_rdata <= i_rdata when i_address_valid = '1' else (others => 'Z');
address_valid <= i_address_valid;

end Behavioral;