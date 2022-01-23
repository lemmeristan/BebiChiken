library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_textio.all;
use IEEE.std_logic_unsigned.all;

library std;
use std.textio.all;


entity rom is
    generic (
        base_address : std_logic_vector(31 downto 0) := X"80010000";
		rom_size : std_logic_vector(31 downto 0) := X"00020000";
		rom_file : string := "ROM.txt"
	);
    port ( 
        rst, clk : in std_logic;
        addr : in std_logic_vector(31 downto 0);
        data_out : out std_logic_vector(31 downto 0)
    );
    end rom;

architecture Behavioral of rom is
    
	type memory_t is array (to_integer(unsigned(rom_size)/4) - 1 downto 0) of std_logic_vector (31 downto 0);
    impure function InitRomFromFile (RomFileName : in string) return memory_t is
        FILE RomFile : text is in RomFileName;
        variable RomFileLine : line;
        variable tempIn : std_logic_vector(31 downto 0);
        variable ROM : memory_t;
        variable good: boolean;   -- Status of the read operations
        variable i : integer;

        attribute syn_looplimit : integer;
        attribute syn_looplimit of readFileLoop : label is to_integer(unsigned(rom_size))/4;
    begin
        --i := to_integer(unsigned(base_address)/4);
        
        i := 0;
        readFileLoop: while not endfile(RomFile) loop
            readline (RomFile, RomFileLine);
            hread (RomFileLine, tempIn);
            ROM(i) := tempIn;
            i := i + 1;
        end loop;
        return ROM;
    end function;
    constant memory : memory_t := InitRomFromFile(rom_file);

    signal addr_valid : std_logic;
    signal idx : integer range 0 to 32767;
    signal relativeaddress : std_logic_vector(31 downto 0);

begin

    relativeaddress <= (addr - base_address) and (rom_size-X"00000001");


    idx <= to_integer(unsigned(relativeaddress))/4;
    addr_valid <= '1' when (addr >= base_address) and (addr < (base_address+rom_size)) else '0';

    data_out <= memory(idx);

end Behavioral;