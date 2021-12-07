LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_textio.ALL;
USE IEEE.std_logic_unsigned.ALL;

LIBRARY std;
USE std.textio.ALL;
ENTITY rom IS
    GENERIC (
        base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00400000";
        rom_size : INTEGER := 128; -- 131072; --STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00020000";
        rom_file : STRING := "ROM.txt"
    );
    PORT (
        rst, clk : IN STD_LOGIC;
        addr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        re : IN STD_LOGIC
    );
END rom;

ARCHITECTURE Behavioral OF rom IS

    --    HAL_GPIO_SetDirections(0xFFFFFFFF);
    -- 80010984:	c00027b7          	lui	a5,0xc0002
    -- 80010988:	fff00713          	li	a4,-1
    -- 8001098c:	00e7a023          	sw	a4,0(a5) # c0002000 <__clzsi2+0x3ffebe4c>
    --     while (1)
    --     {
    --         HAL_GPIO_SetOutputs(0);
    -- 80010990:	c00027b7          	lui	a5,0xc0002
    -- 80010994:	00478793          	addi	a5,a5,4 # c0002004 <__clzsi2+0x3ffebe50>
    -- 80010998:	0007a023          	sw	zero,0(a5)
    --         HAL_GPIO_SetOutputs(1);
    -- 8001099c:	c00027b7          	lui	a5,0xc0002
    -- 800109a0:	00478793          	addi	a5,a5,4 # c0002004 <__clzsi2+0x3ffebe50>
    -- 800109a4:	00100713          	li	a4,1
    -- 800109a8:	00e7a023          	sw	a4,0(a5)
    --         HAL_GPIO_SetOutputs(0);
    -- 800109ac:	fe5ff06f          	j	80010990 <main+0x18>

    -- 800109b0 <_out_buffer>:
    -- } out_fct_wrap_type;

    TYPE memory_t IS ARRAY ((rom_size/4) - 1 DOWNTO 0) OF STD_LOGIC_VECTOR (31 DOWNTO 0);
    --(to_integer(unsigned(rom_size)/4) - 1 DOWNTO 0) OF STD_LOGIC_VECTOR (31 DOWNTO 0);
    -- IMPURE FUNCTION InitRomFromFile (RomFileName : IN STRING) RETURN memory_t IS
    --     FILE RomFile : text IS IN RomFileName;
    --     VARIABLE RomFileLine : line;
    --     VARIABLE tempIn : STD_LOGIC_VECTOR(31 DOWNTO 0);
    --     VARIABLE ROM : memory_t;
    --     VARIABLE good : BOOLEAN; -- Status of the read operations
    --     VARIABLE i : INTEGER;

    --     ATTRIBUTE syn_looplimit : INTEGER;
    --     ATTRIBUTE syn_looplimit OF readFileLoop : LABEL IS to_integer(unsigned(rom_size))/4;
    -- BEGIN
    --     --i := to_integer(unsigned(base_address)/4);

    --     i := 0;
    --     readFileLoop : WHILE NOT endfile(RomFile) LOOP
    --         readline (RomFile, RomFileLine);
    --         Hread (RomFileLine, tempIn);
    --         ROM(i) := tempIn;
    --         i := i + 1;
    --     END LOOP;
    --     RETURN ROM;
    -- END FUNCTION;
    CONSTANT memory : memory_t := (
        0 => X"c00027b7", -- start: lui	a5,0xc0002
        1 => X"00800713", -- li	a4, 4
        2 => X"00e7a023", -- sw	a4,0(a5) # c0002000
        3 => X"c00027b7", -- lui	a5,0xc0002
        4 => X"00478793", -- addi	a5,a5,4
        5 => X"0007a023", -- sw	zero,0(a5)
        6 => X"c00027b7", -- lui	a5,0xc0002
        7 => X"00478793", -- addi	a5,a5,4
        8 => X"00800713", -- li	a4,4
        9 => X"00e7a023", -- sw	a4,0(a5) # c0002004
        10 => X"fe5ff06f", -- j start
        OTHERS => X"00000013");-- InitRomFromFile(rom_file);

    SIGNAL addr_valid : STD_LOGIC;
    SIGNAL idx : INTEGER RANGE 0 TO 32767;
    SIGNAL relativeaddress : STD_LOGIC_VECTOR(31 DOWNTO 0);

    SIGNAL romsize_bit : STD_LOGIC_VECTOR(31 DOWNTO 0);

BEGIN
    romsize_bit <= STD_LOGIC_VECTOR(to_unsigned(rom_size, romsize_bit'length));

    relativeaddress <= (addr - base_address) AND (romsize_bit - X"00000001");
    idx <= to_integer(unsigned(relativeaddress))/4;
    addr_valid <= '1' WHEN (addr >= base_address) AND (addr < (base_address + rom_size)) ELSE
        '0';

    data_out <= memory(idx) WHEN addr_valid = '1' AND re = '1' ELSE
        (OTHERS => '0');

END Behavioral;