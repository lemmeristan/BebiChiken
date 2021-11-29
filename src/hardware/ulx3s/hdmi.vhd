LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE Ieee.std_logic_unsigned.ALL; -- addition

USE IEEE.numeric_std.ALL;

ENTITY hdmi IS
    GENERIC (
        res_x : INTEGER := 640;
        res_y : INTEGER := 480;
        base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"D0000000"

    );
    PORT (

        rst, clk : IN STD_LOGIC;
        mem_addr, mem_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        mem_we, mem_re : IN STD_LOGIC;
        mem_width : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        mem_rdy, mem_wack : OUT STD_LOGIC;

        address_valid : OUT STD_LOGIC;

        gpdi_dp : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
        --gpdi_dn : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
    );
END hdmi;

ARCHITECTURE behavioural OF hdmi IS

    COMPONENT HDMI_test_hires IS
        PORT (
            pclk : IN STD_LOGIC;
            gpdi_dp : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
            GFX_X, GFX_Y : OUT STD_LOGIC_VECTOR(9 DOWNTO 0);
            red, green, blue : IN STD_LOGIC_VECTOR(7 DOWNTO 0)
        );
    END COMPONENT;

    TYPE memory_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL frame_buffer : memory_t((res_x * res_y / 128) - 1 DOWNTO 0);
    CONSTANT mem_lb : STD_LOGIC_VECTOR(31 DOWNTO 0) := base_address;
    SIGNAL mem_ub : STD_LOGIC_VECTOR(31 DOWNTO 0);-- := STD_LOGIC_VECTOR(to_unsigned((frame_buffer'high * 4), 32)) - X"00000001";
    SIGNAL i_address_valid : STD_LOGIC;
    SIGNAL idx : INTEGER;
    SIGNAL X, Y : STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL actual_x, actual_y : INTEGER;

    SIGNAL current_pixel : STD_LOGIC_VECTOR(31 DOWNTO 0);
BEGIN

    actual_x <= to_integer(unsigned(X)) / 128;
    actual_y <= to_integer(unsigned(Y)) / 128;

    current_pixel <= frame_buffer((actual_y * res_x) + actual_x) WHEN rst = '0' ELSE
    X"0000FF00";

    mem_ub <= STD_LOGIC_VECTOR(to_unsigned((res_x * res_y * 4) - 1, 32));

    gen : HDMI_test_hires PORT MAP(
        pclk => clk,
        gpdi_dp => gpdi_dp,
        GFX_X => X, GFX_Y => Y,
        red => current_pixel(23 DOWNTO 16),
        green => current_pixel(15 DOWNTO 8),
        blue => current_pixel(7 DOWNTO 0));

    writeToFrameBuffer : PROCESS (rst, clk)
    BEGIN
        IF rising_edge(clk) THEN
            mem_rdata <= (OTHERS => 'Z');
            IF i_address_valid = '1' THEN
                IF mem_we = '1' THEN
                    frame_buffer(idx/128) <= mem_wdata;
                    ELSIF mem_re = '1' THEN
                    mem_rdata <= frame_buffer(idx/128);
                END IF;
            END IF;
        END IF;
    END PROCESS;

    i_address_valid <= '1' WHEN (mem_addr >= mem_lb) AND (mem_addr <= mem_lb) AND (mem_width = "11") ELSE
    '0';
    address_valid <= i_address_valid;

    idx <= to_integer(unsigned((mem_addr - base_address)))/4;

    mem_rdy <= '1';
    mem_wack <= '1';

END behavioural;