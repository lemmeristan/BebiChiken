-- Author: Lemmer El Assal
-- Date: 23/01/2022 19:06 CET

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY fifo_generic IS
    GENERIC (
        vendor     : STD_LOGIC := '1';
        data_width : INTEGER   := 36
    );

    PORT (
        rst    : IN STD_LOGIC;
        wr_clk : IN STD_LOGIC;
        rd_clk : IN STD_LOGIC;
        din    : IN STD_LOGIC_VECTOR(data_width - 1 DOWNTO 0);
        wr_en  : IN STD_LOGIC;
        rd_en  : IN STD_LOGIC;
        dout   : OUT STD_LOGIC_VECTOR(data_width - 1 DOWNTO 0);
        full, afull   : OUT STD_LOGIC;
        empty  : OUT STD_LOGIC
    );
END fifo_generic;

ARCHITECTURE behavioural OF fifo_generic IS

    CONSTANT num_dpram                        : INTEGER := data_width / 18;
    SIGNAL rd_ptr, wr_ptr                     : STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL AddressA, AddressB                 : STD_LOGIC_VECTOR(13 DOWNTO 0);
    SIGNAL i_empty, i_full, i_afull, first_read, we_r : STD_LOGIC;

    SIGNAL i_dout, din_r : STD_LOGIC_VECTOR(data_width - 1 DOWNTO 0);

BEGIN

afull <= i_afull;
full <= i_full;
empty <= i_empty;

    AddressA <= wr_ptr & "0011";
    AddressB <= rd_ptr & "0011";

    gen_dpram : FOR i IN 0 TO num_dpram - 1 GENERATE
        xilinx : IF vendor = '0' GENERATE
            i_dp16kd : dpram_xilinx_18k
            PORT MAP(
                clka   => wr_clk,
                wea(0) => we_r,
                addra  => wr_ptr,
                dina   => din_r(((i + 1) * 18) - 1 DOWNTO i * 18),
                douta  => OPEN,
                clkb   => rd_clk,
                web(0) => '0',
                addrb  => rd_ptr,
                dinb => (OTHERS => '0'),
                doutb  => i_dout(((i + 1) * 18) - 1 DOWNTO i * 18)
            );
        END GENERATE xilinx;

        lattice : IF vendor = '1' GENERATE
            i_dp16kd : dp16k_wrapper
            PORT MAP(
                DataInA  => din_r(((i + 1) * 18) - 1 DOWNTO i * 18),
                DataInB => (OTHERS => '0'),
                AddressA => AddressA,
                AddressB => AddressB,

                ClockA   => wr_clk,
                ClockB   => rd_clk,
                ClockEnA => '1',
                ClockEnB => '1',
                WrA      => we_r,
                WrB      => '0',
                ResetA   => '0',
                ResetB   => '0',
                QA       => OPEN,
                QB       => i_dout(((i + 1) * 18) - 1 DOWNTO i * 18),
                CSA      => "000",
                CSB      => "000"
            );

        END GENERATE lattice;
    END GENERATE gen_dpram;
    PROCESS (rst, wr_clk)
    BEGIN
        IF rst = '1' THEN
            wr_ptr <= (OTHERS => '0');
            we_r   <= '0';
            din_r <= (others => '0');
        ELSIF rising_edge(wr_clk) THEN
            we_r <= wr_en;
            din_r <= din;
            IF (wr_en = '1') and (i_full = '0') THEN
                wr_ptr <= wr_ptr + "0000000001";
            END IF;
        END IF;
    END PROCESS;
    PROCESS (rst, rd_clk)
    BEGIN
        IF rst = '1' THEN
            rd_ptr <= (OTHERS => '0');
            dout <= (others => '0');
        ELSIF rising_edge(rd_clk) THEN
            IF (rd_en = '1') and (i_empty = '0') THEN
                dout <= i_dout;
                rd_ptr <= rd_ptr + "0000000001";
            END IF;
        END IF;
    END PROCESS;
    i_empty <= '1' WHEN rd_ptr = wr_ptr ELSE
        '0';
    i_afull <= '1' WHEN wr_ptr + "0000000011" = rd_ptr ELSE
        '0';
    i_full <= '1' WHEN wr_ptr + "0000000001" = rd_ptr ELSE
        '0';


END behavioural;