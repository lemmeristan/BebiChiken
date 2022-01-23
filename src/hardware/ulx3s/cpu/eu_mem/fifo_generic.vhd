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
        vendor : std_logic := '1';
        data_width : integer := 36
        );

    PORT (
        rst : IN STD_LOGIC;
        wr_clk : IN STD_LOGIC;
        rd_clk : IN STD_LOGIC;
        din : IN STD_LOGIC_VECTOR(data_width-1 DOWNTO 0);
        wr_en : IN STD_LOGIC;
        rd_en : IN STD_LOGIC;
        dout : OUT STD_LOGIC_VECTOR(data_width-1 DOWNTO 0);
        full : OUT STD_LOGIC;
        empty : OUT STD_LOGIC
    );
END fifo_generic;

ARCHITECTURE behavioural OF fifo_generic IS

CONSTANT num_dpram : integer := data_width / 18;
signal rd_ptr, wr_ptr : std_logic_vector(9 downto 0);
signal AddressA, AddressB : std_logic_vector(13 downto 0);
signal i_empty, i_afull : std_logic;

begin

    AddressA <= wr_ptr & "1111";
    AddressB <= rd_ptr & "1111";

    gen_dpram : FOR i IN 0 to num_dpram-1 GENERATE

    lattice : IF vendor = '1' GENERATE


        i_dp16kd : dp16k_wrapper  
        PORT MAP (
            DataInA => din(((i+1) * 18)-1 downto i*18),
            DataInB => (others => '0'),
            AddressA => AddressA,
            AddressB => AddressB,
        
            ClockA => wr_clk,
            ClockB => rd_clk,
            ClockEnA => '1',
            ClockEnB => '1',
            WrA => wr_en,
            WrB => '0',
            ResetA => '0',
            ResetB => '0',
            QA => open,
            QB => dout(((i+1) * 18)-1 downto i*18),
            CSA => "000",
            CSB => "000"
        );

    END GENERATE lattice;


    xilinx : IF vendor = '0' GENERATE
        -- inst_dpram_rs1 : dpram_regfile_xilinx
        -- PORT MAP(
        --     clka   => clk,
        --     wea(0) => '0',
        --     addra  => rs1,
        --     dina   => X"00000000",
        --     douta  => rs1_data_out_of_op(op),
        --     clkb   => clk,
        --     web(0) => writeback_we(op),
        --     addrb  => writeback_rd(op),
        --     dinb   => writeback_data(op),
        --     doutb  => OPEN
        -- );


    END GENERATE xilinx;

END GENERATE gen_dpram;


    process(rst, wr_clk)
    begin
        if rst = '1' then
            wr_ptr <= (others => '0');
        elsif rising_edge(wr_clk) then
            if wr_en = '1' then
                wr_ptr <= wr_ptr + "0000000001";
            end if;
        end if;
    end process;


    process(rst, rd_clk)
    begin
        if rst = '1' then
            rd_ptr <= (others => '0');
        elsif rising_edge(rd_clk) then
            if rd_en = '1' then
                rd_ptr <= rd_ptr + "0000000001";
            end if;
        end if;
    end process;


    empty <= '1' when rd_ptr = wr_ptr else '0';
    i_afull <= '1' when wr_ptr + "0000000010" = rd_ptr else '0';
    full <= '1' when wr_ptr + "0000000001" = rd_ptr else '0';




end behavioural;