----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 12/23/2019 13:18:44 PM
-- Design Name: 
-- Module Name: tb_uart - Behavioral
-- Project Name: 
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


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity tb_spimaster is
end tb_spimaster;

architecture Behavioral of tb_spimaster is

      component spimaster PORT ( 
        rst, clk : in std_logic;
        sck, mosi : out std_logic;
        miso : in std_logic;
        mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
        mem_rdata : out std_logic_vector(31 downto 0);
        mem_we, mem_re : in std_logic;
        mem_wack, mem_rdy : out std_logic
    );
    end component;
    
    signal clk, rst, mem_we, mem_re, mem_rdy, mem_wack, miso, mosi, sck : std_logic := '1';
    signal mem_addr, mem_wdata, mem_rdata : std_logic_vector(31 downto 0);

    constant FLAG_OPEN_DRAIN_SCK : integer := 0;
    constant FLAG_OPEN_DRAIN_MOSI : integer := 1;
    constant FLAG_IDLE_SCK : integer := 2;

begin

clk <= not clk after 5 ns;
rst <= '0' after 100 ns;

    sim: spimaster PORT MAP (
        rst => rst, clk => clk,
        sck => sck, mosi => mosi, miso => mosi,-- miso,
        mem_addr => mem_addr, mem_wdata => mem_wdata,
        mem_rdata => mem_rdata,
        mem_we => mem_we, mem_re => mem_re,
        mem_rdy => mem_rdy, mem_wack => mem_wack
    );
    
    process
    begin
        mem_re <= '0';
        mem_we <= '0';
        wait for 100 ns;
        
        mem_addr <= X"C0003001"; -- divider
        mem_wdata <= X"00000000"; -- 390 kHz
        mem_we <= '1';
        wait for 40 ns;
        mem_we <= '0';
        wait for 10 ns;

        mem_addr <= X"C0003002"; -- config
        mem_wdata <= (FLAG_OPEN_DRAIN_SCK => '0', FLAG_OPEN_DRAIN_MOSI => '0', FLAG_IDLE_SCK => '1', others => '0');
        mem_we <= '1';
        wait for 40 ns;
        mem_we <= '0';
        wait for 10 ns;

        mem_addr <= X"C0003000";
        mem_wdata <= X"000000AA";
        mem_we <= '1';
        wait until mem_wack = '1';
        mem_we <= '0';
        wait for 1 us;

        mem_addr <= X"C0003000";
        mem_wdata <= X"00000055";
        mem_we <= '1';
        wait until mem_wack = '1';
        mem_we <= '0';
        wait for 100 ns;

        wait;
    end process;
 
end Behavioral;
