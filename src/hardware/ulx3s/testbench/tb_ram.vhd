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

entity tb_ram is
end tb_ram;

architecture Behavioral of tb_ram is

      component block_ram PORT ( 
        rst, clk : in std_logic;
        mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
        mem_rdata : out std_logic_vector(31 downto 0);
        mem_we, mem_re : in std_logic;
        mem_width : in std_logic_vector(1 downto 0);
        mem_rdy, mem_wack : out std_logic
    );
    end component;
    
    signal clk, rst, mem_we, mem_re, mem_rdy, mem_wack : std_logic := '1';
    signal mem_addr, mem_wdata, mem_rdata : std_logic_vector(31 downto 0);
    signal mem_width : std_logic_vector(1 downto 0);

begin

clk <= not clk after 5 ns;
rst <= '0' after 100 ns;

    sim: block_ram PORT MAP (
        rst => rst, clk => clk,
        mem_addr => mem_addr, mem_wdata => mem_wdata,
        mem_rdata => mem_rdata,
        mem_we => mem_we, mem_re => mem_re,
        mem_width => mem_width,
        mem_rdy => mem_rdy, mem_wack => mem_wack
    );
    
    process
    begin
        mem_re <= '0';
        mem_we <= '0';

        wait for 100 ns;
        mem_width <= "10";
        mem_addr <= X"80000000";
        mem_wdata <= X"AA558877";
        mem_we <= '1';
        wait until mem_wack = '1';
        mem_we <= '0';
        wait for 100 ns;
        mem_re <= '1';
        wait until mem_rdy = '1';
        mem_re <= '0';
        wait for 100 ns;

        mem_width <= "01";
        mem_addr <= X"80000000";
        mem_wdata <= X"000066AA";
        mem_we <= '1';
        wait until mem_wack = '1';
        mem_we <= '0';
        wait for 100 ns;
        mem_re <= '1';
        wait until mem_rdy = '1';
        mem_re <= '0';
        wait for 100 ns;

        mem_width <= "00";
        mem_addr <= X"80000000";
        mem_wdata <= X"000066AA";
        mem_we <= '1';
        wait until mem_wack = '1';
        mem_we <= '0';
        wait for 100 ns;
        mem_re <= '1';
        wait until mem_rdy = '1';
        mem_re <= '0';
        wait for 100 ns;

        wait;
    end process;
 
end Behavioral;
