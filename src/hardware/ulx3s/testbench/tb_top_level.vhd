----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 12/13/2019 12:27:44 PM
-- Design Name: 
-- Module Name: tb_top_level - Behavioral
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

entity tb_top_level is
end tb_top_level;

architecture Behavioral of tb_top_level is

      component main PORT ( 
    CLK100MHZ : in std_logic;
    btn : in std_logic_vector(3 downto 0);
    led : out std_logic_vector(3 downto 0)
    );
    end component;
    
    signal clk, rst : std_logic := '1';
    signal led : std_logic_vector(3 downto 0);
    

begin

    sim: main PORT MAP (
      btn(3) => rst, btn(2) => rst, btn(1) => rst, btn(0) => rst, CLK100MHZ => clk, led => led
    );
    
    
clk <= not clk after 5 ns;
rst <= '0' after 100 ns;

end Behavioral;
