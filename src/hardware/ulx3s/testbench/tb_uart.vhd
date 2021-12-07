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

entity tb_uart is
end tb_uart;

architecture Behavioral of tb_uart is

      component uart PORT ( 
    CLK100MHZ : in std_logic;
    rst : in std_logic;
    txd : out std_logic
    );
    end component;
    
    signal clk, rst, txd : std_logic := '1';
    

begin

clk <= not clk after 5 ns;
rst <= '0' after 100 ns;

    sim: uart PORT MAP (
      CLK100MHZ => clk, rst => rst, txd => txd
    );
    
    

end Behavioral;
