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

entity tb_i2cmaster is
end tb_i2cmaster;

architecture Behavioral of tb_i2cmaster is

      component i2cmaster PORT ( 
        rst, clk : in std_logic;
        scl, sda : inout std_logic;
        mem_addr, mem_wdata : in std_logic_vector(31 downto 0);
        mem_rdata : out std_logic_vector(31 downto 0);
        mem_we, mem_re : in std_logic;
        mem_wack, mem_rdy : out std_logic
    );
    end component;
    
    signal rst, mem_we, mem_re, mem_rdy, mem_wack, sda, scl : std_logic;
    signal mem_addr, mem_wdata, mem_rdata : std_logic_vector(31 downto 0);

    signal clk : std_logic := '1';

begin
clk <= not clk after 5 ns;

    sim: i2cmaster PORT MAP (
        rst => rst, clk => clk,
        scl => scl, sda => sda,
        mem_addr => mem_addr, mem_wdata => mem_wdata,
        mem_rdata => mem_rdata,
        mem_we => mem_we, mem_re => mem_re,
        mem_rdy => mem_rdy, mem_wack => mem_wack
    );
    
    process
    begin
        rst <= '1';
        sda <= 'H';


        mem_re <= '0';
        mem_we <= '0';
        wait for 100 ns;
        rst <= '0';

        mem_addr <= X"C0004001"; -- divider
        mem_wdata <= X"00000010"; -- 390 kHz
        mem_we <= '1';
        wait for 40 ns;
        mem_we <= '0';
        wait for 40 ns;


        -- start condition
        mem_addr <= X"C0004003";
        mem_wdata <= X"00000003";
        mem_we <= '1';
        wait for 100 ns;
--        wait until mem_wack = '1';
        mem_we <= '0';

        wait for 1 us;

        mem_addr <= X"C0004003";
        mem_wdata <= X"00000001";
        mem_we <= '1';
        wait for 100 ns;
--        wait until mem_wack = '1';
        mem_we <= '0';
        wait for 1 us;

        -- write device address + write bit

        mem_addr <= X"C0004000";
        mem_wdata <= X"000000AA";
        mem_we <= '1';
        wait for 100 ns;
        wait until mem_wack = '1';
        mem_we <= '0';
        wait for 1 us;

        -- read ACK

        sda <= 'L';

        mem_addr <= X"C0004002";
        mem_re <= '1';
        wait until mem_rdy = '1';
        mem_re <= '0';
        wait for 100 ns;


        wait;
    end process;
 
end Behavioral;
