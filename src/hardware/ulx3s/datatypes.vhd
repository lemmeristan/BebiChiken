LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

--use std.textio.all;

package bebichiken is

  type word_t is array (natural range <>) of std_logic_vector(31 downto 0);
  type word_array_t is array (natural range <>) of std_logic_vector(31 downto 0);
  
  type width_t is array (natural range <>) of std_logic_vector(1 downto 0);
  type width_array_t is array (natural range <>) of std_logic_vector(1 downto 0);
  
  type block_size_t is array (natural range <>) of std_logic_vector(12 downto 0);
  type block_size_array_t is array (natural range <>) of std_logic_vector(12 downto 0);
    
  type dpram_address_array_t is array (natural range <>) of std_logic_vector(10 downto 0);

  

end package bebichiken;