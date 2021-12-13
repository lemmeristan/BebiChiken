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

  
  type peripherals_t is (PERIPH_UART, PERIPH_SDRAM, PERIPH_INVALID);
  type peripheral_address_t is array(natural range <>) of peripherals_t;
  type peripheral_word_t is array (peripherals_t) of std_logic_vector(31 downto 0);
  type peripheral_width_t is array (peripherals_t) of std_logic_vector(1 downto 0);
  type peripheral_bit_t is array (peripherals_t) of std_logic;


  TYPE opcode_t is (OPCODE_R_TYPE, OPCODE_I_TYPE, OPCODE_I_TYPE_LOAD, OPCODE_S_TYPE, OPCODE_B_TYPE, OPCODE_U_TYPE_LUI, OPCODE_U_TYPE_AUIPC, OPCODE_J_TYPE_JAL, OPCODE_J_TYPE_JALR, OPCODE_INVALID);
  type opcode_word_t is array(opcode_t) of std_logic_vector(31 downto 0);
  type opcode_width_t is array(opcode_t) of std_logic_vector(1 downto 0);
  type opcode_bit_t is array(opcode_t) of std_logic;
  

end package bebichiken;