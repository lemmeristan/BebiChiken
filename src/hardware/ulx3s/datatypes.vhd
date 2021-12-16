LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

--use std.textio.all;

PACKAGE bebichiken IS

  TYPE word_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
  TYPE word_array_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(31 DOWNTO 0);

  TYPE width_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(1 DOWNTO 0);
  TYPE width_array_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(1 DOWNTO 0);

  TYPE block_size_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(12 DOWNTO 0);
  TYPE block_size_array_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(12 DOWNTO 0);

  TYPE dpram_address_array_t IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(10 DOWNTO 0);
  TYPE peripherals_t IS (PERIPH_UART, PERIPH_SDRAM, PERIPH_INVALID);
  TYPE peripheral_address_t IS ARRAY(NATURAL RANGE <>) OF peripherals_t;
  TYPE peripheral_word_t IS ARRAY (peripherals_t) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
  TYPE peripheral_width_t IS ARRAY (peripherals_t) OF STD_LOGIC_VECTOR(1 DOWNTO 0);
  TYPE peripheral_bit_t IS ARRAY (peripherals_t) OF STD_LOGIC;
  TYPE opcode_t IS (
    OPCODE_R_TYPE_ADD, OPCODE_R_TYPE_SUB, OPCODE_R_TYPE_SLL, OPCODE_R_TYPE_SLT, OPCODE_R_TYPE_SLTU, OPCODE_R_TYPE_XOR, OPCODE_R_TYPE_SRL, OPCODE_R_TYPE_SRA, OPCODE_R_TYPE_OR, OPCODE_R_TYPE_AND,
    OPCODE_I_TYPE_ADDI, OPCODE_I_TYPE_SLLI, OPCODE_I_TYPE_SLTI, OPCODE_I_TYPE_SLTIU, OPCODE_I_TYPE_XORI, OPCODE_I_TYPE_SRLI, OPCODE_I_TYPE_SRAI, OPCODE_I_TYPE_ORI, OPCODE_I_TYPE_ANDI, OPCODE_I_TYPE_LOAD,
    OPCODE_S_TYPE,
    OPCODE_B_TYPE_BEQ, OPCODE_B_TYPE_BNE, OPCODE_B_TYPE_BLT, OPCODE_B_TYPE_BGE, OPCODE_B_TYPE_BLTU, OPCODE_B_TYPE_BGEU,
    OPCODE_U_TYPE_LUI, OPCODE_U_TYPE_AUIPC,
    OPCODE_J_TYPE_JAL, OPCODE_J_TYPE_JALR,
    OPCODE_INVALID);
  TYPE opcode_word_t IS ARRAY(opcode_t) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
  TYPE opcode_regidx_t IS ARRAY(opcode_t) OF STD_LOGIC_VECTOR(4 DOWNTO 0);
  TYPE opcode_width_t IS ARRAY(opcode_t) OF STD_LOGIC_VECTOR(1 DOWNTO 0);
  TYPE opcode_bit_t IS ARRAY(opcode_t) OF STD_LOGIC;


  TYPE lock_owner_t IS ARRAY(NATURAL RANGE <>) of opcode_t;





  END PACKAGE bebichiken;