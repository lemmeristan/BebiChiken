LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

--use std.textio.all;

PACKAGE bebichiken IS

-- datatypes

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


  TYPE opcode_group_t IS (
    OPCODE_R_TYPE,
    OPCODE_I_TYPE,
    OPCODE_MEM_TYPE,
    OPCODE_BRANCH_TYPE,
    OPCODE_U_TYPE,
    OPCODE_INVALID);
  TYPE opcode_group_word_t IS ARRAY(opcode_group_t) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
  TYPE opcode_group_regidx_t IS ARRAY(opcode_group_t) OF STD_LOGIC_VECTOR(4 DOWNTO 0);
  TYPE opcode_group_width_t IS ARRAY(opcode_group_t) OF STD_LOGIC_VECTOR(1 DOWNTO 0);
  TYPE opcode_group_bit_t IS ARRAY(opcode_group_t) OF STD_LOGIC;



  TYPE lock_owner_t IS ARRAY(NATURAL RANGE <>) of opcode_group_t;

  -- components


  COMPONENT regfile_half IS
  GENERIC (
      entry_point : STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
  PORT (
      clk, rst                   : IN STD_LOGIC;
      rs1, rs2, rd                   : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
      rs1_data_out, rs2_data_out, pc : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      update_rd, update_pc                  : IN STD_LOGIC;
      rd_data_in, next_pc                 : IN STD_LOGIC_VECTOR(31 DOWNTO 0)

  );
END COMPONENT;

COMPONENT execunit IS
  GENERIC (operation : opcode_t);

  PORT (
      rst, clk : IN STD_LOGIC;

      we : IN STD_LOGIC;
      rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

      writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      next_pc : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      update_pc : OUT STD_LOGIC;
      rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
      --uses_rs1, uses_rs2, updates_rd, updates_pc, 
      busy, rdy, updates_rd : OUT STD_LOGIC
  );
END COMPONENT;

COMPONENT eu_mem IS
  PORT (
      rst, clk : IN STD_LOGIC;

      we : IN STD_LOGIC;
      rs1_data, rs2_data, instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

      writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      mem_we, mem_re : OUT STD_LOGIC;
      mem_wack, mem_rdy : IN STD_LOGIC;
      mem_wdata, mem_addr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      mem_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      mem_width : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);

      rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);

      busy, update_rd : OUT STD_LOGIC
  );
END COMPONENT;

COMPONENT eu_branch_type IS
  PORT (
      rst, clk : IN STD_LOGIC;

      we : IN STD_LOGIC;
      rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

      writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      next_pc : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
      busy, update_rd, update_pc : OUT STD_LOGIC

  );
END COMPONENT;

COMPONENT eu_i_type IS
  PORT (
      rst, clk : IN STD_LOGIC;

      we : IN STD_LOGIC;
      rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

      writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      next_pc : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
      busy, update_rd : OUT STD_LOGIC

  );
END COMPONENT;

COMPONENT eu_r_type IS
  PORT (
      rst, clk : IN STD_LOGIC;

      we : IN STD_LOGIC;
      rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

      writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      next_pc : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
      busy, update_rd : OUT STD_LOGIC

  );
END COMPONENT;

COMPONENT eu_u_type IS
  PORT (
      rst, clk : IN STD_LOGIC;

      we : IN STD_LOGIC;
      rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

      writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      next_pc : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

      rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
      busy, update_rd : OUT STD_LOGIC

  );
END COMPONENT;


-- functions
FUNCTION f_decode_opcode (
      instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
      RETURN opcode_t;

      FUNCTION f_decode_exec_unit (
      instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
      RETURN opcode_group_t;

      FUNCTION f_uses_rs1 (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN STD_LOGIC;

        FUNCTION f_uses_rs2 (
          instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
          RETURN STD_LOGIC;

          FUNCTION f_updates_rd (
      instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
      RETURN STD_LOGIC;

      FUNCTION f_updates_pc (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN STD_LOGIC;


  END PACKAGE bebichiken;


  package body bebichiken is

    FUNCTION f_decode_opcode (
      instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
      RETURN opcode_t IS
      VARIABLE opcode : STD_LOGIC_VECTOR(6 DOWNTO 0);
      VARIABLE funct3 : STD_LOGIC_VECTOR(2 DOWNTO 0);
      VARIABLE funct7 : STD_LOGIC_VECTOR(6 DOWNTO 0);
  BEGIN
      funct3 := instruction(14 DOWNTO 12);
      funct7 := instruction(31 DOWNTO 25);
      opcode := instruction(6 DOWNTO 0);

      IF opcode = "0110011" THEN

          IF funct3 = "000" THEN

              IF funct7 = "0000000" THEN
                  RETURN OPCODE_R_TYPE_ADD;
              END IF;

              IF funct7 = "0100000" THEN
                  RETURN OPCODE_R_TYPE_SUB;
              END IF;
          END IF;
          IF funct3 = "001" THEN
              RETURN OPCODE_R_TYPE_SLL;
          END IF;
          IF funct3 = "010" THEN
              RETURN OPCODE_R_TYPE_SLT;
          END IF;

          IF funct3 = "011" THEN
              RETURN OPCODE_R_TYPE_SLTU;
          END IF;

          IF funct3 = "100" THEN
              RETURN OPCODE_R_TYPE_XOR;
          END IF;

          IF funct3 = "101" THEN
              IF funct7 = "0000000" THEN
                  RETURN OPCODE_R_TYPE_SRL;
              END IF;

              IF funct7 = "0100000" THEN
                  RETURN OPCODE_R_TYPE_SRA;
              END IF;
          END IF;

          IF funct3 = "110" THEN
              RETURN OPCODE_R_TYPE_OR;
          END IF;

          IF funct3 = "111" THEN
              RETURN OPCODE_R_TYPE_AND;
          END IF;

          RETURN OPCODE_INVALID;
      END IF;

      IF opcode = "0010011" THEN -- I_TYPE
          --        RETURN OPCODE_I_TYPE; -- Register/Immediate (ADDI, ...)
          IF funct3 = "000" THEN
              RETURN OPCODE_I_TYPE_ADDI;
          END IF;

          IF funct3 = "001" THEN
              IF funct7 = "0000000" THEN
                  RETURN OPCODE_I_TYPE_SLLI;
              END IF;
          END IF;

          IF funct3 = "010" THEN
              RETURN OPCODE_I_TYPE_SLTI;
          END IF;
          IF funct3 = "011" THEN
              RETURN OPCODE_I_TYPE_SLTIU;
          END IF;

          IF funct3 = "100" THEN
              RETURN OPCODE_I_TYPE_XORI;
          END IF;

          IF funct3 = "101" THEN

              IF funct7 = "0000000" THEN
                  RETURN OPCODE_I_TYPE_SRLI;
              END IF;

              IF funct7 = "0100000" THEN
                  RETURN OPCODE_I_TYPE_SRAI;
              END IF;

          END IF;

          IF funct3 = "110" THEN
              RETURN OPCODE_I_TYPE_ORI;
          END IF;

          IF funct3 = "111" THEN
              RETURN OPCODE_I_TYPE_ANDI;
          END IF;

      END IF;

      IF opcode = "0000011" THEN
          RETURN OPCODE_I_TYPE_LOAD;
      END IF;

      IF opcode = "0100011" THEN
          RETURN OPCODE_S_TYPE; -- Store (SB, SH, SW)
      END IF;

      IF opcode = "1100011" THEN -- Branch
          IF funct3 = "000" THEN
              RETURN OPCODE_B_TYPE_BEQ;
          END IF;
          IF funct3 = "001" THEN
              RETURN OPCODE_B_TYPE_BNE;
          END IF;
          IF funct3 = "100" THEN
              RETURN OPCODE_B_TYPE_BLT;
          END IF;
          IF funct3 = "101" THEN
              RETURN OPCODE_B_TYPE_BGE;
          END IF;
          IF funct3 = "110" THEN
              RETURN OPCODE_B_TYPE_BLTU;
          END IF;
          IF funct3 = "111" THEN
              RETURN OPCODE_B_TYPE_BGEU;
          END IF;
          RETURN OPCODE_INVALID;
      END IF;

      IF opcode = "0110111" THEN
          RETURN OPCODE_U_TYPE_LUI; -- LUI
      END IF;

      IF opcode = "0010111" THEN
          RETURN OPCODE_U_TYPE_AUIPC; -- AUIPC
      END IF;

      IF opcode = "1101111" THEN
          RETURN OPCODE_J_TYPE_JAL; -- JAL
      END IF;

      IF opcode = "1100111" THEN
          RETURN OPCODE_J_TYPE_JALR; -- JALR
      END IF;

      RETURN OPCODE_INVALID;

  END;
  FUNCTION f_decode_exec_unit (
      instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
      RETURN opcode_group_t IS
      VARIABLE opcode : STD_LOGIC_VECTOR(6 DOWNTO 0);
      VARIABLE funct3 : STD_LOGIC_VECTOR(2 DOWNTO 0);
      VARIABLE funct7 : STD_LOGIC_VECTOR(6 DOWNTO 0);
  BEGIN
      funct3 := instruction(14 DOWNTO 12);
      funct7 := instruction(31 DOWNTO 25);
      opcode := instruction(6 DOWNTO 0);

      IF opcode = "0110011" THEN

          IF funct3 = "000" THEN

              IF funct7 = "0000000" THEN
                  RETURN OPCODE_R_TYPE;
              END IF;

              IF funct7 = "0100000" THEN
                  RETURN OPCODE_R_TYPE;
              END IF;
          END IF;
          IF funct3 = "001" THEN
              RETURN OPCODE_R_TYPE;
          END IF;
          IF funct3 = "010" THEN
              RETURN OPCODE_R_TYPE;
          END IF;

          IF funct3 = "011" THEN
              RETURN OPCODE_R_TYPE;
          END IF;

          IF funct3 = "100" THEN
              RETURN OPCODE_R_TYPE;
          END IF;

          IF funct3 = "101" THEN
              IF funct7 = "0000000" THEN
                  RETURN OPCODE_R_TYPE;
              END IF;

              IF funct7 = "0100000" THEN
                  RETURN OPCODE_R_TYPE;
              END IF;
          END IF;

          IF funct3 = "110" THEN
              RETURN OPCODE_R_TYPE;
          END IF;

          IF funct3 = "111" THEN
              RETURN OPCODE_R_TYPE;
          END IF;

          RETURN OPCODE_INVALID;
      END IF;

      IF opcode = "0010011" THEN -- I_TYPE
          --        RETURN OPCODE_I_TYPE; -- Register/Immediate (ADDI, ...)
          IF funct3 = "000" THEN
              RETURN OPCODE_I_TYPE;
          END IF;

          IF funct3 = "001" THEN
              IF funct7 = "0000000" THEN
                  RETURN OPCODE_I_TYPE;
              END IF;
          END IF;

          IF funct3 = "010" THEN
              RETURN OPCODE_I_TYPE;
          END IF;
          IF funct3 = "011" THEN
              RETURN OPCODE_I_TYPE;
          END IF;

          IF funct3 = "100" THEN
              RETURN OPCODE_I_TYPE;
          END IF;

          IF funct3 = "101" THEN

              IF funct7 = "0000000" THEN
                  RETURN OPCODE_I_TYPE;
              END IF;

              IF funct7 = "0100000" THEN
                  RETURN OPCODE_I_TYPE;
              END IF;

          END IF;

          IF funct3 = "110" THEN
              RETURN OPCODE_I_TYPE;
          END IF;

          IF funct3 = "111" THEN
              RETURN OPCODE_I_TYPE;
          END IF;

      END IF;

      IF opcode = "0000011" THEN
          RETURN OPCODE_MEM_TYPE;
      END IF;

      IF opcode = "0100011" THEN
          RETURN OPCODE_MEM_TYPE; -- Store (SB, SH, SW)
      END IF;

      IF opcode = "1100011" THEN -- Branch
          IF funct3 = "000" THEN
              RETURN OPCODE_BRANCH_TYPE;
          END IF;
          IF funct3 = "001" THEN
              RETURN OPCODE_BRANCH_TYPE;
          END IF;
          IF funct3 = "100" THEN
              RETURN OPCODE_BRANCH_TYPE;
          END IF;
          IF funct3 = "101" THEN
              RETURN OPCODE_BRANCH_TYPE;
          END IF;
          IF funct3 = "110" THEN
              RETURN OPCODE_BRANCH_TYPE;
          END IF;
          IF funct3 = "111" THEN
              RETURN OPCODE_BRANCH_TYPE;
          END IF;
          RETURN OPCODE_INVALID;
      END IF;

      IF opcode = "0110111" THEN
          RETURN OPCODE_U_TYPE; -- LUI
      END IF;

      IF opcode = "0010111" THEN
          RETURN OPCODE_U_TYPE; -- AUIPC
      END IF;

      IF opcode = "1101111" THEN
          RETURN OPCODE_BRANCH_TYPE; -- JAL
      END IF;

      IF opcode = "1100111" THEN
          RETURN OPCODE_BRANCH_TYPE; -- JALR
      END IF;

      RETURN OPCODE_INVALID;

  END;

  FUNCTION f_uses_rs1 (
      instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
      RETURN STD_LOGIC IS
      VARIABLE opcode : opcode_t;
  BEGIN

      IF instruction(19 DOWNTO 15) = "00000" THEN
          RETURN '0';
      END IF;

      opcode := f_decode_opcode(instruction);

      IF opcode = OPCODE_I_TYPE_LOAD OR opcode = OPCODE_S_TYPE OR opcode = OPCODE_J_TYPE_JALR
          OR opcode = OPCODE_B_TYPE_BEQ OR opcode = OPCODE_B_TYPE_BNE OR opcode = OPCODE_B_TYPE_BLT
          OR opcode = OPCODE_B_TYPE_BGE OR opcode = OPCODE_B_TYPE_BLTU OR opcode = OPCODE_B_TYPE_BGEU

          OR opcode = OPCODE_R_TYPE_ADD OR opcode = OPCODE_R_TYPE_SUB OR opcode = OPCODE_R_TYPE_SLL
          OR opcode = OPCODE_R_TYPE_SLT OR opcode = OPCODE_R_TYPE_SLTU OR opcode = OPCODE_R_TYPE_XOR
          OR opcode = OPCODE_R_TYPE_SRL OR opcode = OPCODE_R_TYPE_SRA OR opcode = OPCODE_R_TYPE_OR
          OR opcode = OPCODE_R_TYPE_AND
          OR opcode = OPCODE_I_TYPE_ADDI OR opcode = OPCODE_I_TYPE_SLLI OR opcode = OPCODE_I_TYPE_SLTI
          OR opcode = OPCODE_I_TYPE_SLTIU OR opcode = OPCODE_I_TYPE_XORI OR opcode = OPCODE_I_TYPE_SRLI
          OR opcode = OPCODE_I_TYPE_SRAI OR opcode = OPCODE_I_TYPE_ORI OR opcode = OPCODE_I_TYPE_ANDI
          THEN
          RETURN '1';
      END IF;

      RETURN '0';
  END;

  FUNCTION f_uses_rs2 (
      instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
      RETURN STD_LOGIC IS
      VARIABLE opcode : opcode_t;
  BEGIN

      IF instruction(24 DOWNTO 20) = "00000" THEN
          RETURN '0';
      END IF;

      opcode := f_decode_opcode(instruction);

      IF opcode = OPCODE_S_TYPE
          OR opcode = OPCODE_B_TYPE_BEQ OR opcode = OPCODE_B_TYPE_BNE OR opcode = OPCODE_B_TYPE_BLT
          OR opcode = OPCODE_B_TYPE_BGE OR opcode = OPCODE_B_TYPE_BLTU OR opcode = OPCODE_B_TYPE_BGEU
          OR opcode = OPCODE_R_TYPE_ADD OR opcode = OPCODE_R_TYPE_SUB OR opcode = OPCODE_R_TYPE_SLL
          OR opcode = OPCODE_R_TYPE_SLT OR opcode = OPCODE_R_TYPE_SLTU OR opcode = OPCODE_R_TYPE_XOR
          OR opcode = OPCODE_R_TYPE_SRL OR opcode = OPCODE_R_TYPE_SRA OR opcode = OPCODE_R_TYPE_OR
          OR opcode = OPCODE_R_TYPE_AND

          THEN
          RETURN '1';
      END IF;

      RETURN '0';
  END;

  FUNCTION f_updates_rd (
      instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
      RETURN STD_LOGIC IS
      VARIABLE opcode : opcode_t;
      VARIABLE i_rd : STD_LOGIC_VECTOR(4 DOWNTO 0);
  BEGIN
      IF instruction(11 DOWNTO 7) = "00000" THEN
          RETURN '0';
      END IF;

      opcode := f_decode_opcode(instruction);

      IF opcode = OPCODE_I_TYPE_LOAD
          OR opcode = OPCODE_U_TYPE_LUI
          OR opcode = OPCODE_U_TYPE_AUIPC
          OR opcode = OPCODE_J_TYPE_JAL
          OR opcode = OPCODE_J_TYPE_JALR
          OR opcode = OPCODE_R_TYPE_ADD OR opcode = OPCODE_R_TYPE_SUB OR opcode = OPCODE_R_TYPE_SLL
          OR opcode = OPCODE_R_TYPE_SLT OR opcode = OPCODE_R_TYPE_SLTU OR opcode = OPCODE_R_TYPE_XOR
          OR opcode = OPCODE_R_TYPE_SRL OR opcode = OPCODE_R_TYPE_SRA OR opcode = OPCODE_R_TYPE_OR
          OR opcode = OPCODE_R_TYPE_AND
          OR opcode = OPCODE_I_TYPE_ADDI OR opcode = OPCODE_I_TYPE_SLLI OR opcode = OPCODE_I_TYPE_SLTI
          OR opcode = OPCODE_I_TYPE_SLTIU OR opcode = OPCODE_I_TYPE_XORI OR opcode = OPCODE_I_TYPE_SRLI
          OR opcode = OPCODE_I_TYPE_SRAI OR opcode = OPCODE_I_TYPE_ORI OR opcode = OPCODE_I_TYPE_ANDI
          THEN
          RETURN '1';
      END IF;

      RETURN '0';
  END;

  FUNCTION f_updates_pc (
      instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
      RETURN STD_LOGIC IS
      VARIABLE opcode : opcode_t;
  BEGIN
      opcode := f_decode_opcode(instruction);

      IF opcode = OPCODE_J_TYPE_JAL
          OR opcode = OPCODE_J_TYPE_JALR
          OR opcode = OPCODE_B_TYPE_BEQ OR opcode = OPCODE_B_TYPE_BNE OR opcode = OPCODE_B_TYPE_BLT
          OR opcode = OPCODE_B_TYPE_BGE OR opcode = OPCODE_B_TYPE_BLTU OR opcode = OPCODE_B_TYPE_BGEU
          THEN
          RETURN '1';
      END IF;

      RETURN '0';
  END;
  FUNCTION f_shift_up (
      opcode : IN opcode_t;
      registers : IN STD_LOGIC_VECTOR(1023 DOWNTO 0)
  )
      RETURN STD_LOGIC_VECTOR IS
      VARIABLE ires : STD_LOGIC_VECTOR(1023 DOWNTO 0);
      VARIABLE found : STD_LOGIC;
  BEGIN
      ires := registers;
      found := '0';
      FOR op IN opcode_t LOOP
          ires := ires(991 DOWNTO 0) & X"00000000";
          IF op = opcode THEN
              RETURN ires;
          END IF;
      END LOOP;

      RETURN ires;

  END;

  end bebichiken;