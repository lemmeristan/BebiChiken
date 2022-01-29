LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;        -- signed / unsigned
USE IEEE.std_logic_unsigned.ALL; -- addition
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
        --OPCODE_R_TYPE,
        --OPCODE_I_TYPE,
        OPCODE_ALU_TYPE,
        OPCODE_MEM_TYPE,
        OPCODE_BRANCH_TYPE,
        --OPCODE_U_TYPE,
        OPCODE_INVALID);
    TYPE opcode_group_word_t IS ARRAY(opcode_group_t) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
    TYPE opcode_group_wb_word_t IS ARRAY(opcode_group_t) OF STD_LOGIC_VECTOR(32 DOWNTO 0);

    TYPE opcode_group_regidx_t IS ARRAY(opcode_group_t) OF STD_LOGIC_VECTOR(4 DOWNTO 0);
    TYPE opcode_group_width_t IS ARRAY(opcode_group_t) OF STD_LOGIC_VECTOR(1 DOWNTO 0);
    TYPE opcode_group_bit_t IS ARRAY(opcode_group_t) OF STD_LOGIC;
    TYPE opcode_group_bit_as_vector_t IS ARRAY(opcode_group_t) OF STD_LOGIC_VECTOR(0 DOWNTO 0);
    TYPE lock_owner_t IS ARRAY(NATURAL RANGE <>) OF opcode_group_t;

    -- components


    COMPONENT fifo_generic
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
        full, afull : OUT STD_LOGIC;
        empty : OUT STD_LOGIC
    );
END COMPONENT;


COMPONENT dpram_xilinx_18k
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(17 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(17 DOWNTO 0);
    clkb : IN STD_LOGIC;
    web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(17 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(17 DOWNTO 0)
  );
END COMPONENT;

    COMPONENT dp16k_wrapper
        PORT (
            DataInA  : IN STD_LOGIC_VECTOR(17 DOWNTO 0);
            DataInB  : IN STD_LOGIC_VECTOR(17 DOWNTO 0);
            AddressA : IN STD_LOGIC_VECTOR(13 DOWNTO 0);
            AddressB : IN STD_LOGIC_VECTOR(13 DOWNTO 0);
            ClockA   : IN STD_LOGIC;
            ClockB   : IN STD_LOGIC;
            ClockEnA : IN STD_LOGIC;
            ClockEnB : IN STD_LOGIC;
            WrA      : IN STD_LOGIC;
            WrB      : IN STD_LOGIC;
            ResetA   : IN STD_LOGIC;
            ResetB   : IN STD_LOGIC;
            QA       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0);
            QB       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0);

            CSA : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
            CSB : IN STD_LOGIC_VECTOR(2 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT fifo_dc_36_xilinx
        PORT (
            rst    : IN STD_LOGIC;
            wr_clk : IN STD_LOGIC;
            rd_clk : IN STD_LOGIC;
            din    : IN STD_LOGIC_VECTOR(35 DOWNTO 0);
            wr_en  : IN STD_LOGIC;
            rd_en  : IN STD_LOGIC;
            dout   : OUT STD_LOGIC_VECTOR(35 DOWNTO 0);
            full   : OUT STD_LOGIC;
            empty  : OUT STD_LOGIC
        );
    END COMPONENT;

    COMPONENT fifo_dc_144_xilinx
        PORT (
            rst    : IN STD_LOGIC;
            wr_clk : IN STD_LOGIC;
            rd_clk : IN STD_LOGIC;
            din    : IN STD_LOGIC_VECTOR(143 DOWNTO 0);
            wr_en  : IN STD_LOGIC;
            rd_en  : IN STD_LOGIC;
            dout   : OUT STD_LOGIC_VECTOR(143 DOWNTO 0);
            full   : OUT STD_LOGIC;
            empty  : OUT STD_LOGIC
        );
    END COMPONENT;

    COMPONENT fifo_dc_144_lattice PORT (
        Data    : IN STD_LOGIC_VECTOR(143 DOWNTO 0);
        WrClock : IN STD_LOGIC;
        RdClock : IN STD_LOGIC;
        WrEn    : IN STD_LOGIC;
        RdEn    : IN STD_LOGIC;
        Reset   : IN STD_LOGIC;
        RPReset : IN STD_LOGIC;
        Q       : OUT STD_LOGIC_VECTOR(143 DOWNTO 0);
        Empty   : OUT STD_LOGIC;
        Full    : OUT STD_LOGIC
        );
    END COMPONENT;
    COMPONENT fifo_dc_36_lattice PORT (
        Data    : IN STD_LOGIC_VECTOR(35 DOWNTO 0);
        WrClock : IN STD_LOGIC;
        RdClock : IN STD_LOGIC;
        WrEn    : IN STD_LOGIC;
        RdEn    : IN STD_LOGIC;
        Reset   : IN STD_LOGIC;
        RPReset : IN STD_LOGIC;
        Q       : OUT STD_LOGIC_VECTOR(35 DOWNTO 0);
        Empty   : OUT STD_LOGIC;
        Full    : OUT STD_LOGIC
        );
    END COMPONENT;
    COMPONENT dpram_regfile_xilinx
        PORT (
            clka  : IN STD_LOGIC;
            wea   : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
            addra : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            dina  : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            douta : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            clkb  : IN STD_LOGIC;
            web   : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
            addrb : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            dinb  : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            doutb : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    COMPONENT dpram_regfile_lattice PORT (
        DataInA  : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        DataInB  : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        AddressA : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        AddressB : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        ClockA   : IN STD_LOGIC;
        ClockB   : IN STD_LOGIC;
        ClockEnA : IN STD_LOGIC;
        ClockEnB : IN STD_LOGIC;
        WrA      : IN STD_LOGIC;
        WrB      : IN STD_LOGIC;
        ResetA   : IN STD_LOGIC;
        ResetB   : IN STD_LOGIC;
        QA       : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        QB       : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT regfile_half IS
        GENERIC (
            entry_point : STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
        PORT (
            clk, rst                       : IN STD_LOGIC;
            rs1, rs2, rd                   : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            rs1_data_out, rs2_data_out, pc : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            update_rd, update_pc           : IN STD_LOGIC;
            rd_data_in, next_pc            : IN STD_LOGIC_VECTOR(31 DOWNTO 0)

        );
    END COMPONENT;
    COMPONENT regfile_reduced IS
        GENERIC (
            entry_point : STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
        PORT (
            rst, clk          : IN STD_LOGIC;
            rs1, rs2, rd      : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            lock_rd           : IN STD_LOGIC;
            new_rd_lock_owner : IN opcode_group_t;
            lock_token        : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

            writeback_we    : IN opcode_group_bit_t;
            writeback_data  : IN opcode_group_word_t;
            writeback_token : IN opcode_group_word_t;
            writeback_rd    : IN opcode_group_regidx_t;

            rs1_data_out, rs2_data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            rs1_locked, rs2_locked     : OUT STD_LOGIC

        );
    END COMPONENT;
    COMPONENT regfile_dpram IS
        GENERIC (
            entry_point : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00000000";
            vendor      : STD_LOGIC                     := '0'
        );
        PORT (
            rst, clk          : IN STD_LOGIC;
            rs1, rs2, rd      : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            lock_rd           : IN STD_LOGIC;
            new_rd_lock_owner : IN opcode_group_t;
            lock_token        : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

            writeback_we    : IN opcode_group_bit_t;
            writeback_data  : IN opcode_group_word_t;
            writeback_token : IN opcode_group_word_t;
            writeback_rd    : IN opcode_group_regidx_t;

            rs1_data_out, rs2_data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            rs1_locked, rs2_locked     : OUT STD_LOGIC

        );
    END COMPONENT;
    COMPONENT execunit IS
        GENERIC (operation : opcode_t);

        PORT (
            rst, clk : IN STD_LOGIC;

            we                                  : IN STD_LOGIC;
            rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

            writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

            next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            update_pc : OUT STD_LOGIC;
            rd        : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
            --uses_rs1, uses_rs2, updates_rd, updates_pc, 
            busy, rdy, updates_rd : OUT STD_LOGIC
        );
    END COMPONENT;

    COMPONENT eu_mem IS
    GENERIC (
        vendor : STD_LOGIC := '0'
    );
        PORT (
            rst, clk : IN STD_LOGIC;

            we                                          : IN STD_LOGIC;
            rs1_data, rs2_data, instruction, token, imm : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

            writeback_data, writeback_token : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            writeback_we                    : OUT STD_LOGIC;
            writeback_rd                    : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);

            mem_we, mem_re      : OUT STD_LOGIC;
            mem_wack, mem_rdy   : IN STD_LOGIC;
            mem_wdata, mem_addr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            mem_rdata           : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            mem_width           : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);

            busy : OUT STD_LOGIC
        );
    END COMPONENT;

    COMPONENT eu_branch_type IS
        PORT (
            rst, clk : IN STD_LOGIC;

            we                                              : IN STD_LOGIC;
            rs1_data, rs2_data, instruction, pc, token, imm : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

            writeback_next_pc, writeback_data, writeback_token : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            writeback_we, writeback_update_pc                  : OUT STD_LOGIC;
            writeback_rd                                       : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);

            busy : OUT STD_LOGIC

        );
    END COMPONENT;

    COMPONENT eu_i_type IS
        PORT (
            rst, clk : IN STD_LOGIC;

            we                                : IN STD_LOGIC;
            rs1_data, instruction, token, imm : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

            writeback_data, writeback_token : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            writeback_we                    : OUT STD_LOGIC;
            writeback_rd                    : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);

            busy : OUT STD_LOGIC

        );
    END COMPONENT;

    COMPONENT eu_alu IS
        PORT (
            rst, clk : IN STD_LOGIC;

            we                                          : IN STD_LOGIC;
            rs1_data, rs2_data, instruction, token, imm : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

            writeback_data, writeback_token : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            writeback_we                    : OUT STD_LOGIC;
            writeback_rd                    : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);

            busy : OUT STD_LOGIC

        );
    END COMPONENT;

    COMPONENT eu_u_type IS
        PORT (
            rst, clk : IN STD_LOGIC;

            we                                       : IN STD_LOGIC;
            rs1_data, rs2_data, instruction, pc, imm : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

            writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

            next_pc : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

            rd              : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
            busy, update_rd : OUT STD_LOGIC

        );
    END COMPONENT;
    -- functions

    FUNCTION f_calculate_i_type_zero (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
    ) RETURN STD_LOGIC_VECTOR;

    FUNCTION f_decode_opcode (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
    ) RETURN opcode_t;

    FUNCTION f_decode_exec_unit (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
    ) RETURN opcode_group_t;

    FUNCTION f_uses_rs1 (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
    ) RETURN STD_LOGIC;

    FUNCTION f_uses_rs2 (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
    ) RETURN STD_LOGIC;

    FUNCTION f_updates_rd (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
    ) RETURN STD_LOGIC;

    FUNCTION f_updates_pc (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
    ) RETURN STD_LOGIC;

    FUNCTION f_decode_imm (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
    ) RETURN STD_LOGIC_VECTOR;

    FUNCTION DoShift (
        value            : STD_LOGIC_VECTOR(31 DOWNTO 0);
        shamt            : STD_LOGIC_VECTOR(4 DOWNTO 0);
        arithmetic_shift : BOOLEAN;
        shleft           : BOOLEAN
    ) RETURN STD_LOGIC_VECTOR;
END PACKAGE bebichiken;
PACKAGE BODY bebichiken IS

    FUNCTION f_calculate_i_type_zero (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN STD_LOGIC_VECTOR IS
        VARIABLE op                        : opcode_t;
        VARIABLE r_imm, r_rs1_data, result : STD_LOGIC_VECTOR(31 DOWNTO 0);
    BEGIN

        r_imm      := f_decode_imm(instruction);
        op         := f_decode_opcode(instruction);
        r_rs1_data := X"00000000";
        result     := X"00000000";

        IF op = OPCODE_I_TYPE_ADDI THEN
            result := r_rs1_data + r_imm;
        END IF;
        IF op = OPCODE_I_TYPE_SLLI THEN
            result := DoShift(r_rs1_data, r_imm(4 DOWNTO 0), false, true);
        END IF;
        IF op = OPCODE_I_TYPE_SLTI THEN
            IF signed(r_rs1_data) < signed(r_imm) THEN
                result := X"00000001";
            ELSE
                result := (OTHERS => '0');
            END IF;
        END IF;
        IF op = OPCODE_I_TYPE_SLTIU THEN
            IF unsigned(r_rs1_data) < unsigned(r_imm) THEN
                result := X"00000001";
            ELSE
                result := (OTHERS => '0');
            END IF;
        END IF;
        IF op = OPCODE_I_TYPE_XORI THEN
            result := r_rs1_data XOR r_imm;
        END IF;
        IF op = OPCODE_I_TYPE_SRLI THEN
            result := DoShift(r_rs1_data, r_imm(4 DOWNTO 0), false, false);
        END IF;
        IF op = OPCODE_I_TYPE_SRAI THEN
            result := DoShift(r_rs1_data, r_imm(4 DOWNTO 0), true, false);
        END IF;
        IF op = OPCODE_I_TYPE_ORI THEN
            result := r_rs1_data OR r_imm;
        END IF;
        IF op = OPCODE_I_TYPE_ANDI THEN
            result := r_rs1_data AND r_imm;
        END IF;

        RETURN result;

    END;

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
                    RETURN OPCODE_ALU_TYPE;
                END IF;

                IF funct7 = "0100000" THEN
                    RETURN OPCODE_ALU_TYPE;
                END IF;
            END IF;
            IF funct3 = "001" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;
            IF funct3 = "010" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;

            IF funct3 = "011" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;

            IF funct3 = "100" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;

            IF funct3 = "101" THEN
                IF funct7 = "0000000" THEN
                    RETURN OPCODE_ALU_TYPE;
                END IF;

                IF funct7 = "0100000" THEN
                    RETURN OPCODE_ALU_TYPE;
                END IF;
            END IF;

            IF funct3 = "110" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;

            IF funct3 = "111" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;

            RETURN OPCODE_INVALID;
        END IF;

        IF opcode = "0010011" THEN -- I_TYPE
            --        RETURN OPCODE_I_TYPE; -- Register/Immediate (ADDI, ...)
            IF funct3 = "000" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;

            IF funct3 = "001" THEN
                IF funct7 = "0000000" THEN
                    RETURN OPCODE_ALU_TYPE;
                END IF;
            END IF;

            IF funct3 = "010" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;
            IF funct3 = "011" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;

            IF funct3 = "100" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;

            IF funct3 = "101" THEN

                IF funct7 = "0000000" THEN
                    RETURN OPCODE_ALU_TYPE;
                END IF;

                IF funct7 = "0100000" THEN
                    RETURN OPCODE_ALU_TYPE;
                END IF;

            END IF;

            IF funct3 = "110" THEN
                RETURN OPCODE_ALU_TYPE;
            END IF;

            IF funct3 = "111" THEN
                RETURN OPCODE_ALU_TYPE;
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

        -- IF opcode = "0110111" THEN
        --     RETURN OPCODE_U_TYPE; -- LUI
        -- END IF;

        -- IF opcode = "0010111" THEN
        --     RETURN OPCODE_U_TYPE; -- AUIPC
        -- END IF;

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
        VARIABLE i_rd   : STD_LOGIC_VECTOR(4 DOWNTO 0);
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
        opcode    : IN opcode_t;
        registers : IN STD_LOGIC_VECTOR(1023 DOWNTO 0)
    )
        RETURN STD_LOGIC_VECTOR IS
        VARIABLE ires  : STD_LOGIC_VECTOR(1023 DOWNTO 0);
        VARIABLE found : STD_LOGIC;
    BEGIN
        ires  := registers;
        found := '0';
        FOR op IN opcode_t LOOP
            ires := ires(991 DOWNTO 0) & X"00000000";
            IF op = opcode THEN
                RETURN ires;
            END IF;
        END LOOP;

        RETURN ires;

    END;
    FUNCTION f_decode_imm (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN STD_LOGIC_VECTOR IS
        VARIABLE opcode : opcode_t;
        VARIABLE imm    : STD_LOGIC_VECTOR(31 DOWNTO 0);
    BEGIN

        opcode := f_decode_opcode(instruction);

        IF (opcode = OPCODE_I_TYPE_ADDI) OR (opcode = OPCODE_I_TYPE_SLLI)
            OR (opcode = OPCODE_I_TYPE_SLTI) OR (opcode = OPCODE_I_TYPE_SLTIU) OR (opcode = OPCODE_I_TYPE_XORI)
            OR (opcode = OPCODE_I_TYPE_SRLI) OR (opcode = OPCODE_I_TYPE_SRAI) OR (opcode = OPCODE_I_TYPE_ORI)
            OR (opcode = OPCODE_I_TYPE_ANDI) OR (opcode = OPCODE_I_TYPE_LOAD) THEN
            imm(31 DOWNTO 11) := (OTHERS => instruction(31));
            imm(10 DOWNTO 5)  := instruction(30 DOWNTO 25);
            imm(4 DOWNTO 1)   := instruction(24 DOWNTO 21);
            imm(0)            := instruction(20);
        END IF;
        IF opcode = OPCODE_S_TYPE THEN
            imm(31 DOWNTO 11) := (OTHERS => instruction(31));
            imm(10 DOWNTO 5)  := instruction(30 DOWNTO 25);
            imm(4 DOWNTO 1)   := instruction(11 DOWNTO 8);
            imm(0)            := instruction(7);
        END IF;

        IF (opcode = OPCODE_B_TYPE_BEQ) OR (opcode = OPCODE_B_TYPE_BNE) OR (opcode = OPCODE_B_TYPE_BLT) OR (opcode = OPCODE_B_TYPE_BGE) OR (opcode = OPCODE_B_TYPE_BLTU) OR (opcode = OPCODE_B_TYPE_BGEU) THEN
            imm(31 DOWNTO 12) := (OTHERS => instruction(31));
            imm(11)           := instruction(7);
            imm(10 DOWNTO 5)  := instruction(30 DOWNTO 25);
            imm(4 DOWNTO 1)   := instruction(11 DOWNTO 8);
            imm(0)            := '0';
        END IF;

        IF (opcode = OPCODE_U_TYPE_LUI) OR (opcode = OPCODE_U_TYPE_AUIPC) THEN
            imm(31)           := instruction(31);
            imm(30 DOWNTO 20) := instruction(30 DOWNTO 20);
            imm(19 DOWNTO 12) := instruction(19 DOWNTO 12);
            imm(11 DOWNTO 0)  := (OTHERS => '0');
        END IF;

        IF opcode = OPCODE_J_TYPE_JAL THEN
            imm(31 DOWNTO 20) := (OTHERS => instruction(31));
            imm(19 DOWNTO 12) := instruction(19 DOWNTO 12);
            imm(11)           := instruction(20);
            imm(10 DOWNTO 5)  := instruction(30 DOWNTO 25);
            imm(4 DOWNTO 1)   := instruction(24 DOWNTO 21);
            imm(0)            := '0';
        END IF;

        IF opcode = OPCODE_J_TYPE_JALR THEN
            imm              := (OTHERS => '0');
            imm(11 DOWNTO 0) := instruction(31 DOWNTO 20);
        END IF;
        RETURN imm;

    END;

    FUNCTION DoShift (
        value            : STD_LOGIC_VECTOR(31 DOWNTO 0);
        shamt            : STD_LOGIC_VECTOR(4 DOWNTO 0);
        arithmetic_shift : BOOLEAN;
        shleft           : BOOLEAN
    ) RETURN STD_LOGIC_VECTOR IS
        VARIABLE ires, temp : STD_LOGIC_VECTOR(31 DOWNTO 0);
        VARIABLE appendbit  : STD_LOGIC;
    BEGIN
        IF arithmetic_shift = true THEN
            appendbit := value(31);
        ELSE
            appendbit := '0';
        END IF;

        IF shamt = "11111" THEN
            ires := (OTHERS => appendbit);
            RETURN ires;
        ELSIF shamt = "00000" THEN
            RETURN value;
        END IF;
        -- IF shleft = true THEN
        --     ires := (OTHERS => '0');
        --     ires(31 DOWNTO shamt) := value(31 - shamt DOWNTO 0);
        -- ELSE
        --     ires := (OTHERS => appendbit);
        --     ires(31 - shamt DOWNTO 0) := value(31 DOWNTO shamt);
        -- END IF;

        --return ires;
        IF shleft = true THEN
            ires := value;

            IF (shamt AND "10000") /= "00000" THEN
                ires := ires(15 DOWNTO 0) & X"0000";
            END IF;

            IF (shamt AND "01000") /= "00000" THEN
                ires := ires(23 DOWNTO 0) & X"00";
            END IF;

            IF (shamt AND "00100") /= "00000" THEN
                ires := ires(27 DOWNTO 0) & X"0";
            END IF;

            IF (shamt AND "00010") /= "00000" THEN
                ires := ires(29 DOWNTO 0) & "00";
            END IF;

            IF (shamt AND "00001") /= "00000" THEN
                ires := ires(30 DOWNTO 0) & '0';
            END IF;

        ELSE
            ires := value;

            temp := (OTHERS => appendbit);

            IF (shamt AND "10000") /= "00000" THEN
                ires := temp(15 DOWNTO 0) & ires(31 DOWNTO 16);
            END IF;

            IF (shamt AND "01000") /= "00000" THEN
                ires := temp(7 DOWNTO 0) & ires(31 DOWNTO 8);
            END IF;

            IF (shamt AND "00100") /= "00000" THEN
                ires := temp(3 DOWNTO 0) & ires(31 DOWNTO 4);
            END IF;

            IF (shamt AND "00010") /= "00000" THEN
                ires := temp(1 DOWNTO 0) & ires(31 DOWNTO 2);
            END IF;

            IF (shamt AND "00001") /= "00000" THEN
                ires := temp(0) & ires(31 DOWNTO 1);
            END IF;
        END IF;

        -- IF shleft = true THEN
        --     ires := (OTHERS => '0');
        --     ires(31 DOWNTO shamt) := value(31 - shamt DOWNTO 0);
        -- ELSE
        --     ires := (OTHERS => appendbit);
        --     ires(31 - shamt DOWNTO 0) := value(31 DOWNTO shamt);
        -- END IF;
        RETURN ires;
    END FUNCTION;

END bebichiken;