LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY execunit IS
    GENERIC (operation : opcode_t := OPCODE_I_TYPE_ADDI);

    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_we     : OUT STD_LOGIC;
        writeback_result : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        update_pc : OUT STD_LOGIC;

        uses_rs1, uses_rs2, updates_rd, updates_pc, busy : out std_logic 

    );
END execunit;

ARCHITECTURE behavioural OF execunit IS
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
    SIGNAL r_rs1_data, r_rs2_data, r_instruction, r_pc : STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal r_we : std_logic;

BEGIN


    PROCESS (clk)
    BEGIN
        IF rising_edge(clk) THEN
            r_we         <= we;
            writeback_we <= r_we;

            IF we = '1' THEN
                r_rs1_data    <= rs1_data;
                r_rs2_data    <= rs2_data;
                r_instruction <= instruction;
                r_pc          <= pc;
            END IF;
        END IF;
    END PROCESS;

    busy <= r_we;

    PROCESS (r_rs1_data, r_rs2_data, r_pc)
    BEGIN
        update_pc        <= '0';
        writeback_result <= (OTHERS => '0');
        next_pc <= r_pc + X"00000004";

        updates_pc <= '0';
        updates_rd <= '0';
        uses_rs1 <= '1';
        uses_rs2 <= '0';

        CASE operation IS
            WHEN OPCODE_R_TYPE_ADD =>
                uses_rs2 <= '1';
                writeback_result <= r_rs1_data + r_rs2_data;
            WHEN OPCODE_R_TYPE_SUB =>
                uses_rs2 <= '1';
                writeback_result <= r_rs1_data - r_rs2_data;
            WHEN OPCODE_R_TYPE_SLL =>
                uses_rs2 <= '1';
                writeback_result <= DoShift(r_rs1_data, r_rs2_data(4 DOWNTO 0), false, true);
            WHEN OPCODE_R_TYPE_SLT =>
                uses_rs2 <= '1';
                IF signed(r_rs1_data) < signed(r_rs2_data) THEN
                    writeback_result <= X"00000001";
                ELSE
                    writeback_result <= (OTHERS => '0');
                END IF;
            WHEN OPCODE_R_TYPE_SLTU =>
                uses_rs2 <= '1';
                IF unsigned(r_rs1_data) < unsigned(r_rs2_data) THEN
                    writeback_result <= X"00000001";
                ELSE
                    writeback_result <= (OTHERS => '0');
                END IF;
            WHEN OPCODE_R_TYPE_XOR =>
                uses_rs2 <= '1';
                writeback_result <= r_rs1_data XOR r_rs2_data;

            WHEN OPCODE_R_TYPE_SRL =>
                uses_rs2 <= '1';
                writeback_result <= DoShift(r_rs1_data, r_rs2_data(4 DOWNTO 0), false, false);

            WHEN OPCODE_R_TYPE_SRA =>
            uses_rs2 <= '1';
                writeback_result <= DoShift(r_rs1_data, r_rs2_data(4 DOWNTO 0), true, false);

            WHEN OPCODE_R_TYPE_OR =>
            uses_rs2 <= '1';
                writeback_result <= r_rs1_data OR r_rs2_data;

            WHEN OPCODE_R_TYPE_AND =>
            uses_rs2 <= '1';    
            writeback_result <= r_rs1_data AND r_rs2_data;

            WHEN OPCODE_I_TYPE_ADDI =>
                writeback_result <= r_rs1_data + f_decode_imm(r_instruction);
            WHEN OPCODE_I_TYPE_SLLI =>
                writeback_result <= DoShift(r_rs1_data, f_decode_imm(r_instruction)(4 DOWNTO 0), false, true);
            WHEN OPCODE_I_TYPE_SLTI =>
                IF signed(r_rs1_data) < signed(f_decode_imm(r_instruction)) THEN
                    writeback_result <= X"00000001";
                ELSE
                    writeback_result <= (OTHERS => '0');
                END IF;
            WHEN OPCODE_I_TYPE_SLTIU =>
                IF unsigned(r_rs1_data) < unsigned(f_decode_imm(r_instruction)) THEN
                    writeback_result <= X"00000001";
                ELSE
                    writeback_result <= (OTHERS => '0');
                END IF;
            WHEN OPCODE_I_TYPE_XORI =>
                writeback_result <= r_rs1_data XOR f_decode_imm(r_instruction);

            WHEN OPCODE_I_TYPE_SRLI =>
                writeback_result <= DoShift(r_rs1_data, f_decode_imm(r_instruction)(4 DOWNTO 0), false, false);

            WHEN OPCODE_I_TYPE_SRAI =>
                writeback_result <= DoShift(r_rs1_data, f_decode_imm(r_instruction)(4 DOWNTO 0), true, false);

            WHEN OPCODE_I_TYPE_ORI =>
                writeback_result <= r_rs1_data OR f_decode_imm(r_instruction);

            WHEN OPCODE_I_TYPE_ANDI =>
                writeback_result <= r_rs1_data AND f_decode_imm(r_instruction);

                --WHEN OPCODE_I_TYPE_LOAD =>

                --WHEN OPCODE_S_TYPE     =>
            WHEN OPCODE_B_TYPE_BEQ =>
                update_pc <= '1';
                uses_rs2 <= '1';
                updates_pc <= '1';
                IF signed(r_rs1_data) = signed(r_rs2_data) THEN
                    next_pc <= r_pc + f_decode_imm(r_instruction); --imm_b;
                END IF;
            WHEN OPCODE_B_TYPE_BNE =>
                update_pc <= '1';
                uses_rs2 <= '1';
                updates_pc <= '1';
                IF signed(r_rs1_data) /= signed(r_rs2_data) THEN
                    next_pc <= r_pc + f_decode_imm(r_instruction); --imm_b;
                END IF;
            WHEN OPCODE_B_TYPE_BLT =>
                update_pc <= '1';
                uses_rs2 <= '1';
                updates_pc <= '1';
                IF signed(r_rs1_data) < signed(r_rs2_data) THEN
                    next_pc <= r_pc + f_decode_imm(r_instruction); --imm_b;
                END IF;
            WHEN OPCODE_B_TYPE_BGE =>
                update_pc <= '1';
                uses_rs2 <= '1';
                updates_pc <= '1';
                IF signed(r_rs1_data) >= signed(r_rs2_data) THEN
                    next_pc <= r_pc + f_decode_imm(r_instruction); --imm_b;
                END IF;
            WHEN OPCODE_B_TYPE_BLTU =>
                update_pc <= '1';
                uses_rs2 <= '1';
                updates_pc <= '1';
                IF unsigned(r_rs1_data) < unsigned(r_rs2_data) THEN
                    next_pc <= r_pc + f_decode_imm(r_instruction); --imm_b;
                END IF;
            WHEN OPCODE_B_TYPE_BGEU =>
                update_pc <= '1';
                uses_rs2 <= '1';
                updates_pc <= '1';
                IF unsigned(r_rs1_data) >= unsigned(r_rs2_data) THEN
                    next_pc <= r_pc + f_decode_imm(r_instruction); --imm_b;
                END IF;
            WHEN OPCODE_U_TYPE_LUI =>
                uses_rs1 <= '0';
                writeback_result <= f_decode_imm(r_instruction);
            WHEN OPCODE_U_TYPE_AUIPC =>
                uses_rs1 <= '0';
                writeback_result <= r_pc + f_decode_imm(r_instruction);

            WHEN OPCODE_J_TYPE_JAL =>
                uses_rs1 <= '0';
                writeback_result <= r_pc + X"00000004";
                next_pc <= r_pc + f_decode_imm(r_instruction);
                update_pc <= '1';
                updates_pc <= '1';

            WHEN OPCODE_J_TYPE_JALR =>
                next_pc <= (f_decode_imm(r_instruction) + r_rs1_data) AND X"FFFFFFFE";
                writeback_result <= r_pc + X"00000004";
                update_pc <= '1';
                updates_pc <= '1';

            WHEN OTHERS =>
        END CASE;
    END PROCESS;

END behavioural;