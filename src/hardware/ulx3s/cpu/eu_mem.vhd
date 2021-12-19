LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY eu_mem IS
    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_we     : OUT STD_LOGIC;
        writeback_result : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        mem_we, mem_re : out std_logic;
        mem_wack, mem_rdy : in std_logic;
        mem_wdata, mem_addr : out std_logic_vector(31 downto 0);
        mem_rdata : in std_logic_vector(31 downto 0);
        mem_width : out std_logic_vector(1 downto 0);

        busy : out std_logic 

    );
END eu_mem;

ARCHITECTURE behavioural OF eu_mem IS
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
    SIGNAL r_rs1_data, r_rs2_data, r_instruction : STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal r_we : std_logic;

    type state_t is (S_IDLE, S_BUSY);
    signal state, n_state : state_t;

    signal op : opcode_t;

BEGIN


    PROCESS (rst, clk)
    BEGIN
        if rst = '1' then
            r_rs1_data <= (others => '0');
            r_rs2_data <= (others => '0');
            r_instruction <= (others => '0');
            state <= S_IDLE;
        elsIF rising_edge(clk) THEN

            state <= n_state;

            --r_we         <= we;
            --writeback_we <= r_we;

            IF we = '1' THEN
                r_rs1_data    <= rs1_data;
                r_rs2_data    <= rs2_data;
                r_instruction <= instruction;
            END IF;
        END IF;
    END PROCESS;

    writeback_result <= mem_rdata;
    mem_addr <= rs1_data + f_decode_imm(r_instruction);
    mem_wdata <= rs2_data;
    mem_width <= r_instruction(13 downto 12);
    -- not formally correct, still need to account for funct3(2), i.e.: r_instruction(14), and sign extension
    -- preferrably not even feeding execution unit if instruction is invalid

    op <= f_decode_opcode(r_instruction);

    process(state, we, state, r_instruction, mem_rdy, mem_wack)
    begin
        n_state <= state;
        busy <= '1';
        mem_re <= '0';
        mem_we <= '0';
        writeback_we <= '0';
        case state is
            when S_IDLE =>
                busy <= '0';
                if we = '1' then
                    n_state <= S_BUSY;
                end if;
            when S_BUSY =>
                if op = OPCODE_I_TYPE_LOAD then
                    mem_re <= '1';
                    if mem_rdy = '1' then
                        writeback_we <= '1';
                        n_state <= S_IDLE;
                    end if;

                elsif op = OPCODE_S_TYPE then
                    mem_we <= '1';
                    if mem_wack = '1' then
                        n_state <= S_IDLE;
                    end if;
                else
                    n_state <= S_IDLE;
                end if;
        end case;
    end process;

                    

END behavioural;