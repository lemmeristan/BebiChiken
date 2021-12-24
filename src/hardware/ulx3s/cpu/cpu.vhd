LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY cpu IS
    GENERIC (entry_point : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00200000");

    PORT (
        rst, clk : IN STD_LOGIC;

        -- Instruction memory bus
        inst_width : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
        inst_addr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        inst_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        inst_re : OUT STD_LOGIC;
        inst_rdy : IN STD_LOGIC;

        -- Data memory bus
        data_width : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
        data_addr, data_wdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_re, data_we : OUT STD_LOGIC;
        data_rdy, data_wack : IN STD_LOGIC;

        -- Register file
        --registerfile_rs1, registerfile_rs2, registerfile_rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
        --registerfile_wdata_rd : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        --registerfile_rdata_rs1, registerfile_rdata_rs2 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        --registerfile_we : OUT STD_LOGIC;

        err : OUT STD_LOGIC
    );
END cpu;

ARCHITECTURE behavioural OF cpu IS

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
                RETURN OPCODE_B_TYPE;
            END IF;
            IF funct3 = "001" THEN
                RETURN OPCODE_B_TYPE;
            END IF;
            IF funct3 = "100" THEN
                RETURN OPCODE_B_TYPE;
            END IF;
            IF funct3 = "101" THEN
                RETURN OPCODE_B_TYPE;
            END IF;
            IF funct3 = "110" THEN
                RETURN OPCODE_B_TYPE;
            END IF;
            IF funct3 = "111" THEN
                RETURN OPCODE_B_TYPE;
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
            RETURN OPCODE_J_TYPE; -- JAL
        END IF;

        IF opcode = "1100111" THEN
            RETURN OPCODE_J_TYPE; -- JALR
        END IF;

        RETURN OPCODE_INVALID;

    END;







    FUNCTION f_uses_rs1 (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN std_logic IS
        VARIABLE opcode : opcode_t;
    BEGIN
        opcode := f_decode_opcode(instruction);

        IF opcode = OPCODE_I_TYPE_LOAD OR opcode = OPCODE_S_TYPE OR opcode = OPCODE_J_TYPE_JALR
        or opcode = OPCODE_B_TYPE_BEQ or opcode = OPCODE_B_TYPE_BNE or opcode = OPCODE_B_TYPE_BLT
        or opcode = OPCODE_B_TYPE_BGE or opcode = OPCODE_B_TYPE_BLTU or opcode = OPCODE_B_TYPE_BGEU
        or opcode = OPCODE_R_TYPE_ADD OR opcode = OPCODE_R_TYPE_SUB OR opcode = OPCODE_R_TYPE_SLL
        OR opcode = OPCODE_R_TYPE_SLT or opcode = OPCODE_R_TYPE_SLTU or opcode = OPCODE_R_TYPE_XOR
        or opcode = OPCODE_R_TYPE_SRL or opcode = OPCODE_R_TYPE_SRA or opcode = OPCODE_R_TYPE_OR
        or opcode = OPCODE_R_TYPE_AND or opcode = OPCODE_I_TYPE_ADDI or opcode = OPCODE_I_TYPE_SLLI
        or opcode = OPCODE_I_TYPE_SLTI or opcode = OPCODE_I_TYPE_SLTIU or opcode = OPCODE_I_TYPE_XORI
        or opcode = OPCODE_I_TYPE_SRLI or opcode = OPCODE_I_TYPE_SRAI or opcode = OPCODE_I_TYPE_ORI
        or opcode = OPCODE_I_TYPE_ANDI
         THEN
            RETURN '1';
        END IF;

        RETURN '0';
    END;

    FUNCTION f_uses_rs2 (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN std_logic IS
        VARIABLE opcode : opcode_t;
    BEGIN
        opcode := f_decode_opcode(instruction);

        IF opcode = OPCODE_S_TYPE
        OR opcode = OPCODE_R_TYPE_SLT or opcode = OPCODE_R_TYPE_SLTU or opcode = OPCODE_R_TYPE_XOR
        or opcode = OPCODE_R_TYPE_SRL or opcode = OPCODE_R_TYPE_SRA or opcode = OPCODE_R_TYPE_OR
        or opcode = OPCODE_R_TYPE_AND or opcode = OPCODE_B_TYPE_BEQ or opcode = OPCODE_B_TYPE_BNE
        or opcode = OPCODE_B_TYPE_BLT or opcode = OPCODE_B_TYPE_BGE or opcode = OPCODE_B_TYPE_BLTU
        or opcode = OPCODE_B_TYPE_BGEU
        or opcode = OPCODE_R_TYPE_ADD OR opcode = OPCODE_R_TYPE_SUB OR opcode = OPCODE_R_TYPE_SLL
        OR opcode = OPCODE_R_TYPE_SLT or opcode = OPCODE_R_TYPE_SLTU or opcode = OPCODE_R_TYPE_XOR
        or opcode = OPCODE_R_TYPE_SRL or opcode = OPCODE_R_TYPE_SRA or opcode = OPCODE_R_TYPE_OR
        or opcode = OPCODE_R_TYPE_AND

         then
            RETURN '1';
        END IF;

        RETURN '0';
    END;

    FUNCTION f_updates_rd (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN std_logic IS
        VARIABLE opcode : opcode_t;
    BEGIN
        opcode := f_decode_opcode(instruction);

        IF opcode = OPCODE_R_TYPE_ADD OR opcode = OPCODE_R_TYPE_SUB OR opcode = OPCODE_R_TYPE_SLL
        OR opcode = OPCODE_R_TYPE_SLT or opcode = OPCODE_R_TYPE_SLTU or opcode = OPCODE_R_TYPE_XOR
        or opcode = OPCODE_R_TYPE_SRL or opcode = OPCODE_R_TYPE_SRA or opcode = OPCODE_R_TYPE_OR
        or opcode = OPCODE_R_TYPE_AND or opcode = OPCODE_I_TYPE_ADDI or opcode = OPCODE_I_TYPE_SLLI
        or opcode = OPCODE_I_TYPE_SLTI or opcode = OPCODE_I_TYPE_SLTIU or opcode = OPCODE_I_TYPE_XORI
        or opcode = OPCODE_I_TYPE_SRLI or opcode = OPCODE_I_TYPE_SRAI or opcode = OPCODE_I_TYPE_ORI
        or opcode = OPCODE_I_TYPE_ANDI or opcode = OPCODE_U_TYPE_LUI or opcode = OPCODE_U_TYPE_AUIPC
        or opcode = OPCODE_J_TYPE_JAL or opcode = OPCODE_J_TYPE_JALR then
            RETURN '1';
        END IF;

        RETURN '0';
    END;

    FUNCTION f_updates_pc (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
        RETURN std_logic IS
        VARIABLE opcode : opcode_t;
    BEGIN
        opcode := f_decode_opcode(instruction);

        IF opcode = OPCODE_B_TYPE_BEQ OR opcode = OPCODE_B_TYPE_BNE OR opcode = OPCODE_B_TYPE_BLT
        OR opcode = OPCODE_B_TYPE_BGE or opcode = OPCODE_B_TYPE_BLTU or opcode = OPCODE_B_TYPE_BGEU
        or opcode = OPCODE_J_TYPE_JAL or opcode = OPCODE_J_TYPE_JALR then 
            RETURN '1';
        END IF;

        RETURN '0';
    END;


    FUNCTION f_shift_up (
        opcode : IN opcode_t;
        registers : in std_logic_vector(1023 downto 0)
        )
        RETURN std_logic_vector IS
        VARIABLE ires : std_logic_vector(1023 downto 0);
        VARIABLE found : std_logic;
    BEGIN
        ires := registers;
        found := '0';


        for op in opcode_t loop
            ires := ires(991 downto 0) & X"00000000";
            if op = opcode then
                return ires;
            end if;
        end loop;

        return ires;

    END;



    COMPONENT regfile_single IS
        PORT (
            clk, rst : IN STD_LOGIC;
            rs1, rs2 : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            rs1_data_out, rs2_data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            update_rd : IN STD_LOGIC;
            rd_data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0)

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
            rd : out std_logic_vector(4 downto 0);
            --uses_rs1, uses_rs2, updates_rd, updates_pc, 
            busy, rdy : OUT STD_LOGIC
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

            rd : out std_logic_vector(4 downto 0);

            busy, rdy : OUT STD_LOGIC
        );
    END COMPONENT;

    COMPONENT eu_b_type IS
    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        update_pc : OUT STD_LOGIC;

        rd : out std_logic_vector(4 downto 0);
        busy, rdy : out std_logic

    );
    END COMPONENT;

    COMPONENT eu_i_type IS
    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        update_pc : OUT STD_LOGIC;

        rd : out std_logic_vector(4 downto 0);
        busy, rdy : out std_logic

    );
    END COMPONENT;

    COMPONENT eu_r_type IS
    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        update_pc : OUT STD_LOGIC;

        rd : out std_logic_vector(4 downto 0);
        busy, rdy : out std_logic

    );
    END COMPONENT;
    COMPONENT eu_j_type IS
    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        update_pc : OUT STD_LOGIC;

        rd : out std_logic_vector(4 downto 0);
        busy, rdy : out std_logic

    );
    END COMPONENT;
    COMPONENT eu_u_type IS
    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_rd, writeback_rs1, writeback_rs2 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        update_pc : OUT STD_LOGIC;

        rd : out std_logic_vector(4 downto 0);
        busy, rdy : out std_logic

    );
    END COMPONENT;



    -- funct7 <= instruction(31 DOWNTO 25);
    -- rs2 <= instruction(24 DOWNTO 20);
    -- rs1 <= instruction(19 DOWNTO 15);
    -- funct3 <= instruction(14 DOWNTO 12);
    -- rd <= instruction(11 DOWNTO 7);
    -- opcode <= instruction(6 DOWNTO 0);

    --SIGNAL instruction : STD_LOGIC_VECTOR(31 DOWNTO 0);



    SIGNAL pc: STD_LOGIC_VECTOR(31 DOWNTO 0);
    --SIGNAL imm_i, imm_s, imm_b, imm_u, imm_j, imm_jalr : STD_LOGIC_VECTOR(31 DOWNTO 0);

    --ATTRIBUTE syn_encoding : STRING;
    --ATTRIBUTE syn_encoding OF state_t : TYPE IS "one-hot";

    ATTRIBUTE syn_keep : BOOLEAN;
    SIGNAL decode_error : opcode_bit_t := (OTHERS => '1');
    SIGNAL writeback : opcode_bit_t := (OTHERS => '0');
    SIGNAL update_pc : opcode_bit_t := (OTHERS => '1');
    SIGNAL dwe, dre : opcode_bit_t := (OTHERS => '0');
    SIGNAL selected : opcode_bit_t := (OTHERS => '0');
    SIGNAL next_pc, r_next_pc : opcode_word_t := (OTHERS => (OTHERS => '0'));
    SIGNAL result : opcode_word_t := (OTHERS => (OTHERS => '0'));
    SIGNAL wdata : opcode_word_t := (OTHERS => (OTHERS => '0'));
    SIGNAL daddr : opcode_word_t := (OTHERS => (OTHERS => '0'));
    SIGNAL registerfile_rdata_rs1, registerfile_rdata_rs2, r_instruction : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL writeback_we : opcode_group_bit_t := (others => '0');
    SIGNAL writeback_rd, writeback_rs1, writeback_rs2 : opcode_group_word_t := (others => (others => '0'));
    SIGNAL writeback_pc : opcode_group_word_t := (others => (others => '0'));
    SIGNAL eu_we, eu_busy, execunit_busy : opcode_group_bit_t;

    SIGNAL uses_rs1, uses_rs2, updates_pc, updates_rd : opcode_bit_t := (OTHERS => '0');

    SIGNAL allready : STD_LOGIC;


    SIGNAL eu_mem_busy, eu_mem_we : STD_LOGIC;

    ALIAS funct7 : STD_LOGIC_VECTOR(6 DOWNTO 0) IS r_instruction(31 DOWNTO 25);
    ALIAS rs2 : STD_LOGIC_VECTOR(4 DOWNTO 0) IS r_instruction(24 DOWNTO 20);
    ALIAS rs1 : STD_LOGIC_VECTOR(4 DOWNTO 0) IS r_instruction(19 DOWNTO 15);
    ALIAS funct3 : STD_LOGIC_VECTOR(2 DOWNTO 0) IS r_instruction(14 DOWNTO 12);
    ALIAS rd : STD_LOGIC_VECTOR(4 DOWNTO 0) IS r_instruction(11 DOWNTO 7);
    ALIAS opcode : STD_LOGIC_VECTOR(6 DOWNTO 0) IS r_instruction(6 DOWNTO 0);


    signal rs1_data_out , rs2_data_out ,    rs1_data_in , rs2_data_in  : std_logic_vector(31 downto 0);
    signal update_rs1 , update_rs2  : std_logic;

    type owner_t is array(NATURAL RANGE <>) of opcode_group_t;
    signal owner : owner_t(32 downto 0) := (others => OPCODE_INVALID);

    signal rd_out : opcode_group_regidx_t;

    signal update_rd : std_logic;

    signal regfile_rd : std_logic_vector(4 downto 0);
    signal rd_data_in : std_logic_vector(31 downto 0);
    signal eu_rdy : opcode_group_bit_t;

    signal decoded : opcode_group_t;

    signal rd_registers, rd_registers_shifted : std_logic_vector(1023 downto 0);


BEGIN


    process(inst_rdata, allready)
    begin
        eu_we <= (OTHERS => '0');
        eu_we(f_decode_exec_unit(inst_rdata)) <= allready;
    end process;
    rd_out(OPCODE_INVALID) <= (others => '0');
    regfile_rd <= rd_out(decoded);
    rd_data_in <=  writeback_rd(decoded);


    update_rd <= f_updates_rd(inst_rdata); -- when owner(to_integer(unsigned(rd_out(decoded)))) /= OPCODE_INVALID else '0';


    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            r_instruction <= (OTHERS => '0');
            owner <= (others => OPCODE_INVALID);
            pc <= entry_point;
            decoded <= OPCODE_INVALID;

            --        decoded <= OPCODE_INVALID;
        ELSIF rising_edge(clk) THEN
            decoded <= f_decode_exec_unit(inst_rdata);

            r_instruction <= inst_rdata;

            if (allready = '1') and (f_updates_rd(inst_rdata) = '1') then
                owner(to_integer(unsigned(rd))) <= decoded;
            end if;

            if (allready = '1') then
                if (f_updates_pc(inst_rdata) = '1') then
                    owner(32) <= decoded;
                else
                
                    owner(32) <= OPCODE_INVALID;
                    end if;
            end if;

            if (allready = '1') and (eu_rdy(owner(32)) = '1') then
                pc <= writeback_pc(owner(32));
            end if;





        END IF;
    END PROCESS;


    update_rs1 <= allready and f_uses_rs1(inst_rdata);
    update_rs2 <= allready and f_uses_rs2(inst_rdata);



    inst_addr <= pc;
    inst_re <= '1';
    inst_width <= "10"; -- unused

    writeback_pc(OPCODE_INVALID) <= pc + X"00000004";

    execunit_busy(OPCODE_INVALID) <= '0';

    allready <= '1' WHEN (inst_rdy = '1') --AND (eu_rdy(owner(32)) = '1')
        AND ((f_uses_rs1(inst_rdata) = '0') OR ((f_uses_rs1(inst_rdata) = '1') AND (eu_rdy(owner(to_integer(unsigned(rs1)))) = '1')))
        AND ((f_uses_rs2(inst_rdata) = '0') OR ((f_uses_rs2(inst_rdata) = '1') AND (eu_rdy(owner(to_integer(unsigned(rs2)))) = '1')))
        AND (execunit_busy(f_decode_exec_unit(inst_rdata)) = '0')
        ELSE
        '0';
    update_pc(OPCODE_INVALID) <= allready;


    
    --rd_registers_shifted <= f_shift_up(f_decode_opcode(inst_rdata), rd_registers);



    --rs1_data_in <= rs1_data_out when owner(to_integer(unsigned(rs1))) = OPCODE_INVALID else writeback_rs1(owner(to_integer(unsigned(rs1))));
    --rs2_data_in <= rs2_data_out when owner(to_integer(unsigned(rs2))) = OPCODE_INVALID else writeback_rs2(owner(to_integer(unsigned(rs2))));

    --writeback_we(OPCODE_INVALID) <= '0';


    registerfile_rdata_rs1 <= writeback_rs1(owner(to_integer(unsigned(rs1)))) when owner(to_integer(unsigned(rs1))) /= OPCODE_INVALID else rs1_data_out;
    registerfile_rdata_rs2 <= writeback_rs2(owner(to_integer(unsigned(rs2)))) when owner(to_integer(unsigned(rs2))) /= OPCODE_INVALID else rs2_data_out;


    i_regfile_single : regfile_single
    PORT MAP(

        clk => clk, rst => rst,
        rs1 => rs1, rs2 => rs2,
        rs1_data_out => rs1_data_out, rs2_data_out => rs2_data_out,
        rd => regfile_rd,
        update_rd => update_rd,
        rd_data_in => rd_data_in

    );

    



    process(rst,clk)
    variable top : integer;
    begin
        if rst = '1' then
            rd_registers <= (others => '0');
        elsif rising_edge(clk ) then
            top := 1023;
            for op in opcode_group_t loop
                rd_registers(top downto top-31) <= writeback_rd(op);
                top := top - 32;
            end loop;
        end if;
    end process;


        process(execunit_busy, eu_mem_busy)
        begin
            eu_busy <= execunit_busy;
            eu_busy(OPCODE_MEM_TYPE) <= eu_mem_busy;
            --eu_busy(OPCODE_S_TYPE) <= eu_mem_busy;
            --eu_busy(OPCODE_I_TYPE_LOAD) <= eu_mem_busy;
        end process;

       eu_mem_we <= eu_we(OPCODE_MEM_TYPE); --eu_we(OPCODE_S_TYPE) OR eu_we(OPCODE_I_TYPE_LOAD);

        i_eu_mem : eu_mem
        PORT MAP(
            rst => rst, clk => clk,

            we => eu_mem_we,
            rs1_data => registerfile_rdata_rs1, rs2_data => registerfile_rdata_rs2, instruction => inst_rdata, 

            writeback_rd => writeback_rd(OPCODE_MEM_TYPE),
            writeback_rs1 => writeback_rs1(OPCODE_MEM_TYPE),
            writeback_rs2 => writeback_rs2(OPCODE_MEM_TYPE),

            mem_wack => data_wack, mem_rdy => data_rdy, mem_rdata => data_rdata, mem_wdata => data_wdata, mem_addr => data_addr, mem_width => data_width,
            mem_re => data_re, mem_we => data_we,
            rd => rd_out(OPCODE_MEM_TYPE),
            busy => eu_mem_busy,
            rdy => eu_rdy(OPCODE_MEM_TYPE)
        );


        i_eu_b : eu_b_type
        PORT MAP(
            rst => rst, clk => clk,

            we => eu_we(OPCODE_B_TYPE),
            rs1_data => registerfile_rdata_rs1, rs2_data => registerfile_rdata_rs2, instruction => inst_rdata, pc => pc,

            writeback_rd => writeback_rd(OPCODE_B_TYPE),
            writeback_rs1 => writeback_rs1(OPCODE_B_TYPE),
            writeback_rs2 => writeback_rs2(OPCODE_B_TYPE),

            rd => rd_out(OPCODE_B_TYPE),
            busy => execunit_busy(OPCODE_B_TYPE),
            rdy => eu_rdy(OPCODE_B_TYPE)
        );

        i_eu_i : eu_i_type
        PORT MAP(
            rst => rst, clk => clk,

            we => eu_we(OPCODE_I_TYPE),
            rs1_data => registerfile_rdata_rs1, rs2_data => registerfile_rdata_rs2, instruction => inst_rdata, pc => pc, 

            writeback_rd => writeback_rd(OPCODE_I_TYPE),
            writeback_rs1 => writeback_rs1(OPCODE_I_TYPE),
            writeback_rs2 => writeback_rs2(OPCODE_I_TYPE),

            rd => rd_out(OPCODE_I_TYPE),
            busy => execunit_busy(OPCODE_I_TYPE),
            rdy => eu_rdy(OPCODE_I_TYPE)
        );

        i_eu_j : eu_j_type
        PORT MAP(
            rst => rst, clk => clk,

            we => eu_we(OPCODE_J_TYPE),
            rs1_data => registerfile_rdata_rs1, rs2_data => registerfile_rdata_rs2, instruction => inst_rdata, pc => pc, 

            writeback_rd => writeback_rd(OPCODE_J_TYPE),
            writeback_rs1 => writeback_rs1(OPCODE_J_TYPE),
            writeback_rs2 => writeback_rs2(OPCODE_J_TYPE),

            rd => rd_out(OPCODE_J_TYPE),
            busy => execunit_busy(OPCODE_J_TYPE),
            rdy => eu_rdy(OPCODE_J_TYPE)
        );

        i_eu_r : eu_r_type
        PORT MAP(
            rst => rst, clk => clk,

            we => eu_we(OPCODE_R_TYPE),
            rs1_data => registerfile_rdata_rs1, rs2_data => registerfile_rdata_rs2, instruction => inst_rdata, pc => pc, 

            writeback_rd => writeback_rd(OPCODE_R_TYPE),
            writeback_rs1 => writeback_rs1(OPCODE_R_TYPE),
            writeback_rs2 => writeback_rs2(OPCODE_R_TYPE),

            rd => rd_out(OPCODE_R_TYPE),
            busy => execunit_busy(OPCODE_R_TYPE),
            rdy => eu_rdy(OPCODE_R_TYPE)
        );

        i_eu_u : eu_r_type
        PORT MAP(
            rst => rst, clk => clk,

            we => eu_we(OPCODE_U_TYPE),
            rs1_data => registerfile_rdata_rs1, rs2_data => registerfile_rdata_rs2, instruction => inst_rdata, pc => pc, 

            writeback_rd => writeback_rd(OPCODE_U_TYPE),
            writeback_rs1 => writeback_rs1(OPCODE_U_TYPE),
            writeback_rs2 => writeback_rs2(OPCODE_U_TYPE),

            rd => rd_out(OPCODE_U_TYPE),
            busy => execunit_busy(OPCODE_U_TYPE),
            rdy => eu_rdy(OPCODE_U_TYPE)
        );


END behavioural;