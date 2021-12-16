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



COMPONENT regfile_wide IS
    GENERIC ( entry_point : std_logic_vector(31 downto 0) );
    PORT (
        rst, clk : IN STD_LOGIC;
        rs1, rs2, rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        lock_rd, lock_pc : in std_logic;
        new_rd_lock_owner, new_pc_lock_owner : in opcode_t;
        update_pc : in opcode_bit_t;

        writeback_we : in opcode_bit_t;
        writeback_data : in opcode_word_t;
        writeback_pc : in opcode_word_t;

        rs1_data_out, rs2_data_out, pc : out std_logic_vector(31 downto 0);
        rs1_locked, rs2_locked, pc_locked : out std_logic

    );
END COMPONENT;


COMPONENT execunit IS
    GENERIC ( operation : opcode_t );

    PORT (
        rst, clk : IN STD_LOGIC;

        we                                  : IN STD_LOGIC;
        rs1_data, rs2_data, instruction, pc : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        rd                                  : IN STD_LOGIC_VECTOR(4 DOWNTO 0);

        writeback_we     : OUT STD_LOGIC;
        writeback_result : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);

        next_pc   : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        update_pc : OUT STD_LOGIC

    );
END COMPONENT;



-- funct7 <= instruction(31 DOWNTO 25);
-- rs2 <= instruction(24 DOWNTO 20);
-- rs1 <= instruction(19 DOWNTO 15);
-- funct3 <= instruction(14 DOWNTO 12);
-- rd <= instruction(11 DOWNTO 7);
-- opcode <= instruction(6 DOWNTO 0);

SIGNAL instruction : STD_LOGIC_VECTOR(31 DOWNTO 0);

alias funct7 : STD_LOGIC_VECTOR(6 DOWNTO 0) is instruction(31 downto 25);
alias rs2 : std_logic_vector(4 downto 0) is instruction(24 downto 20);
alias rs1 : std_logic_vector(4 downto 0) is instruction(19 downto 15);
alias funct3 : std_logic_vector(2 downto 0) is instruction(14 downto 12);
alias rd : std_logic_vector(4 downto 0) is instruction(11 downto 7);
alias opcode : std_logic_vector(6 downto 0) is instruction(6 downto 0);





SIGNAL pc, n_pc : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL imm_i, imm_s, imm_b, imm_u, imm_j, imm_jalr : STD_LOGIC_VECTOR(31 DOWNTO 0);

TYPE state_t IS (FETCH_INSTRUCTION, EXECUTE_1, EXECUTE_2,EXECUTE_3,EXECUTE_4,EXECUTE_5,EXECUTE_6,EXECUTE_7,EXECUTE_8, SEND_CHAR_0, SEND_CHAR_1, SEND_CHAR_2, SEND_CHAR_3,  PANIC);
--ATTRIBUTE syn_encoding : STRING;
--ATTRIBUTE syn_encoding OF state_t : TYPE IS "one-hot";
SIGNAL state, n_state : state_t;




SIGNAL set_instruction : STD_LOGIC;

ATTRIBUTE syn_keep : BOOLEAN;


signal set_instruction_of_opcode : opcode_bit_t;
signal instruction_of_opcode, registerfile_rdata_rs1_of_opcode, registerfile_rdata_rs2_of_opcode : opcode_word_t;
ATTRIBUTE syn_keep OF instruction_of_opcode, registerfile_rdata_rs1_of_opcode, registerfile_rdata_rs2_of_opcode : SIGNAL IS true;


SIGNAL decode_error : opcode_bit_t := (OTHERS => '1');
SIGNAL writeback : opcode_bit_t := (OTHERS => '0');
SIGNAL update_pc : opcode_bit_t := (OTHERS => '1');
SIGNAL dwe, dre : opcode_bit_t := (OTHERS => '0');
SIGNAL selected : opcode_bit_t := (OTHERS => '0');
SIGNAL next_pc, r_next_pc, r_instruction : opcode_word_t := (OTHERS => (OTHERS => '0'));
SIGNAL result : opcode_word_t := (OTHERS => (OTHERS => '0'));
SIGNAL wdata : opcode_word_t := (OTHERS => (OTHERS => '0'));
SIGNAL daddr : opcode_word_t := (OTHERS => (OTHERS => '0'));

SIGNAL i_data_wdata, i_data_addr : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL i_data_we, i_data_re : STD_LOGIC;

signal registerfile_rdata_rs1, registerfile_rdata_rs2 : std_logic_vector(31 downto 0);
signal registerfile_rs1, registerfile_rs2, registerfile_rd : std_logic_vector(4 downto 0);
signal lock_rd, lock_pc, rs1_locked, rs2_locked, pc_locked : std_logic;
signal new_rd_lock_owner, new_pc_lock_owner : opcode_t;
signal writeback_we : opcode_bit_t;
signal writeback_data : opcode_word_t;
signal writeback_pc : opcode_word_t;
signal eu_we : opcode_bit_t;


BEGIN

process(rst,clk)
begin
    if rst = '1' then
        eu_we <= (others => '0');
        update_pc(OPCODE_INVALID) <= '0';
        lock_rd <= '0';
        new_pc_lock_owner <= OPCODE_INVALID;
    elsif rising_edge(clk) then
        eu_we <= (others => '0');
        update_pc(OPCODE_INVALID) <= '0';
        if (inst_rdy = '1') and (pc_locked = '0') and (rs1_locked = '0') and (rs2_locked = '0') then
            eu_we(f_decode_opcode(inst_rdata)) <= '1';
            update_pc(OPCODE_INVALID) <= '1';
            lock_rd <= '1';
            new_pc_lock_owner <= f_decode_opcode(inst_rdata);
        end if;
    end if;
end process;

inst_addr <= pc;


i_regfile_wide : regfile_wide
GENERIC MAP(entry_point => entry_point)
PORT MAP(

        rst => rst,
        clk => clk,
        rs1 => registerfile_rs1, rs2 => registerfile_rs2, rd => registerfile_rd,
        lock_rd => lock_rd, lock_pc => lock_pc,
        new_rd_lock_owner => new_rd_lock_owner, new_pc_lock_owner => new_pc_lock_owner,
        update_pc => update_pc,

        writeback_we => writeback_we,
        writeback_data => writeback_data,
        writeback_pc => writeback_pc,

        rs1_data_out => registerfile_rdata_rs1, rs2_data_out => registerfile_rdata_rs2, pc => pc,
        rs1_locked => rs1_locked, rs2_locked => rs2_locked, pc_locked => pc_locked

);

execunit_gen : FOR op IN opcode_t GENERATE

    exclude_invalid : IF op /= OPCODE_INVALID GENERATE
        i_eu : execunit
        PORT MAP(
            rst => rst, clk => clk,

            we => eu_we(op),
            rs1_data => registerfile_rdata_rs1, rs2_data => registerfile_rdata_rs2, instruction => inst_rdata, pc => pc,
            
            writeback_we => writeback_we(op),
            writeback_result => writeback_data(op),
    
            next_pc => writeback_pc(op),
            update_pc => update_pc(op)
        );
    END GENERATE exclude_invalid;


END GENERATE execunit_gen;

END behavioural;