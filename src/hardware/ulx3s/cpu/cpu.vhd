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
        registerfile_rs1, registerfile_rs2, registerfile_rd : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
        registerfile_wdata_rd : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        registerfile_rdata_rs1, registerfile_rdata_rs2 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        registerfile_we : OUT STD_LOGIC;

        err : OUT STD_LOGIC
    );
END cpu;

ARCHITECTURE behavioural OF cpu IS



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

signal r_registerfile_rdata_rs1, r_registerfile_rdata_rs2 : std_logic_vector(31 downto 0);


FUNCTION f_decode_opcode (
    instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
    RETURN opcode_t IS
variable opcode : std_logic_vector(6 downto 0);
    BEGIN

    opcode := instruction(6 downto 0);

    if opcode = "0110011" then
        return OPCODE_R_TYPE;  -- Register/Register (ADD, ...)
    end if;

    if opcode = "0010011" then
        return OPCODE_I_TYPE; -- Register/Immediate (ADDI, ...)
    end if;

    if opcode = "0000011" then
        return OPCODE_I_TYPE_LOAD;
    end if;

    if opcode = "0100011" then
        return OPCODE_S_TYPE; -- Store (SB, SH, SW)
    end if;

    if opcode = "1100011" then
        return OPCODE_B_TYPE; -- Branch
    end if;

    if opcode = "0110111" then
        return OPCODE_U_TYPE_LUI; -- LUI
    end if;

    if opcode = "0010111" then
        return OPCODE_U_TYPE_AUIPC; -- AUIPC
    end if;

    if opcode = "1101111" then
        return OPCODE_J_TYPE_JAL; -- JAL
    end if;

    if opcode = "1100111" then
        return OPCODE_J_TYPE_JALR; -- JALR
    end if;

    return OPCODE_INVALID;

END;



FUNCTION f_decode_opcode (
    instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
    RETURN opcode_t IS
    VARIABLE opcode : STD_LOGIC_VECTOR(6 DOWNTO 0);
  BEGIN

    opcode := instruction(6 DOWNTO 0);

    IF opcode = "0110011" THEN

      IF funct3 = "000" THEN

        IF funct7 = "0000000" THEN
          RETURN OPCODE_R_TYPE_ADD;
        END IF;

        IF funct7 = "0100000" THEN
          RETURN OPCODE_R_TYPE_SUB;
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


    IMPURE FUNCTION DoShift (
    value : STD_LOGIC_VECTOR(31 DOWNTO 0);
    shamt : STD_LOGIC_VECTOR(4 DOWNTO 0);
    arithmetic_shift : BOOLEAN;
    shleft : BOOLEAN
) RETURN STD_LOGIC_VECTOR IS
    VARIABLE ires, temp : STD_LOGIC_VECTOR(31 DOWNTO 0);
    VARIABLE appendbit : STD_LOGIC;
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

        if (shamt and "10000") /= "00000" then
            ires := ires(15 downto 0) & X"0000";
        end if;

        if (shamt and "01000") /= "00000" then
            ires := ires(23 downto 0) & X"00";
        end if;

        if (shamt and "00100") /= "00000" then
            ires := ires(27 downto 0) & X"0";
        end if;

        if (shamt and "00010") /= "00000" then
            ires := ires(29 downto 0) & "00";
        end if;

        if (shamt and "00001") /= "00000" then
            ires := ires(30 downto 0) & '0';
        end if;

    ELSE
        ires := value;
        
        temp := (others => appendbit);

        if (shamt and "10000") /= "00000" then
            ires := temp(15 downto 0) & ires(31 downto 16);
        end if;

        if (shamt and "01000") /= "00000" then
            ires := temp(7 downto 0) & ires(31 downto 8);
        end if;

        if (shamt and "00100") /= "00000" then
            ires := temp(3 downto 0) & ires(31 downto 4);
        end if;

        if (shamt and "00010") /= "00000" then
            ires := temp(1 downto 0) & ires(31 downto 2);
        end if;

        if (shamt and "00001") /= "00000" then
            ires := temp(0) & ires(31 DOWNTO 1);
        end if;

        
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


FUNCTION f_decode_imm (
    instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0))
    RETURN std_logic_vector IS
variable opcode : opcode_t;
variable imm : std_logic_vector(31 downto 0);
    BEGIN

    opcode := f_decode_opcode(instruction);

    if (opcode = OPCODE_I_TYPE) OR (opcode = OPCODE_I_TYPE_LOAD) then
        imm(31 DOWNTO 11) := (OTHERS => instruction(31));
        imm(10 DOWNTO 5) := instruction(30 DOWNTO 25);
        imm(4 DOWNTO 1) := instruction(24 DOWNTO 21);
        imm(0) := instruction(20);
    end if;


    if opcode = OPCODE_S_TYPE then
        imm(31 DOWNTO 11) := (OTHERS => instruction(31));
        imm(10 DOWNTO 5) := instruction(30 DOWNTO 25);
        imm(4 DOWNTO 1) := instruction(11 DOWNTO 8);
        imm(0) := instruction(7);
    end if;

    if opcode = OPCODE_B_TYPE then
        imm(31 DOWNTO 12) := (OTHERS => instruction(31));
        imm(11) := instruction(7);
        imm(10 DOWNTO 5) := instruction(30 DOWNTO 25);
        imm(4 DOWNTO 1) := instruction(11 DOWNTO 8);
        imm(0) := '0';
    end if;

    if (opcode = OPCODE_U_TYPE_LUI) OR (opcode = OPCODE_U_TYPE_AUIPC) then
        imm(31) := instruction(31);
        imm(30 DOWNTO 20) := instruction(30 DOWNTO 20);
        imm(19 DOWNTO 12) := instruction(19 DOWNTO 12);
        imm(11 DOWNTO 0) := (OTHERS => '0');
    end if;

    if opcode = OPCODE_J_TYPE_JAL then
        imm(31 DOWNTO 20) := (OTHERS => instruction(31));
        imm(19 DOWNTO 12) := instruction(19 DOWNTO 12);
        imm(11) := instruction(20);
        imm(10 DOWNTO 5) := instruction(30 DOWNTO 25);
        imm(4 DOWNTO 1) := instruction(24 DOWNTO 21);
        imm(0) := '0';
    end if;

    if opcode = OPCODE_J_TYPE_JALR then
        imm := (OTHERS => '0');
        imm(11 DOWNTO 0) := instruction(31 DOWNTO 20);
    end if;



    
    return imm;

END;



BEGIN


-- Immediate fields
decode_imm : PROCESS (instruction)
BEGIN
    -- I-type
    imm_i(31 DOWNTO 11) <= (OTHERS => instruction(31));
    imm_i(10 DOWNTO 5) <= instruction(30 DOWNTO 25);
    imm_i(4 DOWNTO 1) <= instruction(24 DOWNTO 21);
    imm_i(0) <= instruction(20);

    -- S-type
    imm_s(31 DOWNTO 11) <= (OTHERS => instruction(31));
    imm_s(10 DOWNTO 5) <= instruction(30 DOWNTO 25);
    imm_s(4 DOWNTO 1) <= instruction(11 DOWNTO 8);
    imm_s(0) <= instruction(7);

    -- B-type
    imm_b(31 DOWNTO 12) <= (OTHERS => instruction(31));
    imm_b(11) <= instruction(7);
    imm_b(10 DOWNTO 5) <= instruction(30 DOWNTO 25);
    imm_b(4 DOWNTO 1) <= instruction(11 DOWNTO 8);
    imm_b(0) <= '0';

    -- U-type
    imm_u(31) <= instruction(31);
    imm_u(30 DOWNTO 20) <= instruction(30 DOWNTO 20);
    imm_u(19 DOWNTO 12) <= instruction(19 DOWNTO 12);
    imm_u(11 DOWNTO 0) <= (OTHERS => '0');

    -- J-type
    imm_j(31 DOWNTO 20) <= (OTHERS => instruction(31));
    imm_j(19 DOWNTO 12) <= instruction(19 DOWNTO 12);
    imm_j(11) <= instruction(20);
    imm_j(10 DOWNTO 5) <= instruction(30 DOWNTO 25);
    imm_j(4 DOWNTO 1) <= instruction(24 DOWNTO 21);
    imm_j(0) <= '0';

    -- JALR
    imm_jalr <= (OTHERS => '0');
    imm_jalr(11 DOWNTO 0) <= instruction(31 DOWNTO 20);

END PROCESS;





inst_addr <= pc;
data_wdata <= i_data_wdata;
data_re <= i_data_re;
data_we <= i_data_we;
data_addr <= i_data_addr;




registerfile_rs1 <= rs1;
registerfile_rs2 <= rs2;
registerfile_rd <= rd;
registerfile_wdata_rd <= result(f_decode_opcode(instruction));
inst_width <= "10";

fsm : PROCESS (state, funct3, instruction, inst_rdata, pc, opcode, inst_rdy, opcode, decode_error, writeback, update_pc, r_next_pc, result, daddr, dwe, dre)
BEGIN
    n_state <= state;
    n_pc <= pc;
    registerfile_we <= '0';
    set_instruction <= '0';
    inst_re <= '1';
    err <= '0';
    selected <= (OTHERS => '0');
--    data_width <= (OTHERS => '0');
--    i_data_addr <= (OTHERS => '0');
--    i_data_wdata <= (OTHERS => '0');
    i_data_re <= '0';
    i_data_we <= '0';
    set_instruction_of_opcode <= (others => '0');


    CASE state IS

        -- when SEND_CHAR_0 =>
        --     i_data_wdata <= X"000000" & pc(31 downto 24);
        --     i_data_we <= '1';
        --     if data_wack = '1' then
        --         n_state <= SEND_CHAR_1;
        --     end if;

        -- when SEND_CHAR_1 =>
        --     i_data_wdata <= X"000000" & pc(23 downto 16);
        --     i_data_we <= '1';
        --     if data_wack = '1' then
        --         n_state <= SEND_CHAR_2;
        --     end if;

        -- when SEND_CHAR_2 =>
        --     i_data_wdata <= X"000000" & pc(15 downto 8);
        --     i_data_we <= '1';
        --     if data_wack = '1' then
        --         n_state <= SEND_CHAR_3;
        --     end if;

        -- when SEND_CHAR_3 =>
        --     i_data_wdata <= X"000000" & pc(7 downto 0);
        --     i_data_we <= '1';
        --     if data_wack = '1' then
        --         n_state <= FETCH_INSTRUCTION;
        --     end if;




        WHEN FETCH_INSTRUCTION =>
            inst_re <= '1';
            --set_instruction_of_opcode(f_decode_opcode(inst_rdata)) <= '1';

            IF inst_rdy = '1' THEN
                n_state <= EXECUTE_1;
            END IF;

        WHEN EXECUTE_1 =>
            set_instruction_of_opcode(f_decode_opcode(inst_rdata)) <= '1';
            inst_re <= '1';
            set_instruction <= '1';
            n_state <= EXECUTE_2;

            WHEN EXECUTE_2 =>
            set_instruction_of_opcode(f_decode_opcode(inst_rdata)) <= '1';
            inst_re <= '1';
            set_instruction <= '1';
            n_state <= EXECUTE_3;

            WHEN EXECUTE_3 =>
            set_instruction_of_opcode(f_decode_opcode(inst_rdata)) <= '1';
            inst_re <= '1';
            set_instruction <= '1';
            n_state <= EXECUTE_4;
        

            WHEN EXECUTE_4 =>
            set_instruction_of_opcode(f_decode_opcode(inst_rdata)) <= '1';
            inst_re <= '1';
            set_instruction <= '1';
            n_state <= EXECUTE_5;

            WHEN EXECUTE_5 =>
            set_instruction_of_opcode(f_decode_opcode(inst_rdata)) <= '1';
            inst_re <= '1';
            set_instruction <= '1';
            n_state <= EXECUTE_6;


            WHEN EXECUTE_6 =>
            set_instruction_of_opcode(f_decode_opcode(inst_rdata)) <= '1';
            inst_re <= '1';
            set_instruction <= '1';
            n_state <= EXECUTE_7;


            WHEN EXECUTE_7 =>
            set_instruction_of_opcode(f_decode_opcode(inst_rdata)) <= '1';
            inst_re <= '1';
            set_instruction <= '1';
            n_state <= EXECUTE_8;



        -- when SEND_CHAR_0 =>
        --     i_data_wdata <= X"000000" & instruction(31 downto 24);
        --     i_data_we <= '1';
        --     if data_wack = '1' then
        --         n_state <= SEND_CHAR_1;
        --     end if;
    
        -- when SEND_CHAR_1 =>
        --     i_data_wdata <= X"000000" & instruction(23 downto 16);
        --     i_data_we <= '1';
        --     if data_wack = '1' then
        --         n_state <= SEND_CHAR_2;
        --     end if;
    
        -- when SEND_CHAR_2 =>
        --     i_data_wdata <= X"000000" & instruction(15 downto 8);
        --     i_data_we <= '1';
        --     if data_wack = '1' then
        --         n_state <= SEND_CHAR_3;
        --     end if;
    
        -- when SEND_CHAR_3 =>
        --     i_data_wdata <= X"000000" & instruction(7 downto 0);
        --     i_data_we <= '1';
        --     if data_wack = '1' then
        --         n_state <= EXECUTE_2;
        --     end if;



        
        WHEN EXECUTE_8 =>

        



            i_data_re <= dre(f_decode_opcode(instruction));
            i_data_we <= dwe(f_decode_opcode(instruction));

            selected(f_decode_opcode(instruction)) <= '1';

            IF update_pc(f_decode_opcode(instruction)) = '1' THEN
                --set_instruction <= '1';
                n_pc <= next_pc(f_decode_opcode(instruction));
                n_state <= FETCH_INSTRUCTION;
                IF writeback(f_decode_opcode(instruction)) = '1' THEN
                    registerfile_we <= '1';
                END IF;
            ELSIF decode_error(f_decode_opcode(instruction)) = '1' THEN
                n_state <= PANIC;
            END IF;
            

        WHEN PANIC =>
            err <= '1';
        WHEN OTHERS =>
            n_state <= FETCH_INSTRUCTION;
    END CASE;
END PROCESS;


data_width <= funct3(1 DOWNTO 0);
i_data_addr <= daddr(f_decode_opcode(instruction));
i_data_wdata <= r_registerfile_rdata_rs2; --wdata(f_decode_opcode(instruction));

synchronous : PROCESS (rst, clk)
BEGIN
    IF rst = '1' THEN
        state <= FETCH_INSTRUCTION;
        instruction <= (OTHERS => '0');
        pc <= entry_point;

        r_registerfile_rdata_rs1 <= (others => '0');
         r_registerfile_rdata_rs2 <= (others => '0');
         instruction_of_opcode <= (others => (others => '0'));
    ELSIF rising_edge(clk) THEN 
        if update_pc(f_decode_opcode(instruction)) = '1' THEN
            pc <= next_pc(f_decode_opcode(instruction));
        end if;

--        pc <= n_pc;

        for i in opcode_t loop
        IF set_instruction_of_opcode(i) = '1' THEN
            instruction_of_opcode(i) <= inst_rdata;
            registerfile_rdata_rs1_of_opcode(i) <= registerfile_rdata_rs1;
            registerfile_rdata_rs2_of_opcode(i) <= registerfile_rdata_rs2;
        END IF;
        end loop;

        instruction <= inst_rdata;

        r_registerfile_rdata_rs1 <= registerfile_rdata_rs1;
        r_registerfile_rdata_rs2 <= registerfile_rdata_rs2;

        state <= n_state;

    END IF;
END PROCESS;

decode_store : PROCESS (clk, opcode, imm_s, pc, r_registerfile_rdata_rs1, registerfile_rdata_rs2, data_wack, funct3, selected)
BEGIN

        update_pc(OPCODE_S_TYPE) <= data_wack;

        writeback(OPCODE_S_TYPE) <= '0';

        result(OPCODE_S_TYPE) <= imm_s;
        next_pc(OPCODE_S_TYPE) <= pc + X"00000004";

        decode_error(OPCODE_S_TYPE) <= '0';

        daddr(OPCODE_S_TYPE) <= r_registerfile_rdata_rs1 + imm_s;
        dwe(OPCODE_S_TYPE) <= '1';

END PROCESS;

decode_load : PROCESS (imm_i, pc, r_registerfile_rdata_rs1, data_rdy, data_rdata, funct3, opcode)
BEGIN
        writeback(OPCODE_I_TYPE_LOAD) <= '1';

        next_pc(OPCODE_I_TYPE_LOAD) <= pc + X"00000004";
        update_pc(OPCODE_I_TYPE_LOAD) <= data_rdy;
        decode_error(OPCODE_I_TYPE_LOAD) <= '0';

        daddr(OPCODE_I_TYPE_LOAD) <= r_registerfile_rdata_rs1 + imm_i;
        dre(OPCODE_I_TYPE_LOAD) <= '1';

        result(OPCODE_I_TYPE_LOAD) <= data_rdata;

        IF (funct3(2) = '0') THEN
            CASE funct3(1 DOWNTO 0) IS
                WHEN "00" =>
                    result(OPCODE_I_TYPE_LOAD)(31 DOWNTO 8) <= (OTHERS => data_rdata(7));

                WHEN "01" =>
                    result(OPCODE_I_TYPE_LOAD)(31 DOWNTO 16) <= (OTHERS => data_rdata(15));

                WHEN "11" =>
                    decode_error(OPCODE_I_TYPE_LOAD) <= '1';

                WHEN OTHERS =>

            END CASE;
        END IF;


END PROCESS;

decode_lui : PROCESS (imm_u, pc)
BEGIN

        result(OPCODE_U_TYPE_LUI) <= imm_u;
        writeback(OPCODE_U_TYPE_LUI) <= '1';
        next_pc(OPCODE_U_TYPE_LUI) <= pc + X"00000004";
        update_pc(OPCODE_U_TYPE_LUI) <= '1';
        decode_error(OPCODE_U_TYPE_LUI) <= '0';


END PROCESS;

decode_auipc : PROCESS (imm_u, pc)
BEGIN

        writeback(OPCODE_U_TYPE_AUIPC) <= '1';
        result(OPCODE_U_TYPE_AUIPC) <= pc + imm_u;
        next_pc(OPCODE_U_TYPE_AUIPC) <= pc + X"00000004";
        decode_error(OPCODE_U_TYPE_AUIPC) <= '0';
        update_pc(OPCODE_U_TYPE_AUIPC) <= '1';

END PROCESS;

--update_pc(111) <= '1';
decode_jal : PROCESS (imm_j, pc)
BEGIN
        writeback(OPCODE_J_TYPE_JAL) <= '1';
        result(OPCODE_J_TYPE_JAL) <= pc + X"00000004";
        next_pc(OPCODE_J_TYPE_JAL) <= pc + imm_j;
        decode_error(OPCODE_J_TYPE_JAL) <= '0';
        update_pc(OPCODE_J_TYPE_JAL) <= '1';

END PROCESS;

decode_jalr : PROCESS (r_registerfile_rdata_rs1, pc, imm_jalr)
BEGIN
        writeback(OPCODE_J_TYPE_JALR) <= '1';
        result(OPCODE_J_TYPE_JALR) <= pc + X"00000004";
        next_pc(OPCODE_J_TYPE_JALR) <= (imm_jalr + r_registerfile_rdata_rs1) AND X"FFFFFFFE"; --(pc + r_registerfile_rdata_rs1) and X"FFFFFFFE";
        update_pc(OPCODE_J_TYPE_JALR) <= '1';
        decode_error(OPCODE_J_TYPE_JALR) <= '0';


END PROCESS;

decode_b_type : PROCESS (opcode, funct3, registerfile_rdata_rs1_of_opcode, registerfile_rdata_rs2_of_opcode, imm_b, pc, instruction_of_opcode(OPCODE_B_TYPE))
BEGIN


        result(OPCODE_B_TYPE) <= (OTHERS => '0');
        next_pc(OPCODE_B_TYPE) <= pc + X"00000004";

        decode_error(OPCODE_B_TYPE) <= '0';
        update_pc(OPCODE_B_TYPE) <= '1';

        CASE funct3 IS
            WHEN "000" => -- BEQ
                IF signed(registerfile_rdata_rs1_of_opcode(OPCODE_B_TYPE)) = signed(registerfile_rdata_rs2_of_opcode(OPCODE_B_TYPE)) THEN
                    next_pc(OPCODE_B_TYPE) <= pc + f_decode_imm(instruction_of_opcode(OPCODE_B_TYPE)); --imm_b;
                END IF;
            WHEN "001" => -- BNE
                IF signed(registerfile_rdata_rs1_of_opcode(OPCODE_B_TYPE)) /= signed(registerfile_rdata_rs2_of_opcode(OPCODE_B_TYPE)) THEN
                    next_pc(OPCODE_B_TYPE) <= pc + f_decode_imm(instruction_of_opcode(OPCODE_B_TYPE)); --imm_b;
                END IF;
            WHEN "100" => -- BLT
                IF signed(registerfile_rdata_rs1_of_opcode(OPCODE_B_TYPE)) < signed(registerfile_rdata_rs2_of_opcode(OPCODE_B_TYPE)) THEN
                    next_pc(OPCODE_B_TYPE) <= pc + f_decode_imm(instruction_of_opcode(OPCODE_B_TYPE)); --imm_b;
                END IF;
            WHEN "101" => -- BGE
                IF signed(registerfile_rdata_rs1_of_opcode(OPCODE_B_TYPE)) >= signed(registerfile_rdata_rs2_of_opcode(OPCODE_B_TYPE)) THEN
                    next_pc(OPCODE_B_TYPE) <= pc + f_decode_imm(instruction_of_opcode(OPCODE_B_TYPE)); --imm_b;
                END IF;
            WHEN "110" => -- BLTU
                IF unsigned(registerfile_rdata_rs1_of_opcode(OPCODE_B_TYPE)) < unsigned(registerfile_rdata_rs2_of_opcode(OPCODE_B_TYPE)) THEN
                    next_pc(OPCODE_B_TYPE) <= pc + f_decode_imm(instruction_of_opcode(OPCODE_B_TYPE)); --imm_b;
                END IF;
            WHEN "111" => -- BGEU
                IF unsigned(registerfile_rdata_rs1_of_opcode(OPCODE_B_TYPE)) >= unsigned(registerfile_rdata_rs2_of_opcode(OPCODE_B_TYPE)) THEN
                    next_pc(OPCODE_B_TYPE) <= pc + f_decode_imm(instruction_of_opcode(OPCODE_B_TYPE)); --imm_b;
                END IF;
            WHEN OTHERS =>
                decode_error(OPCODE_B_TYPE) <= '1';
        END CASE;



END PROCESS;
decode_r_type : PROCESS (opcode, funct3, funct7, registerfile_rdata_rs1_of_opcode, registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE), pc) --clk, pc)
BEGIN
        writeback(OPCODE_R_TYPE) <= '1';
        next_pc(OPCODE_R_TYPE) <= pc + X"00000004";
        decode_error(OPCODE_R_TYPE) <= '0';
        result(OPCODE_R_TYPE) <= (OTHERS => '0');
        update_pc(OPCODE_R_TYPE) <= '1';

        CASE funct3 IS
            WHEN "000" =>
                CASE funct7 IS
                    WHEN "0000000" => -- ADD
                        result(OPCODE_R_TYPE) <= registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE) + registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE);

                    WHEN "0100000" => -- SUB
                        result(OPCODE_R_TYPE) <= registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE) - registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE);

                    WHEN OTHERS =>
                        decode_error(OPCODE_R_TYPE) <= '1';
                END CASE;

            WHEN "001" => -- SLL
                update_pc(OPCODE_R_TYPE) <= '1';
                result(OPCODE_R_TYPE) <= DoShift(registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE), registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE)(4 DOWNTO 0), false, true);

            WHEN "010" => -- SLT
                IF signed(registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE)) < signed(registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE)) THEN
                    result(OPCODE_R_TYPE) <= X"00000001";
                ELSE
                    result(OPCODE_R_TYPE) <= (OTHERS => '0');
                END IF;

            WHEN "011" => -- SLTU
                IF unsigned(registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE)) < unsigned(registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE)) THEN
                    result(OPCODE_R_TYPE) <= X"00000001";
                ELSE
                    result(OPCODE_R_TYPE) <= (OTHERS => '0');
                END IF;

            WHEN "100" => -- XOR
                result(OPCODE_R_TYPE) <= registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE) XOR registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE);

            WHEN "101" =>
                update_pc(OPCODE_R_TYPE) <= '0';

                CASE funct7 IS
                    WHEN "0000000" => -- SRL

                        update_pc(OPCODE_R_TYPE) <= '1';
                        result(OPCODE_R_TYPE) <= DoShift(registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE), registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE)(4 DOWNTO 0), false, false);

                    WHEN "0100000" => -- SRA
                        update_pc(OPCODE_R_TYPE) <= '1';
                        result(OPCODE_R_TYPE) <= DoShift(registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE), registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE)(4 DOWNTO 0), true, false);

                    WHEN OTHERS =>
                        decode_error(OPCODE_R_TYPE) <= '1';
                END CASE;

            WHEN "110" => -- OR
                result(OPCODE_R_TYPE) <= registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE) OR registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE);

            WHEN "111" => -- AND
                result(OPCODE_R_TYPE) <= registerfile_rdata_rs1_of_opcode(OPCODE_R_TYPE) AND registerfile_rdata_rs2_of_opcode(OPCODE_R_TYPE);
            WHEN OTHERS =>
                decode_error(OPCODE_R_TYPE) <= '1';
        END CASE;
END PROCESS;

decode_i_type : PROCESS (opcode, imm_i, funct3, funct7, registerfile_rdata_rs1_of_opcode, pc) --clk, pc)
BEGIN

        decode_error(OPCODE_I_TYPE) <= '0';
        update_pc(OPCODE_I_TYPE) <= '1';
        next_pc(OPCODE_I_TYPE) <= pc + X"00000004";

        writeback(OPCODE_I_TYPE) <= '1';

        result(OPCODE_I_TYPE) <= (OTHERS => '0');

        CASE funct3 IS
            WHEN "000" => -- ADDI
                result(OPCODE_I_TYPE) <= registerfile_rdata_rs1_of_opcode(OPCODE_I_TYPE) + f_decode_imm(instruction_of_opcode(OPCODE_I_TYPE));

            WHEN "001" =>
                CASE funct7 IS
                    WHEN "0000000" => -- SLLI
                        update_pc(OPCODE_I_TYPE) <= '1';
                        result(OPCODE_I_TYPE) <= DoShift(registerfile_rdata_rs1_of_opcode(OPCODE_I_TYPE), f_decode_imm(instruction_of_opcode(OPCODE_I_TYPE))(4 DOWNTO 0), false, true);

                    WHEN OTHERS =>
                        decode_error(OPCODE_I_TYPE) <= '1';
                END CASE;
            WHEN "010" => -- SLTI
                IF signed(registerfile_rdata_rs1_of_opcode(OPCODE_I_TYPE)) < signed(f_decode_imm(instruction_of_opcode(OPCODE_I_TYPE))) THEN
                    result(OPCODE_I_TYPE) <= X"00000001";
                ELSE
                    result(OPCODE_I_TYPE) <= (OTHERS => '0');
                END IF;

            WHEN "011" => -- SLTIU
                IF unsigned(registerfile_rdata_rs1_of_opcode(OPCODE_I_TYPE)) < unsigned(f_decode_imm(instruction_of_opcode(OPCODE_I_TYPE))) THEN
                    result(OPCODE_I_TYPE) <= X"00000001";
                ELSE
                    result(OPCODE_I_TYPE) <= (OTHERS => '0');
                END IF;

            WHEN "100" => -- XORI
                result(OPCODE_I_TYPE) <= registerfile_rdata_rs1_of_opcode(OPCODE_I_TYPE) XOR f_decode_imm(instruction_of_opcode(OPCODE_I_TYPE));

            WHEN "101" =>
                update_pc(OPCODE_I_TYPE) <= '0';

                CASE funct7 IS
                    WHEN "0000000" => -- SRLI
                        update_pc(OPCODE_I_TYPE) <= '1';
                        result(OPCODE_I_TYPE) <= DoShift(registerfile_rdata_rs1_of_opcode(OPCODE_I_TYPE), f_decode_imm(instruction_of_opcode(OPCODE_I_TYPE))(4 DOWNTO 0), false, false);

                    WHEN "0100000" => -- SRAI
                        update_pc(OPCODE_I_TYPE) <= '1';
                        result(OPCODE_I_TYPE) <= DoShift(registerfile_rdata_rs1_of_opcode(OPCODE_I_TYPE), f_decode_imm(instruction_of_opcode(OPCODE_I_TYPE))(4 DOWNTO 0), true, false);

                    WHEN OTHERS =>
                        decode_error(OPCODE_I_TYPE) <= '1';
                END CASE;
            WHEN "110" => -- ORI
                result(OPCODE_I_TYPE) <= registerfile_rdata_rs1_of_opcode(OPCODE_I_TYPE) OR f_decode_imm(instruction_of_opcode(OPCODE_I_TYPE));

            WHEN "111" => -- ANDI
                result(OPCODE_I_TYPE) <= registerfile_rdata_rs1_of_opcode(OPCODE_I_TYPE) AND f_decode_imm(instruction_of_opcode(OPCODE_I_TYPE));

            WHEN OTHERS =>
                decode_error(OPCODE_I_TYPE) <= '1';

        END CASE;
END PROCESS;

--interrupt_error <= instruction_details_array(f_decode_opcode(instruction)).decode_error;
--exec_done <= update_pc(f_decode_opcode(instruction));
END behavioural;