library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.std_logic_unsigned.all;

entity cpu is
    generic (entry_point : std_logic_vector(31 downto 0) := X"80010000");

  Port (
    rst, clk : in std_logic;

    -- Instruction memory bus
    inst_width : out std_logic_vector(1 downto 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
    inst_addr : out std_logic_vector(31 downto 0);
    inst_rdata : in std_logic_vector(31 downto 0);
    inst_re : out std_logic;
    inst_rdy : in std_logic;

    -- Data memory bus
    data_width : out std_logic_vector(1 downto 0); -- "00" -> 1 byte, "01" -> 2 bytes, "10" -> 4 bytes, "11" -> invalid / 8 bytes for RV64
    data_addr, data_wdata : out std_logic_vector(31 downto 0);
    data_rdata : in std_logic_vector(31 downto 0);
    data_re, data_we : out std_logic;
    data_rdy, data_wack : in std_logic;

    -- Register file
    registerfile_rs1, registerfile_rs2, registerfile_rd : out std_logic_vector(4 downto 0);
    registerfile_wdata_rd : out std_logic_vector(31 downto 0);
    registerfile_rdata_rs1, registerfile_rdata_rs2 : in std_logic_vector(31 downto 0);
    registerfile_we : out std_logic;

    err : out std_logic
  );
end cpu;

architecture behavioural of cpu is

    type instruction_details_t is
        record
            selected, decode_error, execution_done, use_rs1, use_rs2, use_rd, decrement_counter : std_logic;
            result, next_pc, imm : std_logic_vector(31 downto 0);

            -- mmu interface
            data_width : std_logic_vector(1 downto 0);
            data_addr, data_wdata : std_logic_vector(31 downto 0);
            data_re, data_we : std_logic;
        end record;
    constant init_instruction_details: instruction_details_t := (selected => '0', imm => (others => '0'), execution_done => '0', decode_error => '1', result => (others => '0'), next_pc => (others => '0'), use_rs1 => '0', use_rs2 => '0', use_rd => '0', decrement_counter => '0',
                                                                    data_width => "10", data_addr => (others => '0'), data_wdata => (others => '0'), data_re => '0', data_we => '0');
    type instruction_details_array_t is array (natural range <>) of instruction_details_t;
    signal instruction_details_array : instruction_details_array_t(127 downto 0) := (others => init_instruction_details);


    signal opcode, funct7 : std_logic_vector(6 downto 0);
    signal instruction, n_instruction : std_logic_vector(31 downto 0);

    signal pc, n_pc : std_logic_vector(31 downto 0);
    signal rs1, rs2, rd : std_logic_vector(4 downto 0);
    signal funct3 : std_logic_vector(2 downto 0);


    signal imm_i, imm_s, imm_b, imm_u, imm_j, imm_jalr : std_logic_vector(31 downto 0);

    constant R_TYPE         : std_logic_vector(6 downto 0) := "0110011"; -- Register/Register (ADD, ...)
    constant I_TYPE         : std_logic_vector(6 downto 0) := "0010011"; -- Register/Immediate (ADDI, ...)
    constant I_TYPE_LOAD    : std_logic_vector(6 downto 0) := "0000011";
    constant S_TYPE         : std_logic_vector(6 downto 0) := "0100011"; -- Store (SB, SH, SW)
    constant B_TYPE         : std_logic_vector(6 downto 0) := "1100011"; -- Branch
    constant U_TYPE_LUI     : std_logic_vector(6 downto 0) := "0110111"; -- LUI
    constant U_TYPE_AUIPC   : std_logic_vector(6 downto 0) := "0010111"; -- AUIPC
    constant J_TYPE_JAL     : std_logic_vector(6 downto 0) := "1101111"; -- JAL
    constant J_TYPE_JALR    : std_logic_vector(6 downto 0) := "1100111"; -- JALR
    

    type state_t is (FETCH_INSTRUCTION, WAIT_UNTIL_RD_UNLOCKED, FETCH_RS1, FETCH_RS2, EXECUTE, WRITEBACK, INCREMENT_PC, PANIC);
    attribute syn_encoding : string; attribute syn_encoding of state_t : type is "one-hot";
    signal state, n_state : state_t;

    signal set_instruction : std_logic;

    signal decode_error : std_logic_vector(127 downto 0) := (others => '1');
    signal use_rs1 : std_logic_vector(127 downto 0) := (others => '0');
    signal use_rs2 : std_logic_vector(127 downto 0) := (others => '0');
    signal use_rd : std_logic_vector(127 downto 0) := (others => '0');
    signal execution_done : std_logic_vector(127 downto 0) := (others => '1');
    signal dec_counter : std_logic_vector(127 downto 0) := (others => '0');
    signal dwe : std_logic_vector(127 downto 0) := (others => '0'); -- data_we
    signal selected : std_logic_vector(127 downto 0) := (others => '0');


    type word_t is array (natural range <>) of std_logic_vector(31 downto 0);
    signal next_pc : word_t(127 downto 0) := (others => (others => '0'));
    signal result :  word_t(127 downto 0) := (others => (others => '0'));
    signal imm : word_t(127 downto 0) := (others => (others => '0'));
    signal wdata : word_t(127 downto 0) := (others => (others => '0'));
    signal daddr : word_t(127 downto 0) := (others => (others => '0'));

    signal state_out : std_logic_vector(2 downto 0);

    signal i_inst_addr, i_data_wdata, i_data_addr : std_logic_vector(31 downto 0);
    signal i_data_we, i_data_re : std_logic;

    component ila_0 PORT (
      clk : in std_logic;
      probe2, probe3, probe4, probe5, probe6, probe7, probe8 : in std_logic_vector(31 downto 0);
      probe0, probe9, probe10 : in std_logic;
      probe1 : in std_logic_vector(2 downto 0);
      probe11, probe12 : in std_logic_vector(127 downto 0)
    );
    end component;


    impure function DoShift (
        value : std_logic_vector(31 downto 0); 
        shamt : integer range 0 to 31;
        arithmetic_shift : boolean; 
        shleft : boolean
    ) return std_logic_vector is
        variable result : std_logic_vector(31 downto 0);
        variable appendbit : std_logic;
    begin
        if arithmetic_shift = true then
            appendbit := value(31);
        else
            appendbit := '0';
        end if;

        if shamt > 31 then
            result := (others => appendbit);
            return result;
        elsif shamt = 0 then
            return value;
        end if;

        if shleft = true then
            result := (others => '0');
            result(31 downto shamt) := value(31-shamt downto 0);
        else
            result := (others => appendbit);
            result(31-shamt downto 0) := value(31 downto shamt);
        end if;
        return result;
    end function;

    impure function DoDivision (
        dividend, divisor : std_logic_vector(31 downto 0)
    ) return std_logic_vector is
        variable temp, quotient : std_logic_vector(31 downto 0) := (others => '0');
    begin
        temp := dividend;
        while temp /= X"00000000" loop
            if temp > divisor then
                quotient := quotient + X"00000001";
                temp := temp - divisor;
            else
                temp := X"00000000";
            end if;
        end loop;
        return quotient;
    end function;

begin
    i_inst_addr <= pc;
    inst_addr <= i_inst_addr;
    data_wdata <= i_data_wdata;
    data_re <= i_data_re;
    data_we <= i_data_we;
    data_addr <= i_data_addr;

    -- ila: ila_0 PORT MAP(
    --   clk => clk,
    --   probe0 => rst,
    --   probe1 => state_out,
    --   probe2 => pc,
    --   probe3 => i_inst_addr,
    --   probe4 => inst_rdata,
    --   probe5 => instruction,
    --   probe6 => data_rdata,
    --   probe7 => i_data_wdata,
    --   probe8 => i_data_addr,
    --   probe9 => i_data_we,
    --   probe10 => i_data_re,
    --   probe11 => selected,
    --   probe12 => execution_done
    -- );



    funct7 <= instruction(31 downto 25);
    rs2 <= instruction(24 downto 20);
    rs1 <= instruction(19 downto 15);
    funct3 <= instruction(14 downto 12);
    rd <= instruction(11 downto 7);
    opcode <= instruction(6 downto 0);

    registerfile_rs1 <= rs1;
    registerfile_rs2 <= rs2;
    registerfile_rd <= rd;
    registerfile_wdata_rd <= result(to_integer(unsigned(opcode)));
    inst_width <= "10";

    fsm: process(state, instruction_details_array, pc, inst_rdy, opcode, decode_error, use_rd, execution_done, next_pc, result,daddr, dwe)
    begin
        n_state <= state;
        n_pc <= pc;
        registerfile_we <= '0';
        set_instruction <= '0';
        inst_re <= '0';
        err <= '0';
        selected <= (others => '0');


        data_width <= (others => '0');
        i_data_addr <= (others => '0');
        i_data_wdata <= (others => '0');
        i_data_re <= '0';
        i_data_we <= '0';

        case state is
            when FETCH_INSTRUCTION =>
                set_instruction <= '1';
                if inst_rdy = '1' then
                    inst_re <= '1';
                    n_state <= EXECUTE;
                end if;
            
            when EXECUTE =>

                data_width <= instruction_details_array(to_integer(unsigned(opcode))).data_width;
                i_data_addr <= daddr(to_integer(unsigned(opcode)));
                i_data_wdata <= wdata(to_integer(unsigned(opcode)));
                i_data_re <= instruction_details_array(to_integer(unsigned(opcode))).data_re;
                i_data_we <= dwe(to_integer(unsigned(opcode)));

                selected(to_integer(unsigned(opcode))) <= '1';

                if execution_done(to_integer(unsigned(opcode))) = '1' then
                    --set_instruction <= '1';
                    n_pc <= next_pc(to_integer(unsigned(opcode)));
                    n_state <= FETCH_INSTRUCTION;
                    if use_rd(to_integer(unsigned(opcode))) = '1' then
                        registerfile_we <= '1';
                    end if;
                elsif decode_error(to_integer(unsigned(opcode))) = '1' then
                    n_state <= PANIC;
                end if;

            when PANIC =>
                err <= '1';
            when others =>
                n_state <= FETCH_INSTRUCTION;
        end case;
    end process;

    synchronous: process(rst, clk)
    begin
        if rst = '1' then
            state <= FETCH_INSTRUCTION;
            instruction <= (others => '0');
            pc <= entry_point;
        elsif rising_edge(clk) then
            pc <= n_pc;
            if set_instruction = '1' then
                instruction <= inst_rdata;
            end if;

            state <= n_state;

        end if;
    end process;



    decode_store: process(imm_s, pc, registerfile_rdata_rs1, registerfile_rdata_rs2, data_wack, funct3, selected(to_integer(unsigned(S_TYPE))))
    begin
        imm(to_integer(unsigned(S_TYPE))) <= imm_s;
        result(to_integer(unsigned(S_TYPE))) <= imm_s;
        use_rs1(to_integer(unsigned(S_TYPE))) <= '1';
        use_rs2(to_integer(unsigned(S_TYPE))) <= '1';
        next_pc(to_integer(unsigned(S_TYPE))) <= pc + X"00000004";
        execution_done(to_integer(unsigned(S_TYPE))) <= data_wack;
        decode_error(to_integer(unsigned(S_TYPE))) <= '0';

        daddr(to_integer(unsigned(S_TYPE))) <= registerfile_rdata_rs1 + imm_s;
        wdata(to_integer(unsigned(S_TYPE)))<= registerfile_rdata_rs2;
        dwe(to_integer(unsigned(S_TYPE))) <= selected(to_integer(unsigned(S_TYPE)));
        instruction_details_array(to_integer(unsigned(S_TYPE))).data_width <= funct3(1 downto 0);
    end process;

    decode_load: process(imm_i, pc, registerfile_rdata_rs1, data_rdy, data_rdata, funct3)
    begin
        imm(to_integer(unsigned(I_TYPE_LOAD))) <= imm_i;
        use_rs1(to_integer(unsigned(I_TYPE_LOAD))) <= '1';
        use_rd(to_integer(unsigned(I_TYPE_LOAD))) <= '1';

        next_pc(to_integer(unsigned(I_TYPE_LOAD))) <= pc + X"00000004";
        execution_done(to_integer(unsigned(I_TYPE_LOAD))) <= data_rdy;
        decode_error(to_integer(unsigned(I_TYPE_LOAD))) <= '0';

        daddr(to_integer(unsigned(I_TYPE_LOAD))) <= registerfile_rdata_rs1 + imm_i;
        instruction_details_array(to_integer(unsigned(I_TYPE_LOAD))).data_re <= '1';

        result(to_integer(unsigned(I_TYPE_LOAD))) <= data_rdata;

        instruction_details_array(to_integer(unsigned(I_TYPE_LOAD))).data_width <= funct3(1 downto 0);


        if(funct3(2) = '0') then
            case funct3(1 downto 0) is
                when "00" =>
                    result(to_integer(unsigned(I_TYPE_LOAD)))(31 downto 8) <= (others => data_rdata(7));

                when "01" =>
                    result(to_integer(unsigned(I_TYPE_LOAD)))(31 downto 16) <= (others => data_rdata(15));

                when "11" =>
                    decode_error(to_integer(unsigned(I_TYPE_LOAD))) <= '1';

                when others =>

            end case;
        end if;

    end process;

    execution_done(55) <= '1';
    decode_lui: process(imm_u, pc)
    begin
        imm(to_integer(unsigned(U_TYPE_LUI))) <= imm_u;
        result(to_integer(unsigned(U_TYPE_LUI))) <= imm_u;
        use_rd(to_integer(unsigned(U_TYPE_LUI))) <= '1';
        next_pc(to_integer(unsigned(U_TYPE_LUI))) <= pc + X"00000004";
        --execution_done(to_integer(unsigned(U_TYPE_LUI))) <= '1';
        decode_error(to_integer(unsigned(U_TYPE_LUI))) <= '0';
    end process;

    execution_done(23) <= '1';
    decode_auipc: process(imm_u, pc)
    begin
        imm(to_integer(unsigned(U_TYPE_AUIPC))) <= imm_u;
        use_rd(to_integer(unsigned(U_TYPE_AUIPC))) <= '1';
        result(to_integer(unsigned(U_TYPE_AUIPC))) <= pc + imm_u;
        next_pc(to_integer(unsigned(U_TYPE_AUIPC))) <= pc + X"00000004";
        --execution_done(to_integer(unsigned(U_TYPE_AUIPC))) <= '1';
        decode_error(to_integer(unsigned(U_TYPE_AUIPC))) <= '0';
    end process;

    execution_done(111) <= '1';
    decode_jal: process(imm_j, pc)
    begin
        imm(to_integer(unsigned(J_TYPE_JAL))) <= imm_j;
        use_rd(to_integer(unsigned(J_TYPE_JAL))) <= '1';
        result(to_integer(unsigned(J_TYPE_JAL))) <= pc + X"00000004";
        next_pc(to_integer(unsigned(J_TYPE_JAL))) <= pc + imm_j;
        --execution_done(to_integer(unsigned(J_TYPE_JAL))) <= '1';
        decode_error(to_integer(unsigned(J_TYPE_JAL))) <= '0';
    end process;

    execution_done(103) <= '1';
    decode_jalr: process(registerfile_rdata_rs1, pc, imm_jalr)
    begin
        imm(to_integer(unsigned(J_TYPE_JALR))) <= imm_jalr;
        use_rd(to_integer(unsigned(J_TYPE_JALR))) <= '1';
        use_rs1(to_integer(unsigned(J_TYPE_JALR))) <= '1';
        result(to_integer(unsigned(J_TYPE_JALR))) <= pc + X"00000004";
        next_pc(to_integer(unsigned(J_TYPE_JALR))) <= (imm_jalr + registerfile_rdata_rs1) and X"FFFFFFFE"; --(pc + registerfile_rdata_rs1) and X"FFFFFFFE";
        --execution_done(to_integer(unsigned(J_TYPE_JALR))) <= '1';
        decode_error(to_integer(unsigned(J_TYPE_JALR))) <= '0';
    end process;

    execution_done(99) <= '1';
    decode_b_type: process(funct3, registerfile_rdata_rs1, registerfile_rdata_rs2, imm_b, pc)
    begin
        imm(to_integer(unsigned(B_TYPE))) <= imm_b;

        result(to_integer(unsigned(B_TYPE))) <= (others => '0');
        use_rs1(to_integer(unsigned(B_TYPE))) <= '1';
        use_rs2(to_integer(unsigned(B_TYPE))) <= '1';
        next_pc(to_integer(unsigned(B_TYPE))) <= pc + X"00000004";

        decode_error(to_integer(unsigned(B_TYPE))) <= '0';
        --execution_done(to_integer(unsigned(B_TYPE))) <= '1';

        case funct3 is
            when "000" => -- BEQ
                if signed(registerfile_rdata_rs1) = signed(registerfile_rdata_rs2) then
                    next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
                end if;
            when "001" => -- BNE
                if signed(registerfile_rdata_rs1) /= signed(registerfile_rdata_rs2) then
                    next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
                end if;
            when "100" => -- BLT
                if signed(registerfile_rdata_rs1) < signed(registerfile_rdata_rs2) then
                    next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
                end if;
            when "101" => -- BGE
                if signed(registerfile_rdata_rs1) >= signed(registerfile_rdata_rs2) then
                    next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
                end if;
            when "110" => -- BLTU
                if unsigned(registerfile_rdata_rs1) < unsigned(registerfile_rdata_rs2) then
                    next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
                end if;
            when "111" => -- BGEU
                if unsigned(registerfile_rdata_rs1) >= unsigned(registerfile_rdata_rs2) then
                    next_pc(to_integer(unsigned(B_TYPE))) <= pc + imm_b;
                end if;
            when others =>
                decode_error(to_integer(unsigned(B_TYPE))) <= '1';
        end case;
    end process;


    decode_r_type: process(funct3, funct7, registerfile_rdata_rs1, registerfile_rdata_rs2, pc) --clk, pc)
    begin
        use_rs1(to_integer(unsigned(R_TYPE))) <= '1'; 
        use_rs2(to_integer(unsigned(R_TYPE))) <= '1'; 
        use_rd(to_integer(unsigned(R_TYPE))) <= '1';
        next_pc(to_integer(unsigned(R_TYPE))) <= pc + X"00000004";
        decode_error(to_integer(unsigned(R_TYPE))) <= '0';
        result(to_integer(unsigned(R_TYPE))) <= (others => '0');


        imm(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs2;


        execution_done(to_integer(unsigned(R_TYPE))) <= '1';
        dec_counter(to_integer(unsigned(R_TYPE))) <= '0';

        case funct3 is
            when "000" =>
                case funct7 is
                    when "0000000" => -- ADD
                        result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 + registerfile_rdata_rs2;

                    when "0100000" => -- SUB
                        result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 - registerfile_rdata_rs2;

                    when others =>
                        decode_error(to_integer(unsigned(R_TYPE))) <= '1';
                end case;

            when "001" => -- SLL
                execution_done(to_integer(unsigned(R_TYPE))) <= '1';
                result(to_integer(unsigned(R_TYPE))) <=  DoShift(registerfile_rdata_rs1, to_integer(unsigned(registerfile_rdata_rs2(4 downto 0))), false, true);
                
            when "010" => -- SLT
                if signed(registerfile_rdata_rs1) < signed(registerfile_rdata_rs2) then
                    result(to_integer(unsigned(R_TYPE))) <= X"00000001";
                else
                    result(to_integer(unsigned(R_TYPE))) <= (others => '0');
                end if;

            when "011" => -- SLTU
                if unsigned(registerfile_rdata_rs1) < unsigned(registerfile_rdata_rs2) then
                    result(to_integer(unsigned(R_TYPE))) <= X"00000001";
                else
                    result(to_integer(unsigned(R_TYPE))) <= (others => '0');
                end if;

            when "100" => -- XOR
                result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 xor registerfile_rdata_rs2;

            when "101" =>
                execution_done(to_integer(unsigned(R_TYPE))) <= '0';

                case funct7 is
                    when "0000000" => -- SRL

                        execution_done(to_integer(unsigned(R_TYPE))) <= '1';
                        result(to_integer(unsigned(R_TYPE))) <=  DoShift(registerfile_rdata_rs1, to_integer(unsigned(registerfile_rdata_rs2(4 downto 0))), false, false);

                    when "0100000" => -- SRA
                        execution_done(to_integer(unsigned(R_TYPE))) <= '1';
                        result(to_integer(unsigned(R_TYPE))) <=  DoShift(registerfile_rdata_rs1, to_integer(unsigned(registerfile_rdata_rs2(4 downto 0))), true, false);

                    when others =>
                        decode_error(to_integer(unsigned(R_TYPE))) <= '1';
                end case;

            when "110" => -- OR
                result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 or registerfile_rdata_rs2;

            when "111" => -- AND
                result(to_integer(unsigned(R_TYPE))) <= registerfile_rdata_rs1 and registerfile_rdata_rs2;
            when others =>
                decode_error(to_integer(unsigned(R_TYPE))) <= '1';
        end case;
    end process;

    decode_i_type: process(imm_i, funct3, funct7, registerfile_rdata_rs1, pc) --clk, pc)
    begin
        imm(to_integer(unsigned(I_TYPE))) <= imm_i;

        decode_error(to_integer(unsigned(I_TYPE))) <= '0';
        execution_done(to_integer(unsigned(I_TYPE))) <= '1';
        next_pc(to_integer(unsigned(I_TYPE))) <= pc + X"00000004";

        use_rs1(to_integer(unsigned(I_TYPE))) <= '1'; 
        use_rs2(to_integer(unsigned(I_TYPE))) <= '1'; 
        use_rd(to_integer(unsigned(I_TYPE))) <= '1';

        result(to_integer(unsigned(I_TYPE))) <= (others => '0');

        dec_counter(to_integer(unsigned(I_TYPE))) <= '0';


            case funct3 is
                when "000" => -- ADDI
                    result(to_integer(unsigned(I_TYPE))) <= registerfile_rdata_rs1 + imm_i;

                when "001" =>
                    case funct7 is
                        when "0000000" => -- SLLI
                            execution_done(to_integer(unsigned(I_TYPE))) <= '1';
                            result(to_integer(unsigned(I_TYPE))) <=  DoShift(registerfile_rdata_rs1, to_integer(unsigned(imm_i(4 downto 0))), false, true);

                        when others =>
                            decode_error(to_integer(unsigned(I_TYPE))) <= '1';
                    end case;
                when "010" => -- SLTI
                    if signed(registerfile_rdata_rs1) < signed(imm_i) then
                        result(to_integer(unsigned(I_TYPE))) <= X"00000001";
                    else
                        result(to_integer(unsigned(I_TYPE))) <= (others => '0');
                    end if;

                when "011" => -- SLTIU
                    if unsigned(registerfile_rdata_rs1) < unsigned(imm_i) then
                        result(to_integer(unsigned(I_TYPE))) <= X"00000001";
                    else
                        result(to_integer(unsigned(I_TYPE))) <= (others => '0');
                    end if;

                when "100" => -- XORI
                    result(to_integer(unsigned(I_TYPE))) <= registerfile_rdata_rs1 xor imm_i;

                when "101" =>
                    execution_done(to_integer(unsigned(I_TYPE))) <= '0';

                    case funct7 is
                        when "0000000" => -- SRLI
                            execution_done(to_integer(unsigned(I_TYPE))) <= '1';
                            result(to_integer(unsigned(I_TYPE))) <=  DoShift(registerfile_rdata_rs1, to_integer(unsigned(imm_i(4 downto 0))), false, false);

                        when "0100000" => -- SRAI
                            execution_done(to_integer(unsigned(I_TYPE))) <= '1';
                            result(to_integer(unsigned(I_TYPE))) <=  DoShift(registerfile_rdata_rs1, to_integer(unsigned(imm_i(4 downto 0))), true, false);

                        when others =>
                            decode_error(to_integer(unsigned(I_TYPE))) <= '1';
                    end case;
                when "110" => -- ORI
                    result(to_integer(unsigned(I_TYPE))) <= registerfile_rdata_rs1 or imm_i;

                when "111" => -- ANDI
                    result(to_integer(unsigned(I_TYPE))) <= registerfile_rdata_rs1 and imm_i;

                when others =>
                    decode_error(to_integer(unsigned(I_TYPE))) <= '1';

            end case;
        --end if;
    end process;



    -- Immediate fields
    decode_imm: process(instruction)
    begin
        -- I-type
        imm_i(31 downto 11) <= (others => instruction(31));
        imm_i(10 downto 5) <= instruction(30 downto 25);
        imm_i(4 downto 1) <= instruction(24 downto 21);
        imm_i(0) <= instruction(20);

        -- S-type
        imm_s(31 downto 11) <= (others => instruction(31));
        imm_s(10 downto 5) <= instruction(30 downto 25);
        imm_s(4 downto 1) <= instruction(11 downto 8);
        imm_s(0) <= instruction(7);

        -- B-type
        imm_b(31 downto 12) <= (others => instruction(31));
        imm_b(11) <= instruction(7);
        imm_b(10 downto 5) <= instruction(30 downto 25);
        imm_b(4 downto 1) <= instruction(11 downto 8);
        imm_b(0) <= '0';

        -- U-type
        imm_u(31) <= instruction(31);
        imm_u(30 downto 20) <= instruction(30 downto 20);
        imm_u(19 downto 12) <= instruction(19 downto 12);
        imm_u(11 downto 0) <= (others => '0');

        -- J-type
        imm_j(31 downto 20) <= (others => instruction(31));
        imm_j(19 downto 12) <= instruction(19 downto 12);
        imm_j(11) <= instruction(20);
        imm_j(10 downto 5) <= instruction(30 downto 25);
        imm_j(4 downto 1) <= instruction(24 downto 21);
        imm_j(0) <= '0';

        -- JALR
        imm_jalr <= (others => '0');
        imm_jalr(11 downto 0) <= instruction(31 downto 20);

    end process;

    --interrupt_error <= instruction_details_array(to_integer(unsigned(opcode))).decode_error;
    --exec_done <= execution_done(to_integer(unsigned(opcode)));
end behavioural;