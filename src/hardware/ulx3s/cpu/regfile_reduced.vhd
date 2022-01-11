LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

LIBRARY work;
USE work.bebichiken.ALL;

ENTITY regfile_reduced IS
    GENERIC(
        entry_point : std_logic_vector(31 downto 0) := X"00000000"
    );
    PORT (
        rst, clk : IN STD_LOGIC;
        rs1, rs2, rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        lock_rd, lock_pc : in std_logic;
        new_rd_lock_owner, new_pc_lock_owner : in opcode_group_t;
        token_for_rd, token_for_pc : in std_logic_vector(31 downto 0);

        writeback_we, writeback_update_pc : in opcode_group_bit_t;
        writeback_data : in opcode_group_word_t;
        writeback_next_pc, writeback_token : in opcode_group_word_t;
        writeback_rd : in opcode_group_regidx_t;

        rs1_data_out, rs2_data_out, pc : out std_logic_vector(31 downto 0);
        rs1_locked, rs2_locked, pc_locked : out std_logic

    );
END regfile_reduced;

ARCHITECTURE behavioural OF regfile_reduced IS

signal owner : lock_owner_t(31 downto 0);
TYPE registers_t IS ARRAY (31 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0);

signal pc_owner : opcode_group_t;


TYPE registers_of_eu_t is ARRAY(opcode_group_t) OF registers_t;
SIGNAL registers, tokens : registers_of_eu_t; -- := (OTHERS => (OTHERS => '0'));
signal token_of_register : registers_t;

signal token_of_pc : std_logic_vector(31 downto 0);

signal i_rs1, i_rs2, i_rd : integer range 0 to 31;

    -- Registers
        ATTRIBUTE syn_ramstyle : STRING;
        ATTRIBUTE syn_ramstyle OF registers, owner : SIGNAL IS "rw_check";
    
signal rs1_data_out_of_op, rs2_data_out_of_op, r_pc, rs1_token_of_op, rs2_token_of_op : opcode_group_word_t;

signal owner_for_rs1, owner_for_rs2 : opcode_group_t;


TYPE rd_owner_t IS ARRAY(opcode_group_t) OF opcode_group_t;
signal rd_owner_of_op : rd_owner_t;

BEGIN

i_rs1 <= to_integer(unsigned(rs1));
i_rs2 <= to_integer(unsigned(rs2));
i_rd <= to_integer(unsigned(rd));


    pc <= r_pc(pc_owner);

    pc_locked <= '0' when pc_owner = OPCODE_INVALID else '1';
    rs1_locked <= '0' when rs1_token_of_op(owner_for_rs1) = token_of_register(i_rs1) else '1';
    rs2_locked <= '0' when rs2_token_of_op(owner_for_rs2) = token_of_register(i_rs2) else '1';
    rs1_data_out <= rs1_data_out_of_op(owner_for_rs1) WHEN rs1 /= "00000" ELSE (OTHERS => '0');
    rs2_data_out <= rs2_data_out_of_op(owner_for_rs2) WHEN rs2 /= "00000" ELSE (OTHERS => '0');

    
    owner_for_rs1 <= owner(i_rs1);
    owner_for_rs2 <= owner(i_rs2);



    process(owner, writeback_rd)
    begin
        for x in opcode_group_t loop
            rd_owner_of_op(x) <= owner(to_integer(unsigned(writeback_rd(x))));


            rs1_data_out_of_op(x) <= registers(x)(i_rs1);
            rs2_data_out_of_op(x) <= registers(x)(i_rs2);

            rs1_token_of_op(x) <= tokens(x)(i_rs1);
            rs2_token_of_op(x) <= tokens(x)(i_rs2);


        end loop;
    end process;



    process(rst, clk)
    begin
        if rst = '1' then
            owner <= (others => OPCODE_INVALID);
            pc_owner <= OPCODE_INVALID;
            r_pc <= (others => entry_point);


        elsif rising_edge(clk) then



            if (lock_rd = '1') and (owner(i_rd) = OPCODE_INVALID) then
                owner(i_rd) <= new_rd_lock_owner;
                token_of_register(i_rd) <= token_for_rd;
            end if;

            
            if (lock_pc = '1') and (pc_owner = OPCODE_INVALID) then
                pc_owner <= new_pc_lock_owner;
                token_of_pc <= token_for_pc;
            end if;

            
            for x in opcode_group_t loop
                if writeback_we(x) = '1' then
                    registers(x)(to_integer(unsigned(writeback_rd(x)))) <= writeback_data(x);
                    tokens(x)(to_integer(unsigned(writeback_rd(x)))) <= writeback_token(x);

                    --if rd_owner_of_op(x) = x then
                    --    owner(to_integer(unsigned(writeback_rd(x)))) <= OPCODE_INVALID;
                    --end if;
                end if;

                if writeback_update_pc(x) = '1' then

                    r_pc(x) <= writeback_next_pc(x);

                    
                    if pc_owner = x then
                        pc_owner <= OPCODE_INVALID;
                    end if;
                end if;



                

                
    

            end loop;
            
                


        end if;
    end process;



END behavioural;