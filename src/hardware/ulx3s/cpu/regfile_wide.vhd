LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

LIBRARY work;
USE work.bebichiken.ALL;

ENTITY regfile_wide IS
    GENERIC(
        entry_point : std_logic_vector(31 downto 0) := X"00000000"
    );
    PORT (
        rst, clk : IN STD_LOGIC;
        rs1, rs2, rd : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        lock_rd, lock_pc : in std_logic;
        new_rd_lock_owner, new_pc_lock_owner : in opcode_group_t;

        writeback_we : in opcode_group_bit_t;
        writeback_data : in opcode_group_word_t;
        writeback_rd : in opcode_group_regidx_t;
        writeback_pc : in opcode_group_word_t;
        writeback_update_pc : in opcode_group_bit_t;


        rs1_data_out, rs2_data_out, pc : out std_logic_vector(31 downto 0);
        rs1_locked, rs2_locked, pc_locked : out std_logic

    );
END regfile_wide;

ARCHITECTURE behavioural OF regfile_wide IS
    -- Registers

    

    signal locks : lock_owner_t(32 downto 0);
    TYPE registers_t IS ARRAY (32 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL registers : registers_t; -- := (OTHERS => (OTHERS => '0'));

    --    ATTRIBUTE syn_ramstyle : STRING;
    --    ATTRIBUTE syn_ramstyle OF registers : SIGNAL IS "rw_check";
    --attribute noprune: boolean; attribute noprune of data_in_r: signal is true;

BEGIN
    pc <= registers(32);

    pc_locked <= '0' when locks(32) = OPCODE_INVALID else '1';
    rs1_locked <= '0' when locks(to_integer(unsigned(rs1))) = OPCODE_INVALID else '1';
    rs2_locked <= '0' when locks(to_integer(unsigned(rs2))) = OPCODE_INVALID else '1';
    rs1_data_out <= registers(to_integer(unsigned(rs1))) WHEN rs1 /= "00000" ELSE
        (OTHERS => '0');
    rs2_data_out <= registers(to_integer(unsigned(rs2))) WHEN rs2 /= "00000" ELSE
        (OTHERS => '0');



    process(rst, clk)
    begin
        if rst = '1' then
            locks <= (others => OPCODE_INVALID);
            registers <= (32 => entry_point, others => (others => '0'));
            --registers(32) <= entry_point;
        elsif rising_edge(clk) then
        --     for i in 0 to 31 loop
        --         if locks(i) = OPCODE_INVALID then
        --             if lock_rd = '1' then
        --                 locks(i) <= new_rd_lock_owner;
        --             end if;
        --         elsif writeback_we(locks(i)) = '1' then
        --             registers(i) <= writeback_data(locks(i));
        --             locks(i) <= OPCODE_INVALID;
        --         end if;
        --     end loop;
            
            
        --    if writeback_update_pc(locks(32)) = '1' then
        --         registers(32) <= writeback_pc(locks(32));
        --         locks(32) <= OPCODE_INVALID;
        --     end if;



        for op in opcode_group_t loop
            if op = OPCODE_INVALID then
                if lock_pc = '1' then
                    locks(32) <= new_pc_lock_owner;
                end if;

                if lock_rd = '1' then
                    locks(to_integer(unsigned(rd))) <= new_rd_lock_owner;
                end if;
            else
                if writeback_we(op) = '1' then
                    if locks(to_integer(unsigned(writeback_rd(op)))) = op then
                        locks(to_integer(unsigned(writeback_rd(op)))) <= OPCODE_INVALID;
                        registers(to_integer(unsigned(writeback_rd(op)))) <= writeback_data(op);
                    end if;
                end if;
                
                if writeback_update_pc(op) = '1' then
                    if locks(32) = op then
                        locks(32) <= OPCODE_INVALID;
                        registers(32) <= writeback_pc(op);
                    end if;
                end if;
            end if;

        end loop;
            
                


        end if;
    end process;



END behavioural;