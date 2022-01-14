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
        rs1_data, rs2_data, instruction, token, imm : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_data, writeback_token : out std_logic_vector(31 downto 0);
        writeback_we  : out std_logic;
        writeback_rd                       : out std_logic_vector(4 downto 0);

        mem_we, mem_re : out std_logic;
        mem_wack, mem_rdy : in std_logic;
        mem_wdata, mem_addr : out std_logic_vector(31 downto 0);
        mem_rdata : in std_logic_vector(31 downto 0);
        mem_width : out std_logic_vector(1 downto 0);

        busy : out std_logic 

    );
END eu_mem;

ARCHITECTURE behavioural OF eu_mem IS

    SIGNAL r_rs1_data, r_rs2_data, r_instruction, r_token, r_imm : STD_LOGIC_VECTOR(31 DOWNTO 0);

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
            r_token <= (others => '0');
            r_imm <= (others => '0');
            state <= S_IDLE;
        elsIF rising_edge(clk) THEN


            state <= n_state;

            IF we = '1' THEN
                r_rs1_data    <= rs1_data;
                r_rs2_data    <= rs2_data;
                r_instruction <= instruction;
                r_token <= token;
                r_imm <= imm;

            END IF;
        END IF;
    END PROCESS;


    -- not formally correct, still need to account for funct3(2), i.e.: r_instruction(14), and sign extension
    -- preferrably not even feeding execution unit if instruction is invalid

    op <= f_decode_opcode(r_instruction);
    writeback_token   <= r_token;
    writeback_rd <= r_instruction(11 DOWNTO 7);


    mem_addr <= r_rs1_data + r_imm;
    mem_wdata <= r_rs2_data;
    mem_width <= r_instruction(13 downto 12);
    writeback_data <= mem_rdata;

    process(state, we, state, op, mem_rdy, mem_wack)
    begin
        n_state <= state;
        busy <= '0';
        mem_re <= '0';
        mem_we <= '0';
        writeback_we <= '0';
        case state is
            when S_IDLE =>
                if we = '1' then
                    n_state <= S_BUSY;
                end if;
            when S_BUSY =>
                busy <= '1';
                case op is
                    when OPCODE_I_TYPE_LOAD =>
                    mem_re <= '1';
                    if mem_rdy = '1' then
                        writeback_we <= '1';
                        n_state <= S_IDLE;
                    end if;

                when OPCODE_S_TYPE =>
                    mem_we <= '1';
                    if mem_wack = '1' then
                        n_state <= S_IDLE;
                    end if;
                when others =>
                    n_state <= S_IDLE;
                end case;
        end case;
    end process;

                    

END behavioural;