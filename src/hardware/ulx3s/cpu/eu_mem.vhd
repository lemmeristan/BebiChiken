LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;
LIBRARY work;
USE work.bebichiken.ALL;

ENTITY eu_mem IS
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
END eu_mem;

ARCHITECTURE behavioural OF eu_mem IS
    SIGNAL Data36, Q36                                                                                                        : STD_LOGIC_VECTOR(35 DOWNTO 0);
    SIGNAL Data108, Data108_r, Q108                                                                                           : STD_LOGIC_VECTOR(107 DOWNTO 0);
    SIGNAL WrClock108, RdClock108, WrEn108, RdEn108, Empty108, Full108, WrClock36, RdClock36, WrEn36, RdEn36, Empty36, Full36 : STD_LOGIC;
    SIGNAL timestamp, n_timestamp                                                                                             : STD_LOGIC_VECTOR(47 DOWNTO 0);

    SIGNAL last_mem_addr, last_instruction, n_last_mem_addr, n_last_instruction, instruction_r, n_instruction_r, n_mem_addr, i_mem_addr, n_mem_wdata, i_mem_wdata : STD_LOGIC_VECTOR(31 DOWNTO 0);

    TYPE fsm108_state_t IS (S0, S1, S2, S3);
    SIGNAL fsm108_state, n_fsm108_state : fsm108_state_t;

    TYPE fsm36_state_t IS (S0, S1);
    SIGNAL fsm36_state, n_fsm36_state : fsm36_state_t;

    SIGNAL we_r, we_r_r, n_mem_we, n_mem_re : STD_LOGIC;


    signal rs1_data_r, rs2_data_r, imm_r, inst_r, token_r : std_logic_vector(31 downto 0);

    signal set_last_instruction, set_last_mem_addr : std_logic;
    signal same_last_instruction, same_last_mem_addr, i_same_last_instruction, i_same_last_mem_addr : std_logic;

    signal set_instruction_r, set_mem_addr, set_mem_wdata : std_logic;


BEGIN

    RdClock108 <= clk;
    RdClock36  <= clk;
    WrClock108 <= clk;
    WrClock36  <= clk;

    n_timestamp <= timestamp + X"000000000001";
    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            timestamp        <= (OTHERS => '0');
            i_mem_addr       <= (OTHERS => '0');
            i_mem_wdata <= (others => '0');
            fsm36_state      <= S0;
            fsm108_state     <= S0;
            last_instruction <= (OTHERS => '0');
            last_mem_addr    <= (OTHERS => '0');
            instruction_r    <= (OTHERS => '0');
            Data108_r        <= (OTHERS => '0');
            we_r             <= '0';
            we_r_r <= '0';

            rs1_data_r <= (others => '0');
            rs2_data_r <= (others => '0');
            inst_r <= (others => '0');
            token_r <= (others => '0');
            mem_we <= '0';
            mem_re <= '0';

            same_last_instruction <= '0';
            same_last_mem_addr <= '0';

        ELSIF rising_edge(clk) THEN
        same_last_instruction <= i_same_last_instruction;
        same_last_mem_addr <= i_same_last_mem_addr;

            mem_we <= n_mem_we;
            mem_re <= n_mem_re;
            timestamp        <= n_timestamp;

            fsm36_state      <= n_fsm36_state;
            fsm108_state     <= n_fsm108_state;

            if set_last_instruction = '1' then
                last_instruction <= Q108(31 DOWNTO 0);
            end if;

            if set_last_mem_addr = '1' then
                last_mem_addr    <= Q108(63 DOWNTO 32);
            end if;

            if set_instruction_r = '1' then
                instruction_r    <= Q108(31 DOWNTO 0);
            end if;

            if set_mem_addr = '1' then
                i_mem_addr       <= Q108(63 DOWNTO 32);
            end if;

            if set_mem_wdata = '1' then
                i_mem_wdata       <= Q108(95 DOWNTO 64);
            end if;




            if we = '1' then
                rs1_data_r <= rs1_data;
                rs2_data_r <= rs2_data;
                inst_r <= instruction;
                token_r <= token;
            end if;
            if we_r = '1' then
                Data108_r        <= Data108;
            end if;
            we_r             <= we;
            we_r_r <= we_r;
        END IF;
    END PROCESS;
    PROCESS (inst_r, rs1_data_r, rs2_data_r, token_r, timestamp, imm)
    BEGIN
        Data108               <= (OTHERS => '0');
        Data108(31 DOWNTO 0)  <= inst_r;                          -- use to extract width and rd later
        Data108(63 DOWNTO 32) <= rs1_data_r + f_decode_imm(inst_r); -- address

        IF f_decode_opcode(inst_r) = OPCODE_I_TYPE_LOAD THEN
            Data108(95 DOWNTO 64) <= token_r;
        ELSIF f_decode_opcode(inst_r) = OPCODE_S_TYPE THEN
            Data108(95 DOWNTO 64) <= rs2_data_r; -- wdata
        END IF;

        Data108(107 DOWNTO 96) <= timestamp(11 downto 0); -- use later to prioritize multiple host memory access
    END PROCESS;

    i_fifo_dc_108 : fifo_generic
    GENERIC MAP(
        vendor     => vendor,
        data_width => 108
    )
    PORT MAP(
        rst => rst,

        -- producer / CPU
        din    => Data108_r,
        wr_clk => clk,
        wr_en  => we_r_r,
        afull   => busy,

        -- consumer / FSM
        rd_clk => RdClock108,
        rd_en  => RdEn108,
        dout   => Q108,
        empty  => Empty108

    );
    i_fifo_dc_36 : fifo_generic
    GENERIC MAP(
        vendor     => vendor,
        data_width => 36
    )
    PORT MAP(
        rst => rst,

        -- producer / CPU
        din    => Data36,
        wr_clk => WrClock36,
        wr_en  => WrEn36,
        full   => Full36,

        -- consumer / FSM
        rd_clk => RdClock36,
        rd_en  => RdEn36,
        dout   => Q36,
        empty  => Empty36
    );
    


    i_same_last_instruction <= '1' when last_instruction /= Q108(31 DOWNTO 0) else '0';
    i_same_last_mem_addr <= '1' when last_mem_addr /= Q108(63 DOWNTO 32) else '0';

    ----------------------------------------------
    -- Split into instructions
    ----------------------------------------------

    fsm108 : PROCESS (fsm108_state, Empty108, Full36, same_last_instruction, Q108, same_last_mem_addr)
    BEGIN
        n_fsm108_state     <= fsm108_state;
        RdEn108            <= '0';
        WrEn36             <= '0';
        n_last_instruction <= last_instruction;
        n_last_mem_addr    <= last_mem_addr;
        Data36             <= (OTHERS => '0');
        set_last_instruction <= '0';
        set_last_mem_addr <= '0';
        n_mem_re <= '0';

        set_instruction_r <= '0';
        set_mem_addr <= '0';
        set_mem_wdata <= '0';

        n_mem_we <= '0';
        n_mem_re <= '0';

        CASE fsm108_state IS
            WHEN S0 =>
                IF Empty108 = '0' THEN
                    RdEn108        <= '1';
                    n_fsm108_state <= S1;
                END IF;
            WHEN S1 =>
                set_instruction_r <= '1';
                set_mem_addr <= '1';
                set_mem_wdata <= '1';
                n_fsm108_state     <= S2;

            WHEN S2 =>
                    IF f_decode_opcode(instruction_r) = OPCODE_I_TYPE_LOAD THEN
                        n_mem_re <= '1';
                        if mem_rdy = '1' then
                            n_fsm108_state <= S0;
                        end if;
                    ELSIF f_decode_opcode(instruction_r) = OPCODE_S_TYPE THEN
                        n_mem_we <= '1';
                        if mem_wack = '1' then
                            n_fsm108_state <= S0;
                        end if;
                    ELSE                                   -- invalid instruction
                        n_fsm108_state <= S0;
                    END IF;

            when others =>
                n_fsm108_state <= S0;
        END CASE;
    END PROCESS;

    writeback_data  <= mem_rdata; -- perhaps edit this later depending on width
    writeback_token <= i_mem_wdata;
    mem_addr    <= i_mem_addr;
    mem_wdata <= i_mem_wdata;
    mem_width       <= instruction_r(13 DOWNTO 12);
    writeback_rd    <= instruction_r(11 DOWNTO 7);
    

END behavioural;