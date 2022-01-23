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
    SIGNAL Data144, Data144_r, Q144                                                                                           : STD_LOGIC_VECTOR(143 DOWNTO 0);
    SIGNAL WrClock144, RdClock144, WrEn144, RdEn144, Empty144, Full144, WrClock36, RdClock36, WrEn36, RdEn36, Empty36, Full36 : STD_LOGIC;
    SIGNAL timestamp, n_timestamp                                                                                             : STD_LOGIC_VECTOR(47 DOWNTO 0);

    SIGNAL last_mem_addr, last_instruction, n_last_mem_addr, n_last_instruction, instruction_r, n_instruction_r, n_mem_addr, i_mem_addr : STD_LOGIC_VECTOR(31 DOWNTO 0);

    TYPE fsm144_state_t IS (S0, S1, S2, S3);
    SIGNAL fsm144_state, n_fsm144_state : fsm144_state_t;

    TYPE fsm36_state_t IS (S0, S1);
    SIGNAL fsm36_state, n_fsm36_state : fsm36_state_t;

    SIGNAL we_r : STD_LOGIC;
BEGIN

    RdClock144 <= clk;
    RdClock36  <= clk;
    WrClock144 <= clk;
    WrClock36  <= clk;

    mem_addr    <= i_mem_addr;
    n_timestamp <= timestamp + X"000000000001";
    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            timestamp        <= (OTHERS => '0');
            i_mem_addr       <= (OTHERS => '0');
            fsm36_state      <= S0;
            fsm144_state     <= S0;
            last_instruction <= (OTHERS => '0');
            last_mem_addr    <= (OTHERS => '0');
            instruction_r    <= (OTHERS => '0');
            Data144_r        <= (OTHERS => '0');
            we_r             <= '0';
        ELSIF rising_edge(clk) THEN
            timestamp        <= n_timestamp;
            i_mem_addr       <= n_mem_addr;
            fsm36_state      <= n_fsm36_state;
            fsm144_state     <= n_fsm144_state;
            last_instruction <= n_last_instruction;
            last_mem_addr    <= n_last_mem_addr;
            instruction_r    <= n_instruction_r;
            Data144_r        <= Data144;
            we_r             <= we;
        END IF;
    END PROCESS;
    PROCESS (instruction, rs1_data, rs2_data, token, timestamp)
    BEGIN
        Data144               <= (OTHERS => '0');
        Data144(31 DOWNTO 0)  <= instruction;                          -- use to extract width and rd later
        Data144(63 DOWNTO 32) <= rs1_data + f_decode_imm(instruction); -- address

        IF f_decode_opcode(instruction) = OPCODE_I_TYPE_LOAD THEN
            Data144(95 DOWNTO 64) <= token;
        ELSIF f_decode_opcode(instruction) = OPCODE_S_TYPE THEN
            Data144(95 DOWNTO 64) <= rs2_data; -- wdata
        END IF;

        Data144(143 DOWNTO 96) <= timestamp; -- use later to prioritize multiple host memory access
    END PROCESS;
    lattice : IF vendor = '1' GENERATE

        i_fifo_dc_144_lattice : fifo_dc_144_lattice PORT MAP(
            Reset   => '0',
            RPReset => '0',

            -- producer / CPU
            Data    => Data144,
            WrClock => clk,
            WrEn    => we,
            Full    => busy,

            -- consumer / FSM
            RdClock => RdClock144,
            RdEn    => RdEn144,
            Q       => Q144,
            Empty   => Empty144
        );

        i_fifo_dc_36_lattice : fifo_dc_36_lattice PORT MAP(
            Reset   => '0',
            RPReset => '0',

            -- producer / CPU
            Data    => Data36,
            WrClock => WrClock36,
            WrEn    => WrEn36,
            Full    => Full36,

            -- consumer / FSM
            RdClock => RdClock36,
            RdEn    => RdEn36,
            Q       => Q36,
            Empty   => Empty36
        );

    END GENERATE lattice;
    xilinx : IF vendor = '0' GENERATE

        i_fifo_dc_144_xilinx : fifo_dc_144_xilinx PORT MAP(
            rst => '0',

            -- producer / CPU
            din    => Data144_r,
            wr_clk => clk,
            wr_en  => we_r,
            full   => busy,

            -- consumer / FSM
            rd_clk => RdClock144,
            rd_en  => RdEn144,
            dout   => Q144,
            empty  => Empty144

        );
        i_fifo_dc_36_xilinx : fifo_dc_36_xilinx PORT MAP(
            rst => '0',

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

    END GENERATE xilinx;

    ----------------------------------------------
    -- Split into instructions
    ----------------------------------------------

    fsm144 : PROCESS (fsm144_state, Empty144, Full36, last_instruction, Q144, last_mem_addr)
    BEGIN
        n_fsm144_state     <= fsm144_state;
        RdEn144            <= '0';
        WrEn36             <= '0';
        n_last_instruction <= last_instruction;
        n_last_mem_addr    <= last_mem_addr;
        Data36             <= (OTHERS => '0');

        CASE fsm144_state IS
            WHEN S0 =>
                IF Empty144 = '0' THEN
                    RdEn144        <= '1';
                    n_fsm144_state <= S1;
                END IF;
            WHEN S1 =>                            -- remember to check whether or not fifo is full
                Data36 <= "0001" & Q144(31 DOWNTO 0); -- set instruction
                IF ((last_instruction /= Q144(31 DOWNTO 0)) AND (Full36 = '0')) OR (last_instruction = Q144(31 DOWNTO 0)) THEN
                    IF (last_instruction /= Q144(31 DOWNTO 0)) THEN
                        WrEn36 <= '1';
                    END IF;
                    n_last_instruction <= Q144(31 DOWNTO 0);
                    n_fsm144_state     <= S2;
                END IF;
            WHEN S2 =>
                Data36 <= "0010" & Q144(63 DOWNTO 32); -- set mem_addr
                IF ((last_mem_addr /= Q144(63 DOWNTO 32)) AND (Full36 = '0')) OR (last_mem_addr = Q144(63 DOWNTO 32)) THEN
                    IF (last_mem_addr /= Q144(63 DOWNTO 32)) THEN
                        WrEn36 <= '1';
                    END IF;
                    n_last_mem_addr <= Q144(63 DOWNTO 32);
                    n_fsm144_state  <= S3;
                END IF;
            WHEN S3 =>
                IF Full36 = '0' THEN
                    WrEn36 <= '1';
                    IF f_decode_opcode(Q144(31 DOWNTO 0)) = OPCODE_I_TYPE_LOAD THEN
                        Data36 <= "0100" & Q144(95 DOWNTO 64); -- read from bus, token as parameter
                    ELSIF f_decode_opcode(Q144(31 DOWNTO 0)) = OPCODE_S_TYPE THEN
                        Data36 <= "1000" & Q144(95 DOWNTO 64); -- write to bus, wdata as parameter
                    ELSE                                   -- invalid instruction
                        WrEn36 <= '0';
                    END IF;
                    n_fsm144_state <= S0;
                END IF;
        END CASE;
    END PROCESS;

    writeback_data  <= mem_rdata; -- perhaps edit this later depending on width
    writeback_token <= Q36(31 DOWNTO 0);
    mem_wdata       <= Q36(31 DOWNTO 0);
    mem_width       <= instruction_r(13 DOWNTO 12);
    writeback_rd    <= instruction_r(11 DOWNTO 7);
    ----------------------------------------------
    -- Execute instructions
    ----------------------------------------------
    fsm36 : PROCESS (fsm36_state, mem_rdy, mem_wack, Empty36, instruction_r, Q36, i_mem_addr)
    BEGIN
        n_instruction_r <= instruction_r;
        RdEn36          <= '0';
        n_fsm36_state   <= fsm36_state;
        n_mem_addr      <= i_mem_addr;
        mem_re          <= '0';
        mem_we          <= '0';
        writeback_we    <= '0';

        CASE fsm36_state IS
            WHEN S0 =>
                IF Empty36 = '0' THEN
                    RdEn36        <= '1';
                    n_fsm36_state <= S1;
                END IF;
            WHEN S1 =>
                CASE Q36(35 DOWNTO 32) IS
                    WHEN "0001" => -- set instruction
                        n_instruction_r <= Q36(31 DOWNTO 0);
                        IF Empty36 = '0' THEN
                            RdEn36 <= '1';
                        ELSE
                            n_fsm36_state <= S0;
                        END IF;
                    WHEN "0010" => -- set address
                        n_mem_addr <= Q36(31 DOWNTO 0);

                        IF Empty36 = '0' THEN
                            RdEn36 <= '1';
                        ELSE
                            n_fsm36_state <= S0;
                        END IF;
                    WHEN "0100" => -- read from bus
                        mem_re <= '1';
                        IF mem_rdy = '1' THEN
                            writeback_we  <= '1';
                            n_fsm36_state <= S0;
                        END IF;
                    WHEN "1000" => -- write to bus
                        mem_we <= '1';
                        IF mem_wack = '1' THEN
                            n_fsm36_state <= S0;
                        END IF;
                    WHEN OTHERS =>
                        IF Empty36 = '0' THEN
                            RdEn36 <= '1';
                        ELSE
                            n_fsm36_state <= S0;
                        END IF;
                END CASE;
        END CASE;

    END PROCESS;

END behavioural;