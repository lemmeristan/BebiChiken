LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

LIBRARY work;
USE work.bebichiken.ALL;

ENTITY registerfile IS
    GENERIC (
        entry_point : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00000000";
        vendor      : STD_LOGIC                     := '1'
    );
    PORT (
        rst, clk          : IN STD_LOGIC;
        rs1, rs2, rd      : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        lock_rd           : IN STD_LOGIC;
        new_rd_lock_owner : IN opcode_group_t;
        lock_token        : IN STD_LOGIC_VECTOR(31 DOWNTO 0);

        writeback_we    : IN opcode_group_bit_t;
        writeback_data  : IN opcode_group_word_t;
        writeback_token : IN opcode_group_word_t;
        writeback_rd    : IN opcode_group_regidx_t;

        rs1_data_out, rs2_data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        rs1_locked, rs2_locked     : OUT STD_LOGIC

    );
END registerfile;

ARCHITECTURE behavioural OF registerfile IS

    SIGNAL owner : lock_owner_t(31 DOWNTO 0);
    TYPE registers_t IS ARRAY (31 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0);

    TYPE registers_of_eu_t IS ARRAY(opcode_group_t) OF registers_t;

    -- Registers
    ATTRIBUTE syn_ramstyle          : STRING;
    ATTRIBUTE syn_ramstyle OF owner : SIGNAL IS "rw_check";

    SIGNAL rs1_data_out_of_op, rs2_data_out_of_op, rs1_token_of_op, rs2_token_of_op, token_of_register_rs1, token_of_register_rs2 : opcode_group_word_t;



BEGIN


    gen_dpram : FOR op IN opcode_group_t GENERATE

        lattice : IF vendor = '1' GENERATE
            inst_dpram_rs1 : dpram_regfile_lattice
            PORT MAP(
                ClockA   => clk,
                WrA => '0',
                AddressA  => rs1,
                DataInA   => X"00000000",
                QA  => rs1_data_out_of_op(op),
                ClockB   => clk,
                WrB => writeback_we(op),
                AddressB  => writeback_rd(op),
                DataInB   => writeback_data(op),
                QB  => OPEN, ResetA => '0', ResetB => '0', ClockEnA => '1', ClockEnB => '1'
            );

            inst_dpram_rs2 : dpram_regfile_lattice
            PORT MAP(
                ClockA   => clk,
                WrA => '0',
                AddressA  => rs2,
                DataInA   => X"00000000",
                QA  => rs2_data_out_of_op(op),
                ClockB   => clk,
                WrB => writeback_we(op),
                AddressB  => writeback_rd(op),
                DataInB   => writeback_data(op),
                QB  => OPEN, ResetA => '0', ResetB => '0', ClockEnA => '1', ClockEnB => '1'
            );

            inst_dpram_token_rs1_is : dpram_regfile_lattice
            PORT MAP(
                ClockA   => clk,
                WrA => '0',
                AddressA  => rs1,
                DataInA   => rs1_token_of_op(op),
                QA  => rs1_token_of_op(op),
                ClockB   => clk,
                WrB => writeback_we(op),
                AddressB  => writeback_rd(op),
                DataInB   => writeback_token(op),
                QB  => OPEN, ResetA => '0', ResetB => '0', ClockEnA => '1', ClockEnB => '1'
            );

            inst_dpram_token_rs2_is : dpram_regfile_lattice
            PORT MAP(
                ClockA   => clk,
                WrA => '0',
                AddressA  => rs2,
                DataInA   => rs2_token_of_op(op),
                QA  => rs2_token_of_op(op),
                ClockB   => clk,
                WrB => writeback_we(op),
                AddressB  => writeback_rd(op),
                DataInB   => writeback_token(op),
                QB  => OPEN, ResetA => '0', ResetB => '0', ClockEnA => '1', ClockEnB => '1'
            );

            inst_dpram_token_rs1_expect : dpram_regfile_lattice
            PORT MAP(
                ClockA   => clk,
                WrA => '0',
                AddressA  => rs1,
                DataInA   => token_of_register_rs1(op),
                QA  => token_of_register_rs1(op),
                ClockB   => clk,
                WrB => lock_rd,
                AddressB  => rd,
                DataInB   => lock_token,
                QB  => OPEN, ResetA => '0', ResetB => '0', ClockEnA => '1', ClockEnB => '1'
            );

            inst_dpram_token_rs2_expect : dpram_regfile_lattice
            PORT MAP(
                ClockA   => clk,
                WrA => '0',
                AddressA  => rs2,
                DataInA   => token_of_register_rs2(op),
                QA  => token_of_register_rs2(op),
                ClockB   => clk,
                WrB => lock_rd,
                AddressB  => rd,
                DataInB   => lock_token,
                QB  => OPEN, ResetA => '0', ResetB => '0', ClockEnA => '1', ClockEnB => '1'
            );

        END GENERATE lattice;


        xilinx : IF vendor = '0' GENERATE
            inst_dpram_rs1 : dpram_regfile_xilinx
            PORT MAP(
                clka   => clk,
                wea(0) => '0',
                addra  => rs1,
                dina   => X"00000000",
                douta  => rs1_data_out_of_op(op),
                clkb   => clk,
                web(0) => writeback_we(op),
                addrb  => writeback_rd(op),
                dinb   => writeback_data(op),
                doutb  => OPEN
            );

            inst_dpram_rs2 : dpram_regfile_xilinx
            PORT MAP(
                clka   => clk,
                wea(0) => '0',
                addra  => rs2,
                dina   => X"00000000",
                douta  => rs2_data_out_of_op(op),
                clkb   => clk,
                web(0) => writeback_we(op),
                addrb  => writeback_rd(op),
                dinb   => writeback_data(op),
                doutb  => OPEN
            );

            inst_dpram_token_rs1_is : dpram_regfile_xilinx
            PORT MAP(
                clka   => clk,
                wea(0) => '0',
                addra  => rs1,
                dina   => rs1_token_of_op(op),
                douta  => rs1_token_of_op(op),
                clkb   => clk,
                web(0) => writeback_we(op),
                addrb  => writeback_rd(op),
                dinb   => writeback_token(op),
                doutb  => OPEN
            );

            inst_dpram_token_rs2_is : dpram_regfile_xilinx
            PORT MAP(
                clka   => clk,
                wea(0) => '0',
                addra  => rs2,
                dina   => rs2_token_of_op(op),
                douta  => rs2_token_of_op(op),
                clkb   => clk,
                web(0) => writeback_we(op),
                addrb  => writeback_rd(op),
                dinb   => writeback_token(op),
                doutb  => OPEN
            );

            inst_dpram_token_rs1_expect : dpram_regfile_xilinx
            PORT MAP(
                clka   => clk,
                wea(0) => '0',
                addra  => rs1,
                dina   => token_of_register_rs1(op),
                douta  => token_of_register_rs1(op),
                clkb   => clk,
                web(0) => lock_rd,
                addrb  => rd,
                dinb   => lock_token,
                doutb  => OPEN
            );

            inst_dpram_token_rs2_expect : dpram_regfile_xilinx
            PORT MAP(
                clka   => clk,
                wea(0) => '0',
                addra  => rs2,
                dina   => token_of_register_rs2(op),
                douta  => token_of_register_rs2(op),
                clkb   => clk,
                web(0) => lock_rd,
                addrb  => rd,
                dinb   => lock_token,
                doutb  => OPEN
            );

        END GENERATE xilinx;

    END GENERATE gen_dpram;

    rs1_locked <= '0' WHEN (rs1_token_of_op(owner(to_integer(unsigned(rs1)))) = token_of_register_rs1(owner(to_integer(unsigned(rs1))))) ELSE
        '1'; -- OR (owner(to_integer(unsigned(rs1))) = OPCODE_INVALID) ELSE '1';
    rs2_locked <= '0' WHEN (rs2_token_of_op(owner(to_integer(unsigned(rs2)))) = token_of_register_rs2(owner(to_integer(unsigned(rs2))))) ELSE
        '1'; --OR (owner(to_integer(unsigned(rs2))) = OPCODE_INVALID) ELSE '1';

    rs1_data_out <= rs1_data_out_of_op(owner(to_integer(unsigned(rs1))));-- WHEN rs1 /= "00000" ELSE (OTHERS => '0');
    rs2_data_out <= rs2_data_out_of_op(owner(to_integer(unsigned(rs2))));-- WHEN rs2 /= "00000" ELSE (OTHERS => '0');

    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            owner             <= (OTHERS => OPCODE_INVALID);
        ELSIF rising_edge(clk) THEN

            IF (lock_rd = '1') THEN
                owner(to_integer(unsigned(rd)))             <= new_rd_lock_owner;
            END IF;

        END IF;
    END PROCESS;

END behavioural;