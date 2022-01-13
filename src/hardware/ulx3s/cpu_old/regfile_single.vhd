LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY regfile_single IS
    PORT (
        clk, rst                   : IN STD_LOGIC;
        rs1, rs2                   : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        rs1_data_out, rs2_data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        rd                         : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        update_rd                  : IN STD_LOGIC;
        rd_data_in                 : IN STD_LOGIC_VECTOR(31 DOWNTO 0)

    );
END regfile_single;

ARCHITECTURE behavioural OF regfile_single IS
    -- Registers
    --    TYPE registers_t IS ARRAY (31 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
    --    SIGNAL registers : registers_t := (OTHERS => (OTHERS => '0'));

    SIGNAL registers1, registers2 : STD_LOGIC_VECTOR(1023 DOWNTO 0) := (OTHERS => '0');

    -- ATTRIBUTE syn_ramstyle : STRING;
    -- ATTRIBUTE syn_ramstyle OF registers : SIGNAL IS "rw_check";
    --attribute noprune: boolean; attribute noprune of data_in_r: signal is true;
    FUNCTION DoShift (
        value : STD_LOGIC_VECTOR(1023 DOWNTO 0);
        shamt : STD_LOGIC_VECTOR(4 DOWNTO 0)
    ) RETURN STD_LOGIC_VECTOR IS
        VARIABLE ires, padding : STD_LOGIC_VECTOR(1023 DOWNTO 0);
    BEGIN
        padding := (others => '0');
        ires := value;

        IF (shamt AND "00001") /= "00000" THEN
            ires := padding(31 downto 0) & ires(1023 DOWNTO 32);
        END IF;

        IF (shamt AND "00010") /= "00000" THEN
            ires := padding(63 downto 0) & ires(1023 downto 64);
        END IF;
        IF (shamt AND "00100") /= "00000" THEN
            ires := padding(127 downto 0) & ires(1023 downto 128);
        END IF;
        IF (shamt AND "01000") /= "00000" THEN
            ires := padding(255 downto 0) & ires(1023 downto 256);
        END IF;

        IF (shamt AND "10000") /= "00000" THEN
            ires := padding(511 downto 0) & ires(1023 downto 512);
        END IF;

        RETURN ires;
    END FUNCTION;

    signal we, we_shifted : std_logic_vector(1023 downto 0);

BEGIN

    PROCESS (rst, clk)
    BEGIN
        IF rst = '1' THEN
            registers1 <= (OTHERS => '0');
            registers2 <= (OTHERS => '0');

        ELSIF rising_edge(clk) THEN
            IF update_rd = '1' THEN
                -- for i in 0 to 1023 loop
                --     if we_shifted(i) = '1' then
                --         registers(i) <= rd_data_in(i mod 32);
                --     end if;
                -- end loop;

                registers1(((to_integer(unsigned(rd))+1)*32)-1 downto to_integer(unsigned(rd))*32) <= rd_data_in;
                registers2(((to_integer(unsigned(rd))+1)*32)-1 downto to_integer(unsigned(rd))*32) <= rd_data_in;

            END IF;

            
        END IF;
    END PROCESS;

    we <= (others => '1');

    we_shifted <= DoShift(we, rd);

    rs1_data_out <= DoShift(registers1, rs1)(31 downto 0) when rs1 /= "00000" else (others => '0');
    rs2_data_out <=  DoShift(registers2, rs2)(31 downto 0) when rs2 /= "00000" else (others => '0');

END behavioural;