TYPE cache_line_t IS
RECORD
current_address : STD_LOGIC_VECTOR(31 DOWNTO 0);
fill_count : STD_LOGIC_VECTOR(13 DOWNTO 0); -- 8 kB
dirty : STD_LOGIC;
END RECORD;
PROCESS (rst, clk)
BEGIN
    CASE state IS
        WHEN S0 =>
            IF (re = '1') OR (we = '1') THEN
                IF mem_addr(i) /= current_address(i) THEN
                    reset_fill_count(i) <= '1';
                    IF dirty = '1' THEN
                        n_state <= S1;
                    ELSE
                        n_state <= S10;
                    END IF;
                END IF;
            END IF;
        WHEN S1 =>
            -- write back

        WHEN S10 => -- fill from memory
            give_up_control(i) <= '1';
            CASE mem_width(i) IS
                WHEN "00" =>
                    IF fill_count(i) >= mem_addr(i) - current_address(i) THEN
                        mem_rdy <= '1';
                    END IF;
                WHEN "01" =>
                    IF fill_count(i) - 1 >= mem_addr(i) - current_address(i) THEN
                        mem_rdy <= '1';
                    END IF;
                WHEN "10" =>
                    IF fill_count(i) - 3 > mem_addr(i) - current_address(i) THEN
                        mem_rdy <= '1';
                    END IF;
            END CASE;

    END CASE;
END PROCESS;

WITH mem_width(i) SELECT
mem_rdata <= X"000000" & blockram(mem_addr(i)(13 DOWNTO 0)) WHEN "00",
    X"0000" & blockram(mem_addr(i)(13 DOWNTO 0) + 1) & blockram(mem_addr(i)(13 DOWNTO 0)) WHEN "01",
    blockram(mem_addr(i)(13 DOWNTO 0) + 3) & blockram(mem_addr(i)(13 DOWNTO 0) + 2) & blockram(mem_addr(i)(13 DOWNTO 0) + 1) & blockram(mem_addr(i)(13 DOWNTO 0)) WHEN OTHERS;