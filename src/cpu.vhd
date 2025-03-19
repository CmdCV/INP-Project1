-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Aleš Urbánek <xurbana00 AT stud.fit.vutbr.cz>
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
  -- Counter register (CNT)
  signal cnt_reg : std_logic_vector(12 downto 0) := (others => '0');
  signal cnt_inc : std_logic := '0'; 
  signal cnt_dec : std_logic := '0';

  -- Temporary register (TMP)
  signal tmp_reg : std_logic_vector(7 downto 0) := (others => '0');
  signal tmp_id : std_logic := '0';

  -- Program pointer (PTR) 
  signal ptr_reg : std_logic_vector(12 downto 0) := (others => '0');
  signal ptr_inc : std_logic := '0';
  signal ptr_dec : std_logic := '0';
  signal ptr_rst : std_logic := '0';

  -- Program counter (PC)
  signal pc_reg : std_logic_vector(12 downto 0) := (others => '0');
  signal pc_inc : std_logic := '0';
  signal pc_dec : std_logic := '0';

  -- Multiplexors (MUX) for selecting data sources
  signal mux1_sel : std_logic := '1';
  signal mux2_sel : std_logic_vector (1 downto 0) := "00";

  -- Finite State Machine (FSM) states
  type fsm_states is (
    state_init,
    state_ready,
    state_fetch,
    state_decode,
    state_write_ptr_inc,
    state_write_ptr_dec,
    state_while_start,
    state_while_cnt,
    state_while_zero,
    state_while_read,
    state_while_end_cnt,
    state_while_end_zero,
    state_while_end_read,
    state_write_tmp,
    state_read_tmp,
    state_write_char
  );

  -- FSM setup
  signal current_state : fsm_states := state_ready;
  signal next_state : fsm_states;

begin

  -- Ridici proces pro CNT
  cnt_controller: process(CLK, RESET) begin
    if (RESET = '1') then cnt_reg <= (others => '0');
    elsif rising_edge(CLK) then
      if (cnt_inc = '1') then cnt_reg <= cnt_reg + 1;
      elsif (cnt_dec = '1') then cnt_reg <= cnt_reg - 1;
      end if;
    end if;
  end process;
  
  -- Ridici proces pro TMP
  tmp_controller: process (CLK, RESET) begin
    if (RESET = '1') then tmp_reg <= (others => '0');
    elsif rising_edge(CLK) then
      if (tmp_id = '1') then tmp_reg <= DATA_RDATA; 
      end if;
    end if;
  end process;

  -- Ridici proces pro PTR
  ptr_controller: process (CLK, RESET) begin
    if (RESET = '1') then ptr_reg <= (others => '0');
    elsif rising_edge(CLK) then
      if (ptr_inc = '1') then ptr_reg <= ptr_reg + 1;
      elsif (ptr_dec = '1') then ptr_reg <= ptr_reg - 1;
      end if;
    end if;
  end process;

  -- Ridici proces pro PC
  pc_controller: process (CLK, RESET) begin
    if (RESET = '1') then pc_reg <= (others => '0');
    elsif rising_edge(CLK) then
      if (pc_inc = '1') then pc_reg <= pc_reg + 1;
      elsif (pc_dec = '1') then pc_reg <= pc_reg - 1;
      end if;
    end if;
  end process;

  -- Ridici proces multiplexoru MUX1
  mux1_controller: process(pc_reg, ptr_reg, mux1_sel)  begin
    case mux1_sel is
      when '0' => DATA_ADDR <= pc_reg;
      when '1' => DATA_ADDR <= ptr_reg;
      when others => null;
    end case;
  end process;

  -- Ridici proces multiplexoru MUX2
  mux2_controller: process (DATA_RDATA, IN_DATA, tmp_reg, ptr_reg, pc_reg, mux2_sel) begin
    case mux2_sel is
      when "00" => DATA_WDATA <= IN_DATA;
      when "01" => DATA_WDATA <= tmp_reg;
      when "10" => DATA_WDATA <= DATA_RDATA - 1;
      when "11" => DATA_WDATA <= DATA_RDATA + 1;
      when others => null;
    end case;
  end process;

  -- Ridici proces pro aktualni stav
  load_next_state : process (CLK, RESET) begin
    if RESET = '1' then current_state <= state_ready;
    elsif rising_edge(CLK) then
      if EN = '1' then current_state <= next_state;
      end if;
    end if;
  end process;

  -- Finite State Machine (FSM) proces
  fsm : process(DATA_RDATA, IN_VLD, OUT_BUSY, IN_DATA, cnt_reg, tmp_reg, ptr_reg, pc_reg, current_state) begin
    cnt_inc <= '0';
    cnt_dec <= '0';
    tmp_id <= '0';
    ptr_inc <= '0';
    ptr_dec <= '0';
    ptr_rst <= '0';
    pc_inc <= '0';
    pc_dec <= '0';
    mux2_sel <= "00";
    mux1_sel <= '1';
    
    DONE <= '0';
    DATA_EN <= '0';
    OUT_WE <= '0';
    IN_REQ <= '0';
    OUT_DATA <= (others => '0');
    DATA_RDWR <= '0';

    case (current_state) is
      when state_ready =>    
        pc_inc <= '0'; 
        ptr_inc <= '0';    
        READY <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_init;
      when state_init =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        if DATA_RDATA = X"40" then
          ptr_inc <= '1';
          READY <= '1';
          next_state <= state_fetch;
        else
          ptr_inc <= '1';
          next_state <= state_ready;
        end if;
      when state_fetch =>
        mux1_sel <= '0';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        next_state <= state_decode;
      when state_decode =>
        case DATA_RDATA is
          when X"3E" => -- >
            pc_inc <= '1';
            ptr_inc <= '1';
            next_state <= state_fetch;
          when X"3C" => -- <
            pc_inc <= '1';
            ptr_dec <= '1';
            next_state <= state_fetch;
          when X"2B" => -- +
            pc_inc <= '1';
            mux1_sel <= '1';
            DATA_EN <= '1';
            DATA_RDWR <= '1';
            next_state <= state_write_ptr_inc;
          when X"2D" => -- -
            pc_inc <= '1';
            mux1_sel <= '1';
            DATA_EN <= '1';
            DATA_RDWR <= '1';
            next_state <= state_write_ptr_dec;
          when X"5B" => -- [
            pc_inc <= '1';
            mux1_sel <= '1';
            DATA_EN <= '1';
            DATA_RDWR <= '1';
            next_state <= state_while_start;
          when X"5D" => -- ]
            next_state <= state_while_end_read;
          when X"24" => -- $
            mux1_sel <= '1';
            DATA_EN <= '1';
            DATA_RDWR <= '1';
            next_state <= state_read_tmp;
          when X"21" => -- !
            mux1_sel <= '1';
            DATA_EN <= '1';
            DATA_RDWR <= '1';
            next_state <= state_write_tmp;
          when X"2E" => -- .
            pc_inc <= '1';
            mux1_sel <= '1';
            DATA_EN <= '1';
            DATA_RDWR <= '1';
            next_state <= state_write_char;
          when X"2C" => -- ,
            IN_REQ <= '1';
            if (IN_VLD = '1') then
              pc_inc <= '1';
              mux1_sel <= '1';
              mux2_sel <= "00";
              DATA_EN <= '1';
              DATA_RDWR <= '0';
              next_state <= state_fetch;
            end if;
          when X"40" => -- @
            DONE <= '1';
          when others =>
            pc_inc <= '1';
            next_state <= state_fetch;
        end case;
      when state_write_ptr_inc =>
        mux1_sel <= '1';
        mux2_sel <= "11";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_fetch;
      when state_write_ptr_dec =>
        mux1_sel <= '1';
        mux2_sel <= "10";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_fetch;
      when state_while_start =>
        next_state <= state_while_read;
        if (DATA_RDATA = X"00") then cnt_inc <= '1';
        else next_state <= state_fetch;
        end if;
      when state_while_end_read =>
        pc_dec <= '1';
        mux1_sel <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_while_end_cnt;
      when state_read_tmp =>
        tmp_id <= '1';
        pc_inc <= '1';
        next_state <= state_fetch;
      when state_write_tmp =>
        pc_inc <= '1';
        mux1_sel <= '1';
        mux2_sel <= "01";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        next_state <= state_fetch;
      when state_write_char =>
        if OUT_BUSY = '0' then
          OUT_WE <= '1';
          OUT_DATA <= DATA_RDATA;
          next_state <= state_fetch;
        end if;
      when state_while_read =>
        pc_inc <= '1';
        mux1_sel <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        next_state <= state_while_cnt;
      when state_while_cnt =>
        next_state <= state_while_zero;
        case DATA_RDATA is
          when X"5B" => cnt_inc <= '1';
          when X"5D" => cnt_dec <= '1';
          when others => next_state <= state_while_read;
        end case;
      when state_while_zero =>
        if (cnt_reg = "0000000000000") then next_state <= state_fetch;
        else next_state <= state_while_read;
        end if;
      when state_while_end_cnt =>
        next_state <= state_while_end_zero;
        case DATA_RDATA is
          when X"5B" => cnt_inc <= '1';
          when X"5D" => cnt_dec <= '1';
          when others => next_state <= state_while_end_read;
        end case;
      when state_while_end_zero =>
        if (cnt_reg = "0000000000000") then
          pc_inc <= '1';
          next_state <= state_fetch;
        else
          next_state <= state_while_end_read;
        end if;
      when others => null;
    end case;
  end process fsm;

end behavioral;