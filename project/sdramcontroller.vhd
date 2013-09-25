library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity sdramcontroller is
port (
	clk : in STD_LOGIC;
	addr : in STD_LOGIC_VECTOR (15 downto 0);
	dataIn : in STD_LOGIC_VECTOR (7 downto 0);
	dataOut : out STD_LOGIC_VECTOR (7 downto 0);
	cmd : in STD_LOGIC_VECTOR (1 downto 0);
	ready : out STD_LOGIC;
	
	--chipClk : out STD_LOGIC;
	--chipCke : out STD_LOGIC;
	chipCS : out STD_LOGIC;
	chipWE : out STD_LOGIC;
	chipCAS : out STD_LOGIC;
	chipRAS: out STD_LOGIC;
	chipDQML : out STD_LOGIC;
	chipDQMH : out STD_LOGIC;
	chipBA : out STD_LOGIC_VECTOR (1 downto 0);
	chipADDR : out STD_LOGIC_VECTOR (11 downto 0);
	chipDATA : inout STD_LOGIC_VECTOR (15 downto 0));
	
end sdramcontroller;

architecture Behavioral of sdramcontroller is

type stateType is 
(
	init0, init1, init2, init3, init4, init5, init6, init7, init8, 
	idle, decoding, 
	read1, read2, read3, read4, read5, read6,
	write1, write2, write3, write4, write5, write6
);

signal currentState : stateType := init0;
signal writeBuffer : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');

constant cmdCommandInhibit : std_logic_vector (3 downto 0) := "1000";
constant cmdNOP : std_logic_vector (3 downto 0) := "0111";
constant cmdActive : std_logic_vector (3 downto 0) := "0011";
constant cmdRead : std_logic_vector (3 downto 0) := "0101";
constant cmdWrite : std_logic_vector (3 downto 0) := "0100";
constant cmdBurstTerminate : std_logic_vector (3 downto 0) := "0110";
constant cmdPrecharge : std_logic_vector (3 downto 0) := "0010";
constant cmdAutoRefresh : std_logic_vector (3 downto 0) := "0001";
constant cmdLoadMoadReg : std_logic_vector (3 downto 0) := "0000";

signal chipCmd :  std_logic_vector (3 downto 0) := "UUUU";
signal waitCounter : std_logic_vector (31 downto 0) := (others => '0');

signal writeChipDataEnable : std_logic := '0';

begin

	--chipClk <= clk;
	--chipCke <= '1';
	chipCS <= chipCmd(3);
	chipRAS <= chipCmd(2);
	chipCAS <= chipCmd(1);
	chipWE <= chipCmd(0);
	
	chipDATA <= "00000000" & writeBuffer when (writeChipDataEnable = '1') else (others => 'Z');
	
	mainproc: 
	process (clk)
	begin
		if (falling_edge(clk) and currentState = read5) then
			-- put the lower bits of data on the bus (we store 8 bit words in 16 bits)
			dataOut <= chipDATA(7 downto 0);
		end if;
		if (rising_edge(clk)) then
			case (currentState) is
				when init0 =>
					ready <= '0';
					-- wait 100 ms and send NOP then init1
					--if (waitCounter = 10) then 
					if (waitCounter = 14000000) then 
						-- set once since the address bus is only 16 bits, so we are only ever using one bank of memory
						chipCmd <= cmdNOP;
						currentState <= init1;
						waitCounter <= (others => '0');
					else 
						waitCounter <= waitCounter + 1;
					end if;
				when init1 =>
					-- send PRECHARGE ALL
					chipBA <= "00"; 
					chipCmd <= cmdPrecharge;
					chipADDR <= "010000000000";
					currentState <= init2;
				when init2 =>
					-- wait tRP while sending NOP
					if (waitCounter = 3) then 
						currentState <= init3;
						waitCounter <= (others => '0');
					else
						chipCmd <= cmdNOP;
						waitCounter <= waitCounter + 1;
					end if;
				when init3 =>
					-- send AUTO REFRESH
					chipCmd <= cmdAutoRefresh;
					currentState <= init4;
				when init4 =>
					-- wait tRFC sending NOP
					if (waitCounter = 8) then -- 66ns
						currentState <= init5;
						waitCounter <= (others => '0');
					else
						chipCmd <= cmdNOP;
						waitCounter <= waitCounter + 1;
					end if;
				when init5 =>
					-- send AUTO REFRESH
					chipCmd <= cmdAutoRefresh;
					currentState <= init6;
				when init6 =>
					-- wait tRFC sending NOP
					if (waitCounter = 9) then -- 66ns
						currentState <= init7;
						waitCounter <= (others => '0');
					else
						chipCmd <= cmdNOP;
						waitCounter <= waitCounter + 1;
					end if;
				when init7 =>
					-- send LOAD MODE REGISTER
					chipADDR <= "001000100000";
					chipCmd <= cmdLoadMoadReg;
					currentState <= init8;
				when init8 =>
					-- wait tMRD sending NOP, then we're ready!
					if (waitCounter = 15) then -- 2 * tCK
						currentState <= idle;
						waitCounter <= (others => '0');
					else
						chipCmd <= cmdNOP;
						waitCounter <= waitCounter + 1;
					end if;
				when idle =>
					ready <= '1';
					writeChipDataEnable <= '0';
					currentState <= idle;
					if (cmd /= "00") then
						ready <= '0';
						currentState <= decoding;
					end if;
				when decoding =>
					-- send active
					chipCmd <= cmdActive;
					-- set row in a0 - a11
					chipADDR(11 downto 8) <= "0000";
					chipADDR(7 downto 0) <= addr(15 downto 8);
					-- set bank in BA0/1 (already done once)
					case (cmd) is
						when "01" => -- read
							currentState <= read1;
						when "11" => -- write
							currentState <= write1;
						when others =>
							chipCmd <= cmdNOP;
							currentState <= idle;
					end case;
				when read1 =>
					chipCmd <= cmdNOP;
					chipDQMH <= '0';
					chipDQML <= '0';
					currentState <= read2;
				when read2 =>
					chipCmd <= cmdRead;
					-- set column in a0 - a7
					chipADDR(7 downto 0) <= addr(7 downto 0);
					-- set a10 low (no auto precharge)
					chipADDR(10) <= '0';
					currentState <= read3;
				when read3 =>
					chipCmd <= cmdNOP;
					currentState <= read4;
				when read4 =>
					currentState <= read5;
				when read5 =>
					-- send precharge
					chipCmd <= cmdPrecharge;
					chipADDR <= "001000000000";
					currentState <= read6;
				when read6 => 
					-- one more NOP
					chipCmd <= cmdNOP;
					-- go idle
					currentState <= idle;
				when write1 =>
					chipCmd <= cmdNOP;
					writeBuffer(7 downto 0) <= dataIn;
					writeChipDataEnable <= '1';
					currentState <= write2;
					chipDQMH <= '0';
					chipDQML <= '0';
					-- set column in a0 - a7
					chipADDR(7 downto 0) <= addr(7 downto 0);
					-- set a10 low (no auto precharge)
					chipADDR(10) <= '0';
				when write2 =>
					chipCmd <= cmdWrite;
					currentState <= write3;
				when write3 =>
					chipCmd <= cmdNOP;
					currentState <= write4;
				when write4 =>
					chipCmd <= cmdNOP;
					currentState <= write5;
				when write5 =>
					-- send precharge
					chipCmd <= cmdPrecharge;
					chipADDR <= "001000000000";
					currentState <= write6;
				when write6 => 
					-- one more NOP
					chipCmd <= cmdNOP;
					-- go idle
					currentState <= idle;
			end case;
		end if;
	end process mainproc;

end Behavioral;

