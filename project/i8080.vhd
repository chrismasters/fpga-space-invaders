
library work;
use work.opcodes.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity i8080 is
Port (
	clk : in STD_LOGIC;
	addressBus : out  STD_LOGIC_VECTOR (15 downto 0);
	dataIn : in  STD_LOGIC_VECTOR (7 downto 0);
	dataOut : out  STD_LOGIC_VECTOR (7 downto 0);
	sync : out  STD_LOGIC; -- not implemented
	dataBusIn : out STD_LOGIC;
	ready : in STD_LOGIC;
	waitAck : out STD_LOGIC;
	wr : out  STD_LOGIC;
	hold : in  STD_LOGIC; -- not implemented
	hlda : out  STD_LOGIC; -- not implemented
	inte : out  STD_LOGIC; -- not implemented
	int : in  STD_LOGIC; -- not implemented
	reset : in STD_LOGIC);
end i8080;

architecture Behavioral of i8080 is

COMPONENT RegisterArray
PORT(
	clk : IN std_logic;
	selector : IN std_logic_vector(7 downto 0);
	dataIn : IN std_logic_vector(15 downto 0);
	load : IN std_logic;          
	dataOut : OUT std_logic_vector(15 downto 0)
);
END COMPONENT;

type stateType is (
fetch0, fetch1, 
decode,
dad1, dad2,
lxi1, lxi2, lxi3, lxi4, lxi5
);

signal currentState : stateType := fetch0;
signal pc : STD_LOGIC_VECTOR(15 downto 0);
signal opcode : STD_LOGIC_VECTOR (7 downto 0);

signal regSelector : STD_LOGIC_VECTOR (7 downto 0);
signal regDataIn : STD_LOGIC_VECTOR (15 downto 0);
signal regDataOut : STD_LOGIC_VECTOR (15 downto 0);
signal regLoad : STD_LOGIC;

signal tempReg1 : STD_LOGIC_VECTOR (7 downto 0);
signal tempReg2 : STD_LOGIC_VECTOR (7 downto 0);

signal a : STD_LOGIC_VECTOR (7 downto 0);

signal flagsSign : STD_LOGIC := '0';
signal flagsZero : STD_LOGIC := '0';
signal flagsAuxCarry : STD_LOGIC := '0';
signal flagsParity : STD_LOGIC := '0';
signal flagsCarry : STD_LOGIC := '0';

begin
	
registers: RegisterArray PORT MAP(
	clk => clk,
	selector => regSelector,
	dataIn => regDataIn,
	dataOut => regDataOut,
	load => regLoad
);

procloop:
process (clk, reset)
variable srcSReg : std_logic_vector(2 downto 0) := "000";
variable srcDReg : std_logic_vector(1 downto 0) := "00";
variable destSReg : std_logic_vector(2 downto 0) := "000";
variable destDReg : std_logic_vector(1 downto 0) := "00";

variable waitCycleCount : integer := 0;

begin
	if (reset = '1') then
		currentState <= fetch0;
		pc <= X"0000"; 
		regLoad <= '0';
	elsif (rising_edge(clk)) then
		case (currentState) is
			when fetch0 =>
				addressBus <= pc;
				dataBusIn <= '1';
				currentState <= fetch1;
			when fetch1 =>
				-- wait
				pc <= pc + 1;
				currentState <= decode;
			when decode =>
				if (not ready = '1') then
					waitAck <= '1';
				else
					-- once external memory is ready, take the opcode off the data bus
					opcode <= dataIn;
					dataBusIn <= '0';
					waitAck <= '0';
					-- work out what to do with the opcode
					if (opcode = opcNOP) then
						-- nothing to see here...
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "001") then
						if (opcode(3) = '0') then
							-- LXI instructions - read two more bytes
							addressBus <= pc;
							dataBusIn <= '1';
							currentState <= lxi1;
						else
							-- DAD instructions (double add) - 16 bit number in srcDReg is added to the 16 bit number in HL
							srcDReg := opcode(5 downto 4);
							case srcDReg is
								when "00" => regSelector <= "00000011";
								when "01" => regSelector <= "00001100";
								when "10" => regSelector <= "00110000";
								when "11" => regSelector <= "01000000";
								when others =>
							end case;
							currentState <= dad1;
						end if;
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "010") then
						if (opcode(5) = '0') then
							-- opcode(3) = 0 then STAX else LDAX
							-- opcode(4) = 0 then BC else DE
							-- STAX - store accumulator - contents of accumulator are placed in the address in BC or DE
							-- LDAX - load accumulator - contents of the address in BC or DE are placed in the accumulator 
						elsif (opcode(5 downto 4) = "10") then
							-- opcode(3) = 0 then SHLD else LHLD
							-- SHLD - store HL direct - contents of L are stored at the address following the instruction, and the contents of H are stored at the location after that
							-- LHLD - load HL direct 
						elsif (opcode(5 downto 4) = "11") then
							-- opcode(3) = 0 then STA else LDA
							-- STA -- store accumulator direct - the contents of the accumulator replace the byte at the address following the instruction
							-- LDA -- load accumulatro direct - the contents of the accumulator are replaced by the byte at the address following the instruction
						end if;
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "011") then
						destDReg := opcode(5 downto 4);
						if (opcode(3) = '0') then
							-- INX
						else
							-- DCX
						end if;
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "100") then
						-- INR
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "101") then
						-- DCR
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "110") then
						-- MVI
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "111") then
						-- rotate accumulator instructions
						if (opcode(5) = '0') then
							-- RLC
							-- RRC
							-- RAL
							-- RAR
						else
							if (opcode(4 downto 3) = "00") then
								-- DAA -- decimal adjust accumulator 00100111
								-- todo
							elsif (opcode(4 downto 3) = "01") then
								-- CMA -- complement accumulator     00101111
								a <= not a;
								currentState <= fetch1;
							else 
								case (opcode(3)) is
									-- STC -- set carry (carry to 1)     00110111
									when '0' => flagsCarry <= '1';
									-- CMC -- complement carry           00111111
									when others => flagsCarry <= not flagsCarry;
								end case;
								currentState <= fetch1;
							end if;
						end if;
					elsif (opcode(7 downto 6) = "01") then 
						-- this covers 0x40 to 0x7F for data transfer... also HLT is in here (76)
						srcSReg := opcode(2 downto 0);
						destSReg := opcode(5 downto 3);
						if (opcode = opcHLT) then
							-- who goes there?
						elsif (srcSReg = "110") then
							-- read from memory address in HL
						elsif (destSReg = "110") then
							-- write to memory address in HL 
						else
							-- register transfer
						end if;
					elsif (opcode(7 downto 6) = "10") then 
						-- arithmetic from 0x80 to 0x9F, logical from 0xA0 to 0xBF
						if (opcode(5 downto 4) = "00") then
							-- add
							if (opcode(3) = '1') then
								-- with carry
							end if;
						elsif (opcode(5 downto 4) = "01") then
							if (opcode(3) = '1') then
								-- with borrow
							end if;
						elsif (opcode(5 downto 3) = "100") then
							-- ANA -- logical and register or memory with accumulator
						else -- if (opcode(5 downto 3) = "101") then
							-- XRA -- logical XOR register or memory with accumulator
						end if;
						
					elsif (opcode(7 downto 6) = "11") then 
						--  from 0xC0 to 0xFF
						if (opcode(2 downto 0) = "111") then
							-- RST
							-- opcode(5 downto 3) * 8
						elsif (opcode(2 downto 0) = "000" or opcode(2 downto 0) = "010" or opcode(2 downto 0) = "100") then
							-- opcode(2 downto 0) = "000" -- Return if
							-- opcode(2 downto 0) = "010" -- Jump if
							-- opcode(2 downto 0) = "100" -- Call if
							if (opcode(5 downto 3) = "000") then  -- call if zero is not zero
							elsif (opcode(5 downto 3) = "001") then -- call if zero bit is zero
							elsif (opcode(5 downto 3) = "010") then -- call is the carry bit is zero
							elsif (opcode(5 downto 3) = "011") then -- call if carry is one
							elsif (opcode(5 downto 3) = "100") then -- call if parity is zero
							elsif (opcode(5 downto 3) = "101") then -- call if parity is one
							elsif (opcode(5 downto 3) = "110") then -- call if sign bit is zero
							else -- (opcode(5 downto 3) = "111") -- call if sign bit is one
							end if;
							
						elsif (opcode = opcPOP_B) then
						elsif (opcode = opcRET) then
						elsif (opcode = opcPOP_D) then
						elsif (opcode = opcPOP_H) then 
						elsif (opcode = opcPCHL) then 
						elsif (opcode = opcPOP_PSW) then
						elsif (opcode = opcSPHL) then

						elsif (opcode = opcJMP_nn) then
						elsif (opcode = opcOUT) then
						elsif (opcode = opcIN) then   
						elsif (opcode = opcXTHL) then
						elsif (opcode = opcXCHG) then
						elsif (opcode = opcDI) then
						elsif (opcode = opcEI) then

						elsif (opcode = opcPUSH_B) then
						elsif (opcode = opcCALL) then
						elsif (opcode = opcPUSH_D) then
						elsif (opcode = opcPUSH_H) then
						elsif (opcode = opcPUSH_PSW) then

						elsif (opcode = opcADI) then
						elsif (opcode = opcACI) then
						elsif (opcode = opcSUI) then  
						elsif (opcode = opcSBI) then  
						elsif (opcode = opcANI) then  
						elsif (opcode = opcXRI) then
						elsif (opcode = opcORI) then
						elsif (opcode = opcCPI) then

						end if;
					else
						-- uh oh... NOP
					end if;
				end if;
			when dad1 =>
				waitCycleCount := 0;
				tempReg1 <= regDataOut(7 downto 0); -- latch register array output
				tempReg2 <= regDataOut(15 downto 8);
				regSelector <= "00110000"; -- read hl
				currentState <= dad2;
			when dad2 =>
				if (waitCycleCount = 0) then
					regDataIn <= regDataOut + (tempReg2 & tempReg1); -- add temp and register output and store
					regLoad <= '1';
					waitCycleCount := waitCycleCount + 1;
				elsif (waitCycleCount = 5) then
					currentState <= fetch0;
				else
					waitCycleCount := waitCycleCount + 1;
					regLoad <= '0';
				end if;
			when lxi1 =>
				-- wait
				pc <= pc + 1;
				waitCycleCount := 0;
				currentState <= lxi2;
			when lxi2 =>
				if (not ready = '1') then
					waitAck <= '1';
				else
					-- once external memory is ready, take the value off the data bus
					tempReg1 <= dataIn;
					addressBus <= pc;
					dataBusIn <= '1';
					waitAck <= '0';
					currentState <= lxi3;
				end if;
			when lxi3 =>
				-- wait
				pc <= pc + 1;
				waitCycleCount := 0;
				currentState <= lxi4;
			when lxi4 =>
				if (not ready = '1') then
					waitAck <= '1';
				else
					-- once external memory is ready, take the value off the data bus
					tempReg2 <= dataIn;
					waitAck <= '0';
					destDReg := opcode(5 downto 4);
					case destDReg is
						when "00" => regSelector <= "00000011";
						when "01" => regSelector <= "00001100";
						when "10" => regSelector <= "00110000";
						when "11" => regSelector <= "01000000";
						when others =>
					end case;
					regDataIn <= tempReg2 & tempReg1;
					regLoad <= '1';
					currentState <= lxi5;
				end if;
			when lxi5 =>
				currentState <= fetch0;
		end case;
	end if;
end process;

end Behavioral;

