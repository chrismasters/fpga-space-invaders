
library work;
use work.opcodes.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;
use IEEE.STD_LOGIC_MISC.ALL;

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
	reset : in STD_LOGIC;
	status : out STD_LOGIC_VECTOR(3 downto 0));
end i8080;

architecture Behavioral of i8080 is

COMPONENT RegisterArray PORT(
	clk : IN std_logic;
	selector : IN std_logic_vector(7 downto 0);
	dataIn : IN std_logic_vector(15 downto 0);
	load : IN std_logic;          
	dataOut : OUT std_logic_vector(15 downto 0));
END COMPONENT;

COMPONENT alu PORT(
	clk : IN std_logic;
	operandA : IN std_logic_vector(7 downto 0);
	operandB : IN std_logic_vector(7 downto 0);
	operatorSelect : IN std_logic_vector(2 downto 0);
	carryIn : IN std_logic;          
	carryOut : OUT std_logic;
	zeroOut : OUT std_logic;
	signOut : OUT std_logic;
	parityOut : OUT std_logic;
	auxCarryOut : OUT std_logic;
	result : OUT std_logic_vector(7 downto 0));
END COMPONENT;

type stateType is (
halted0, badopcode,
fetch0, fetch1, 
decode,
readDirect0,readDirect1,readDirect2,readDirect3,
readImmediate0, readImmediate1,
readRegPair0, readRegPair1, readRegPair2,
dad1, dad2,
lxi0, lxi1,
mvi0, mvi1a, mvi2a, mvi3a, mvi1b,
stax0, stax1, stax2,
ldax0, ldax1, ldax2,
shld0, shld1, shld2, shld3, shld4,
lhld0, lhld1, lhld2, lhld3, lhld4,
sta0, sta1, sta2, 
lda0, lda1, lda2,
inxdcx0, inxdcx1,
inrdcrmem0, inrdcrmem1, inrdcrmem2, inrdcrmem3, inrdcrmem4,
inrdcrreg0, inrdcrreg1,
aluOp0a, aluOp0b, aluOp1,
postRotate0,
daa0,
call0, call1, call2, call3, call4,
jump0,
ret0, ret1, ret2, ret3, ret4,
movmemtoreg0, movmemtoreg1, movmemtoreg2, movmemtoreg3,
movregtomem0, movregtomem1, movregtomem2, movregtomem3,
movregtoreg0, movregtoreg1, movregtoreg2,
pop0, pop1, pop2, pop3, pop4,
push0, push1, push2, push3, push4, push5,
pchl0,
sphl0, sphl1,
ani0, ani1,
ori0, ori1,
inout0,
xchg0, xchg1, xchg2, xchg3
);

signal currentState : stateType := fetch0;
signal readMemReturnState : stateType := fetch0;

signal pc : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');

signal regSelector : STD_LOGIC_VECTOR (7 downto 0);
signal regDataIn : STD_LOGIC_VECTOR (15 downto 0);
signal regDataOut : STD_LOGIC_VECTOR (15 downto 0);
signal regLoad : STD_LOGIC := '0';

signal tempReg1 : STD_LOGIC_VECTOR (7 downto 0);
signal tempReg2 : STD_LOGIC_VECTOR (7 downto 0);

signal a : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');

signal flagsSign : STD_LOGIC := '0';
signal flagsZero : STD_LOGIC := '0';
signal flagsAuxCarry : STD_LOGIC := '0';
signal flagsParity : STD_LOGIC := '0';
signal flagsCarry : STD_LOGIC := '0';

signal aluOperandA : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
signal aluOperandB : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
signal aluCarryIn : STD_LOGIC := '0';
signal aluCarryOut : STD_LOGIC := '0';
signal aluZeroOut : STD_LOGIC := '0';
signal aluSignOut : STD_LOGIC := '0';
signal aluParityOut : STD_LOGIC := '0';
signal aluAuxCarryOut : STD_LOGIC := '0';
signal aluResult : STD_LOGIC_VECTOR (7 downto 0);
signal aluOpSelect : STD_LOGIC_VECTOR (2 downto 0) := "000";

signal intEnabled : STD_LOGIC := '1';

constant regBC  :  std_logic_vector(1 downto 0) := "00"; 
constant regDE  :  std_logic_vector(1 downto 0) := "01"; 
constant regHL  :  std_logic_vector(1 downto 0) := "10"; 
constant regSP  :  std_logic_vector(1 downto 0) := "11"; 

constant regA   :  std_logic_vector(2 downto 0) := "111"; 
constant regB   :  std_logic_vector(2 downto 0) := "000"; 
constant regC   :  std_logic_vector(2 downto 0) := "001"; 
constant regD   :  std_logic_vector(2 downto 0) := "010"; 
constant regE   :  std_logic_vector(2 downto 0) := "011"; 
constant regH   :  std_logic_vector(2 downto 0) := "100"; 
constant regL   :  std_logic_vector(2 downto 0) := "101"; 

begin

inte <= intEnabled;

registers: RegisterArray PORT MAP(
	clk => clk,
	selector => regSelector,
	dataIn => regDataIn,
	dataOut => regDataOut,
	load => regLoad
);

ialu: alu PORT MAP(
	clk => clk,
	operandA => aluOperandA,
	operandB => aluOperandB,
	operatorSelect => aluOpSelect,
	carryIn => aluCarryIn,
	carryOut => aluCarryOut,
	zeroOut => aluZeroOut,
	signOut => aluSignOut,
	parityOut => aluParityOut,
	auxCarryOut => aluAuxCarryOut,
	result => aluResult
);

procloop:
process (clk, reset)

pure function regToSelector(reg : std_logic_vector(2 downto 0)) return std_logic_vector is
	variable regSelector : std_logic_vector(7 downto 0);
begin
	case reg is
		when regB =>   regSelector := "00000001";
		when regC =>   regSelector := "00000010";
		when regD =>   regSelector := "00000100";
		when regE =>   regSelector := "00001000";
		when regH =>   regSelector := "00010000";
		when others => regSelector := "00100000"; -- regL
	end case;
	return regSelector;
end function regToSelector;

--impure function incrementByte(v : std_logic_vector) return std_logic_vector(7 downto 0) is 
--	variable newValue : std_logic_vector(7 downto 0);
--begin
	--flagsParity <= v(0) xor v(1) xor v(2) xor v(3) xor v(4) xor v(5) xor v(6) xor v(7);
	--flagsAuxCarry <= and_reduce((v and X"F") + 1 and X"10");
	--newValue := v + 1; -- todo fix
	--flagsSign <= or_reduce(newValue and X"80");
	--flagsZero <= not(or_reduce(v));
--	return "00000000"; --newValue;
--end function incrementByte;

--impure function decrementByte(v : std_logic_vector) return std_logic_vector(7 downto 0) is 
--	variable newValue : std_logic_vector(7 downto 0);
--begin
	--flagsParity <= v(0) xor v(1) xor v(2) xor v(3) xor v(4) xor v(5) xor v(6) xor v(7);
	--flagsAuxCarry <= and_reduce((v and X"F") - 1 and X"10");
	--newValue := v - 1; -- todo fix
	--flagsSign <= or_reduce(newValue and X"80");
	--flagsZero <= not(or_reduce(v));
--	return "00000000"; -- newValue;
--end function decrementByte;

function to_hstring (value     : STD_LOGIC_VECTOR) return STRING is
    constant ne     : INTEGER := (value'length+3)/4;
    variable pad    : STD_LOGIC_VECTOR(0 to (ne*4 - value'length) - 1);
    variable ivalue : STD_LOGIC_VECTOR(0 to ne*4 - 1);
    variable result : STRING(1 to ne);
    variable quad   : STD_LOGIC_VECTOR(0 to 3);
  begin
    if value'length < 1 then
      return result;
    else
      if value (value'left) = 'Z' then
        pad := (others => 'Z');
      else
        pad := (others => '0');
      end if;
      ivalue := pad & value;
      for i in 0 to ne-1 loop
        quad := To_X01Z(ivalue(4*i to 4*i+3));
        case quad is
          when x"0"   => result(i+1) := '0';
          when x"1"   => result(i+1) := '1';
          when x"2"   => result(i+1) := '2';
          when x"3"   => result(i+1) := '3';
          when x"4"   => result(i+1) := '4';
          when x"5"   => result(i+1) := '5';
          when x"6"   => result(i+1) := '6';
          when x"7"   => result(i+1) := '7';
          when x"8"   => result(i+1) := '8';
          when x"9"   => result(i+1) := '9';
          when x"A"   => result(i+1) := 'A';
          when x"B"   => result(i+1) := 'B';
          when x"C"   => result(i+1) := 'C';
          when x"D"   => result(i+1) := 'D';
          when x"E"   => result(i+1) := 'E';
          when x"F"   => result(i+1) := 'F';
          when "ZZZZ" => result(i+1) := 'Z';
          when others => result(i+1) := 'X';
        end case;
      end loop;
      return "0x" & result;
    end if;
  end function to_hstring;

variable waitCycleCount : integer := 0;
variable opcode : STD_LOGIC_VECTOR (7 downto 0);

begin
	if (reset = '1') then
		currentState <= fetch0;
		pc <= (others => '0'); 
		regLoad <= '0';
		wr <= '0';
		dataOut <= (others => '0');
		status <= "0001";
	elsif (rising_edge(clk)) then
		case (currentState) is
			when fetch0 =>
				report to_hstring(pc);
				addressBus <= pc;
				dataBusIn <= '1';
				currentState <= fetch1;
				regLoad <= '0';
				regSelector <= "01000000"; -- default to selecting the SP
			when fetch1 =>
				-- wait
				if (pc = X"0A9E") then -- DrawStatus - ix3
					pc <= pc + 1;
				end if;
				pc <= pc + 1;
				currentState <= decode;
			when decode =>
				if (not ready = '1') then
					waitAck <= '1';
				else
					-- once external memory is ready, take the opcode off the data bus
					opcode := dataIn;
					aluOpSelect <= opcode(5 downto 3);
					dataBusIn <= '0';
					waitAck <= '0';
					-- work out what to do with the opcode
					if (opcode = opcNOP) then
						-- nothing to see here...
						currentState <= fetch0;
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "001") then
						if (opcode(3) = '0') then
							-- LXI instructions - read two more bytes
							addressBus <= pc;
							dataBusIn <= '1';
							readMemReturnState <= lxi0;
							currentState <= readDirect0;
						else
							-- DAD instructions (double add) - 16 bit number in srcDReg is added to the 16 bit number in HL
							case opcode(5 downto 4) is
								when regBC =>  regSelector <= "00000011";
								when regDE =>  regSelector <= "00001100";
								when regHL =>  regSelector <= "00110000";
								when others => regSelector <= "01000000"; -- regSP
							end case;
							currentState <= dad1;
						end if;
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "010") then
						if (opcode(5) = '0') then
							-- STAX - store accumulator - contents of accumulator are placed in the address in BC or DE
							-- LDAX - load accumulator - contents of the address in BC or DE are placed in the accumulator 
							case opcode(4) is
								when '0' => regSelector <= "00000011"; -- BC
								when others => regSelector <= "00001100";  -- DE
							end case;
							regLoad <= '0'; 
							case opcode(3) is
								when '0' => currentState <= stax0;
								when others => currentState <= ldax0;
							end case;
						elsif (opcode(5 downto 4) = "10") then
							-- SHLD - store HL direct - contents of L are stored at the address following the instruction, 
							--        and the contents of H are stored at the location after that
							-- LHLD - load HL direct 
							case opcode(3) is
								when '0' => readMemReturnState <= shld0;
								when others => readMemReturnState <= lhld0;
							end case;
							regSelector <= "00110000"; -- HL
							regLoad <= '0';
							addressBus <= pc;
							dataBusIn <= '1';
							currentState <= readDirect0;
						elsif (opcode(5 downto 4) = "11") then
							-- STA -- store accumulator direct - the contents of the accumulator replace the byte at the address following the instruction
							-- LDA -- load accumulatro direct - the contents of the accumulator are replaced by the byte at the address following the instruction
							case opcode(3) is
								when '0' => readMemReturnState <= sta0;
								when others => readMemReturnState <= lda0;
							end case;
							addressBus <= pc;
							dataBusIn <= '1';
							currentState <= readDirect0;
						end if;
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "011") then
						-- INX - increment register pair
						-- DCX - increment register pair
						-- no flags affected
						case opcode(5 downto 4) is
							when regBC => regSelector <= "00000011";
							when regDE => regSelector <= "00001100";
							when regHL => regSelector <= "00110000";
							when others => regSelector <= "01000000"; -- regSP
						end case;
						currentState <= inxdcx0;
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 1) = "10") then
						if (opcode(5 downto 3) = "110") then --  mem ref
							regSelector <= "00110000"; -- load mem ref from HL
							currentState <= inrdcrmem0;
						elsif (opcode(5 downto 3) = "111") then -- accumulator
							if (opcode(0) = '0') then -- INR
								a <= (a(7 downto 0) + 1); -- incrementByte(regDataOut);
							else -- DCR
								a <= (a(7 downto 0) - 1); --decrementByte(regDataOut);
							end if;
							currentState <= fetch0; -- todo
						else
							regSelector <= regToSelector(opcode(5 downto 3));
							currentState <= inrdcrreg0;
						end if;
					elsif (opcode(7 downto 6) = "00" and opcode(2 downto 0) = "110") then
						-- MVI -- todo
						readMemReturnState <= mvi0;
						addressBus <= pc;
						dataBusIn <= '1';
						currentState <= readImmediate0;
					elsif (opcode(7 downto 5) = "000" and opcode(2 downto 0) = "111") then
						-- rotate accumulator instructions
						if (opcode = opcRLC) then
							tempReg1(7 downto 1) <= a(6 downto 0);
							tempReg1(0) <= a(7);
							flagsCarry <= a(7);
						elsif (opcode = opcRRC) then
							tempReg1(6 downto 0) <= a(7 downto 1);
							tempReg1(7) <= a(0);
							flagsCarry <= a(0);						
						elsif (opcode = opcRAL) then
							tempReg1(7 downto 0) <= a(6 downto 0) & flagsCarry;
							flagsCarry <= a(7); -- todo: verify you can do this as its used above
						elsif (opcode = opcRAR) then
							tempReg1(7 downto 0) <= flagsCarry & a(7 downto 1);
							flagsCarry <= a(0); -- todo: verify you can do this as its used above
						end if;
						currentState <= postRotate0;
					elsif (opcode(7 downto 5) = "001" and opcode(2 downto 0) = "111") then
						if (opcode(4 downto 3) = "00") then
							-- DAA -- decimal adjust accumulator 00100111
							if (a(3 downto 0) > 9 or flagsAuxCarry = '1') then
								a <= a + 6;
								-- todo set aux carry if reqd
							end if;
							currentState <= daa0;
						elsif (opcode(4 downto 3) = "01") then
							-- CMA -- complement accumulator     00101111
							a <= not a;
							currentState <= fetch0;
						else 
							if (opcode(3) = '0') then
								-- STC -- set carry (carry to 1)  00110111
								flagsCarry <= '1';
							else
								-- CMC -- complement carry        00111111
								flagsCarry <= not flagsCarry;
							end if;
							currentState <= fetch0;
						end if;
					elsif (opcode(7 downto 6) = "01") then 
						-- this covers 0x40 to 0x7F for data transfer... also HLT is in here (76)
						if (opcode = opcHLT) then
							-- who goes there?
							currentState <= halted0;
						elsif (opcode(2 downto 0) = "110") then
							-- read from memory address in HL
							regSelector <= "00110000";
							currentState <= movmemtoreg0;
						elsif (opcode(5 downto 3) = "110") then
							-- write to memory address in HL 
							if (opcode(2 downto 0) = "111") then -- a
								dataOut <= a;
								regSelector <= "00110000";
								currentState <= movregtomem1;
							else
								regSelector <= regToSelector(opcode(2 downto 0));
								currentState <= movregtomem0;
							end if;
						else
							-- register transfer
							if (opcode(2 downto 0) = "111") then -- a 
								tempReg1 <= a;
								currentState <= movregtoreg1;
							else
								regSelector <= regToSelector(opcode(2 downto 0));
								currentState <= movregtoreg0;
							end if;
						end if;
					elsif (opcode(7 downto 6) = "10") then 
						-- arithmetic from 0x80 to 0x9F, logical from 0xA0 to 0xBF
						-- operation already connected to alu
						aluOperandA <= a;
						aluCarryIn <= flagsCarry;
						if (opcode(2 downto 0) = "110") then -- memory
							regSelector <= "00110000"; -- load mem ref from HL
							readMemReturnState <= aluOp0b;
							currentState <= readRegPair0;
						else -- register
							regSelector <= regToSelector(opcode(2 downto 0));
							currentState <= aluOp0a;
						end if;
					elsif (opcode(7 downto 6) = "11") then 
						--  from 0xC0 to 0xFF
						if (opcode(2 downto 0) = "111") then
							-- RST -- todo
							-- opcode(5 downto 3) * 8
							currentState <= fetch0;
						elsif (opcode = opcCALL or opcode(2 downto 0) = "100") then -- conditional calls
							addressBus <= pc;
							dataBusIn <= '1';
							readMemReturnState <= call0;
							regSelector <= "01000000";
							currentState <= readDirect0;
						elsif (opcode = opcJMP or opcode(2 downto 0) = "010") then -- conditional jumps
							addressBus <= pc;
							dataBusIn <= '1';
							readMemReturnState <= jump0;
							regSelector <= "01000000";
							currentState <= readDirect0;
						elsif (opcode = opcRET or opcode(2 downto 0) = "000") then -- conditional returns
							if (opcode = opcRET 
								or (opcode(5 downto 3) = "000" and flagsZero = '0')
								or (opcode(5 downto 3) = "001" and flagsZero = '1')
								or (opcode(5 downto 3) = "010" and flagsCarry = '0')
								or (opcode(5 downto 3) = "011" and flagsCarry = '1')
								or (opcode(5 downto 3) = "100" and flagsParity = '0')
								or (opcode(5 downto 3) = "101" and flagsParity = '1')
								or (opcode(5 downto 3) = "110" and flagsSign = '0')
								or (opcode(5 downto 3) = "111" and flagsSign = '1')) then
								regSelector <= "01000000";
								currentState <= ret0;
							else
								currentState <= fetch0;
							end if;
							
						elsif (opcode(3 downto 0) = "0001") then
							-- pop
							addressBus <= regDataOut;
							dataBusIn <= '1';
							currentState <= pop0;
						elsif (opcode(3 downto 0) = "0101") then
							-- push
							if (opcode(5 downto 4) = "11") then
								-- push from A and flags
								tempReg1 <= a;
								tempReg2 <= flagsSign & flagsZero & "0" & flagsAuxCarry & "0" & flagsParity & "1" & flagsCarry;
								currentState <= push1;
							else
								case opcode(5 downto 4) is
									when regBC => regSelector <= "00000011";
									when regDE => regSelector <= "00001100";
									when others => regSelector <= "00110000"; -- regHL
								end case;
								currentState <= push0;
							end if;
							
						elsif (opcode = opcPCHL) then 
							-- PC <= HL
							regSelector <= "00110000";
							currentState <= pchl0;
							
						elsif (opcode = opcSPHL) then
							-- SP <= HL
							regSelector <= "00110000";
							currentState <= sphl0;

						elsif (opcode = opcOUT or opcode = opcIN) then
							readMemReturnState <= inout0;
							addressBus <= pc;
							dataBusIn <= '1';
							currentState <= readImmediate0;
						
						--elsif (opcode = opcXTHL) then
						
						elsif (opcode = opcXCHG) then
							-- register exchange DE <-> HL
							regSelector <= "00001100";
							currentState <= xchg0;							
							
						elsif (opcode = opcDI) then
							intEnabled <= '0';
							currentState <= fetch0;							

						elsif (opcode = opcEI) then
							intEnabled <= '1';
							currentState <= fetch0;							

						elsif (opcode(7 downto 6) = "11" and opcode(2 downto 0) = "110") then 
							aluOpSelect <= opcode(5 downto 3);
							aluOperandA <= a;
							addressBus <= pc;
							dataBusIn <= '1';
							readMemReturnState <= ani0;
							currentState <= readImmediate0;

						else
							currentState <= badopcode;
						end if;
					else
						currentState <= badopcode;
					end if;
				end if;
			when readDirect0 =>
				-- wait
				pc <= pc + 1;
				currentState <= readDirect1;
			when readDirect1 =>
				if (not ready = '1') then
					waitAck <= '1';
				else -- once external memory is ready, take the data off the data bus
					tempReg1 <= dataIn;
					waitAck <= '0';
					addressBus <= pc;
					dataBusIn <= '1';
					currentState <= readDirect2;
				end if;
			when readDirect2 =>
				-- wait
				pc <= pc + 1;
				currentState <= readDirect3;
			when readDirect3 =>
				if (not ready = '1') then
					waitAck <= '1';
				else -- once external memory is ready, take the data off the data bus
					tempReg2 <= dataIn;
					waitAck <= '0';
					dataBusIn <= '0';
					currentState <= readMemReturnState;
				end if;
			when readImmediate0 =>
				-- wait
				pc <= pc + 1;
				currentState <= readImmediate1;
			when readImmediate1 =>
				if (not ready = '1') then
					waitAck <= '1';
				else -- once external memory is ready, take the data off the data bus
					tempReg1 <= dataIn;
					waitAck <= '0';
					dataBusIn <= '0';
					currentState <= readMemReturnState;
				end if;
			when readRegPair0 =>
				addressBus <= regDataOut;
				regLoad <= '0';
				dataBusIn <= '1';
				currentState <= readRegPair1;				
			when readRegPair1 =>
				-- wait
				currentState <= readRegPair2;
			when readRegPair2 =>
				if (not ready = '1') then
					waitAck <= '1';
				else -- once external memory is ready, take the data off the data bus
					tempReg1 <= dataIn;
					waitAck <= '0';
					dataBusIn <= '0';
					currentState <= readMemReturnState;
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
					regLoad <= '0';
					currentState <= fetch0;
				else
					waitCycleCount := waitCycleCount + 1;
				end if;
			when lxi0 =>
				case opcode(5 downto 4) is
					when regBC =>  regSelector <= "00000011";
					when regDE =>  regSelector <= "00001100";
					when regHL =>  regSelector <= "00110000";
					when others => regSelector <= "01000000"; -- SP
				end case;
				regDataIn <= tempReg2 & tempReg1;
				regLoad <= '1';
				currentState <= lxi1;
			when lxi1 =>
				regLoad <= '0';
				currentState <= fetch0;
			when stax0 =>
				addressBus <= regDataOut;
				dataOut <= a;
				wr <= '1';
				currentState <= stax1;
			when stax1 =>
				-- wait
				currentState <= stax2;
			when stax2 =>
				-- stored.
				wr <= '0';
				currentState <= fetch0;
			when ldax0 =>
				addressBus <= regDataOut;
				dataBusIn <= '1';
				currentState <= ldax1;
			when ldax1 =>
				-- wait
				currentState <= ldax2;
			when ldax2 =>
				if (not ready = '1') then
					waitAck <= '1';
				else -- once external memory is ready, take the data off the data bus
					a <= dataIn;
					dataBusIn <= '0';
					currentState <= fetch0;
				end if;
			when shld0 =>
				-- contents of L are stored at the address following the instruction, 
				-- and the contents of H are stored at the location after that
				addressBus <= (tempReg2 & tempReg1);
				dataOut <= regDataOut(7 downto 0);
				wr <= '1';
				currentState <= shld1;
			when shld1 =>
				-- wait
				currentState <= shld2;
			when shld2 =>
				addressBus <= (tempReg2 & tempReg1) + 1;
				dataOut <= regDataOut(15 downto 8);
				wr <= '1';
				currentState <= shld3;
			when shld3 =>
				-- wait
				currentState <= shld4;
			when shld4 =>
				wr <= '0';
				currentState <= fetch0;
			when lhld0 =>
				-- the byte at the address following the instruction in stored in L
				-- and the contents of the address following that are stored in H
				addressBus <= (tempReg2 & tempReg1);
				dataBusIn <= '1';
				currentState <= lhld1;
			when lhld1 =>
				-- wait
				currentState <= lhld2;
			when lhld2 =>
				if (not ready = '1') then
					waitAck <= '1';
				else -- once external memory is ready, take the data off the data bus
					regDataIn <= "00000000" & dataIn;
					regSelector <= "00100000";
					regLoad <= '1';
					waitAck <= '0';
					addressBus <= (tempReg2 & tempReg1) + 1;
					dataBusIn <= '1';
					currentState <= lhld3;
				end if;
			when lhld3 =>
				-- wait
				currentState <= lhld4;
			when lhld4 =>
				if (not ready = '1') then
					waitAck <= '1';
				else -- once external memory is ready, take the data off the data bus
					regDataIn <= "00000000" & dataIn;
					regSelector <= "00010000";
					regLoad <= '1';
					waitAck <= '0';
					dataBusIn <= '0';
					currentState <= fetch0;
				end if;
			when sta0 =>
				addressBus <= (tempReg2 & tempReg1);
				dataOut <= a;
				wr <= '1';
				currentState <= sta1;
			when sta1 =>
				-- wait
				currentState <= sta2;
			when sta2 =>
				-- stored.
				wr <= '0';
				currentState <= fetch0;
			when lda0 =>
				-- the byte at the address following the instruction in stored in L
				-- and the contents of the address following that are stored in H
				addressBus <= (tempReg2 & tempReg1);
				dataBusIn <= '1';
				currentState <= lda1;
			when lda1 =>
				-- wait
				currentState <= lda2;
			when lda2 =>
				if (not ready = '1') then
					waitAck <= '1';
				else -- once external memory is ready, take the data off the data bus
					a <= dataIn;
					waitAck <= '0';
					dataBusIn <= '0';
					currentState <= fetch0;
				end if;
			when inxdcx0 =>
				if (opcode(3) = '0') then
					regDataIn <= regDataOut + 1;
				else
					regDataIn <= regDataOut - 1;
				end if;
				regLoad <= '1';
				currentState <= inxdcx1;
			when inxdcx1 =>
				regLoad <= '0';
				currentState <= fetch0;
			when inrdcrreg0 =>
				if (opcode(0) = '0') then
					regDataIn <= "00000000" & (regDataOut(7 downto 0) + 1); -- incrementByte(regDataOut);
				else
					regDataIn <= "00000000" & (regDataOut(7 downto 0) - 1); --decrementByte(regDataOut);
				end if;
				regLoad <= '1';
				currentState <= inrdcrreg1;
			when inrdcrreg1 =>	
				flagsZero <= not(or_reduce(regDataIn(7 downto 0)));
				regLoad <= '0';
				currentState <= fetch0;
			when inrdcrmem0 =>
				addressBus <= regDataOut;
				dataBusIn <= '1';
				currentState <= inrdcrmem1;
			when inrdcrmem1 =>
				-- wait
				currentState <= inrdcrmem2;
			when inrdcrmem2 =>
				if (not ready = '1') then
					waitAck <= '1';
				else -- once external memory is ready, take the data off the data bus
					if (opcode(0) = '1') then
						--dataOut <= incrementByte(dataIn);
					else 
						--dataOut <= decrementByte(dataIn);
					end if;
					waitAck <= '0';
					wr <= '1';
					--addressBus <= stays the same as we're writing back
					currentState <= inrdcrmem3;
				end if;
			when inrdcrmem3 =>
				-- wait
				currentState <= inrdcrmem4;
			when inrdcrmem4 =>
				-- stored.
				wr <= '0';
				currentState <= fetch0;
			when mvi0 =>
				if (opcode(5 downto 3) = "110") then
					dataOut <= tempReg1;
					regSelector <= "00110000"; -- HL
					currentState <= mvi1a;
				else
					regSelector <= regToSelector(opcode(5 downto 3));
					regDataIn <= "00000000" & tempReg1;
					regLoad <= '1';
					currentState <= mvi1b;
				end if;
			when mvi1a =>
				addressBus <= regDataOut;
				wr <= '1';
				currentState <= mvi2a;
			when mvi2a =>
				-- wait
				currentState <= mvi3a;
			when mvi3a =>
				-- stored.
				wr <= '0';
				currentState <= fetch0;	
			when mvi1b =>
				regLoad <= '0';
				currentState <= fetch0;
			when aluOp0a => -- from reg
				aluOperandB <= regDataOut(7 downto 0);
				currentState <= aluOp1;
			when aluOp0b => -- from memory
				aluOperandB <= tempReg1;
				currentState <= aluOp1;
			when aluOp1 =>
				a <= aluResult;
				currentState <= fetch0;
			when postRotate0 =>
				a <= tempReg1;
				currentState <= fetch0;
			when daa0 =>
				if (a(7 downto 4) > 9 or flagsCarry = '1') then
					a(7 downto 4) <= a(7 downto 4) + 6;
					-- todo set carry if reqd
				end if;	
				currentState <= fetch0;
			when call0 =>
				if (opcode = opcCALL
					or (opcode(5 downto 3) = "000" and flagsZero = '0')
					or (opcode(5 downto 3) = "001" and flagsZero = '1')
					or (opcode(5 downto 3) = "010" and flagsCarry = '0')
					or (opcode(5 downto 3) = "011" and flagsCarry = '1')
					or (opcode(5 downto 3) = "100" and flagsParity = '0')
					or (opcode(5 downto 3) = "101" and flagsParity = '1')
					or (opcode(5 downto 3) = "110" and flagsSign = '0')
					or (opcode(5 downto 3) = "111" and flagsSign = '1')) then
					-- push pc to stack (high then low)
					addressBus <= regDataOut - 1;
					dataOut <= pc(15 downto 8);
					wr <= '1';
					currentState <= call1;
				else 
					currentState <= fetch0;
				end if;
			when call1 =>
				-- wait
				currentState <= call2;
			when call2 =>
				-- stored
				addressBus <= regDataOut - 2;
				dataOut <= pc(7 downto 0);
				currentState <= call3;
			when call3 =>
				-- wait and set pc to call site
				pc <= tempReg2 & tempReg1;
				regDataIn <= regDataOut - 2;
				regLoad <= '1';
				currentState <= call4;
			when call4 =>
				wr <= '0';
				regLoad <= '0';
				currentState <= fetch0;
			when jump0 =>
				if (opcode = opcJMP
					or (opcode(5 downto 3) = "000" and flagsZero = '0')
					or (opcode(5 downto 3) = "001" and flagsZero = '1')
					or (opcode(5 downto 3) = "010" and flagsCarry = '0')
					or (opcode(5 downto 3) = "011" and flagsCarry = '1')
					or (opcode(5 downto 3) = "100" and flagsParity = '0')
					or (opcode(5 downto 3) = "101" and flagsParity = '1')
					or (opcode(5 downto 3) = "110" and flagsSign = '0')
					or (opcode(5 downto 3) = "111" and flagsSign = '1')) then
					pc <= tempReg2 & tempReg1;
				end if;
				currentState <= fetch0;
				
			when ret0 =>
				addressBus <= regDataOut;
				dataBusIn <= '1';
				currentState <= ret1;
				
			when ret1 =>
				-- wait
				currentState <= ret2;
				
			when ret2 =>
				regLoad <= '0';
				if (not ready = '1') then
					waitAck <= '1';
				else 
					tempReg1 <= dataIn;
					addressBus <= regDataOut + 1;
					waitAck <= '0';
					currentState <= ret3;
				end if;
			
			when ret3 =>
				-- wait
				regDataIn <= regDataOut + 2;
				regLoad <= '1';
				currentState <= ret4;
				
			when ret4 =>
				regLoad <= '0';
				if (not ready = '1') then
					waitAck <= '1';
				else 
					dataBusIn <= '0';
					waitAck <= '0';
					pc <= dataIn & tempReg1;
					currentState <= fetch0;
				end if;

			when movmemtoreg0 =>
				addressBus <= regDataOut;
				dataBusIn <= '1';
				currentState <= movmemtoreg1;

			when movmemtoreg1 =>
				-- wait
				currentState <= movmemtoreg2;
			
			when movmemtoreg2 =>
				if (not ready = '1') then
					waitAck <= '1';
				else 
					dataBusIn <= '0';
					waitAck <= '0';
					if (opcode(5 downto 3) = "111") then
						a <= dataIn;
						currentState <= fetch0;
					else
						regSelector <= regToSelector(opcode(5 downto 3));
						regDataIn(7 downto 0) <= dataIn;
						regLoad <= '1';
						currentState <= movmemtoreg3;
					end if;
				end if;
			
			when movmemtoreg3 =>
				regLoad <= '0';
				currentState <= fetch0;
				
			when movregtomem0 =>
				dataOut <= regDataOut(7 downto 0);
				regSelector <= "00110000";
				currentState <= movregtomem1;
							
			when movregtomem1 =>
				addressBus <= regDataOut;
				wr <= '1';
				currentState <= movregtomem2;
				
			when movregtomem2 =>
				-- wait
				currentState <= movregtomem3;
				
			when movregtomem3 =>
				wr <= '0';
				currentState <= fetch0;

			when movregtoreg0 =>
				tempReg1 <= regDataOut(7 downto 0);
				currentState <= movregtoreg1;
				
			when movregtoreg1 =>
				if (opcode(5 downto 3) = "111") then
					a <= tempReg1;
					currentState <= fetch0;
				else
					regSelector <= regToSelector(opcode(5 downto 3));
					regDataIn <= "00000000" & tempReg1;
					regLoad <= '1';
					currentState <= movregtoreg2;
				end if;
				
			when movregtoreg2 =>
				regLoad <= '0';
				currentState <= fetch0;

			when pop0 =>
				-- wait
				currentState <= pop1;
			
			when pop1 =>
				if (not ready = '1') then
					waitAck <= '1';
				else 
					waitAck <= '0';
					tempReg2 <= dataIn;
					addressBus <= regDataOut + 1;
					dataBusIn <= '1';
					currentState <= pop2;
				end if;

			when pop2 =>
				-- wait and update SP
				regDataIn <= regDataOut + 2;
				regLoad <= '1';
				currentState <=  pop3;

			when pop3 =>
				if (not ready = '1') then
					waitAck <= '1';
				else 
					waitAck <= '0';
					dataBusIn <= '0';
					regLoad <= '0';
					if (opcode(5 downto 4) = "11") then
						-- pop to A and flags
						a <= dataIn;
						flagsSign <= tempReg2(7);
						flagsZero <= tempReg2(6);
						flagsAuxCarry <= tempReg2(4);
						flagsParity <= tempReg2(2);
						flagsCarry <= tempReg2(0);
						currentState <= fetch0;								
					else
						case opcode(5 downto 4) is
							when regBC => regSelector <= "00000011";
							when regDE => regSelector <= "00001100";
							when others => regSelector <= "00110000"; -- regHL
						end case;
						regDataIn <= dataIn & tempReg2;
						regLoad <= '1';
						currentState <= pop4;
					end if;
				end if;
				
			when pop4 =>
				regLoad <= '0';
				currentState <= fetch0;

			when push0 =>
				tempReg1 <= regDataOut(15 downto 8);
				tempReg2 <= regDataOut(7 downto 0);
				regSelector <= "01000000";
				currentState <= push1;

			when push1 =>
				addressBus <= regDataOut - 1;
				dataOut <= tempReg1;
				wr <= '1';
				currentState <= push2;
				
			when push2 =>
				-- wait
				currentState <= push3;
				
			when push3 =>
				addressBus <= regDataOut - 2;
				dataOut <= tempReg2;
				currentState <= push4;
				
			when push4 =>
				-- wait and update SP
				regDataIn <= regDataOut - 2;
				regLoad <= '1';
				currentState <= push5;
				
			when push5 =>
				wr <= '0';
				regLoad <= '0';
				currentState <= fetch0;
			
			when pchl0 =>
				pc <= regDataOut;
				currentState <= fetch0;

			when sphl0 =>
				regDataIn <= regDataOut;
				regSelector <= "01000000";
				regLoad <= '1';
				currentState <= sphl1;
	
			when sphl1 =>
				regLoad <= '0';
				currentState <= fetch0;
				
			when ani0 =>
				aluOperandB <= tempReg1;
				currentState <= ani1;
				
			when ani1 => 
				a <= aluResult;
				currentState <= fetch0;
				
			when ori0 =>
				aluOperandB <= tempReg1;
				
			when ori1 => 
				a <= aluResult;
				currentState <= fetch0;
				
			when inout0 =>
				-- device num in tempReg1
				-- opcode(3) = 1 for in 0 for out
				-- todo read/send accumulator 
				currentState <= fetch0;
				
			when xchg0 => 
				tempReg1 <= regDataOut(15 downto 8);
				tempReg2 <= regDataOut(7 downto 0);
				regSelector <= "00110000"; -- regHL
				currentState <= xchg1;
				
			when xchg1 =>
				tempReg1 <= regDataOut(15 downto 8);
				tempReg2 <= regDataOut(7 downto 0);
				regDataIn <= tempReg1 & tempReg2;
				regLoad <= '1';
				currentState <= xchg2;

			when xchg2 => 
				regSelector <= "00001100"; -- regDE
				regDataIn <= tempReg1 & tempReg2;
				currentState <= xchg3;
			
			when xchg3 =>
				regLoad <= '0';
				currentState <= fetch0;

			when badopcode =>
				status <= "1110";

			when halted0 =>
				status <= "1111";

		end case;
	end if;
end process;

end Behavioral;

