library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity alu is 
Port ( 
	clk : in STD_LOGIC;
	operandA : in  STD_LOGIC_VECTOR (7 downto 0);
	operandB : in  STD_LOGIC_VECTOR (7 downto 0);
	operatorSelect : in  STD_LOGIC_VECTOR (2 downto 0);
	carryIn : in  STD_LOGIC;
	carryOut : out  STD_LOGIC;
	zeroOut : out  STD_LOGIC;
	signOut : out  STD_LOGIC;
	parityOut : out  STD_LOGIC;
	auxCarryOut : out  STD_LOGIC;
	result : out  STD_LOGIC_VECTOR (7 downto 0)
);
end alu;

architecture Behavioral of alu is

begin

procloop:
process (clk)

variable tempResult : std_logic_vector(8 downto 0);

begin

	--if (rising_edge(clk)) then -- not sure this needs to be clocked really
	if (falling_edge(clk)) then -- not sure this needs to be clocked really
		auxCarryOut <= '0';
		carryOut <= '0';
		if (operatorSelect(2 downto 1) = "00") then -- add / add with carry 
			tempResult := std_logic_vector(signed(operandA(7) & operandA) + signed(operandB(7) & operandB));
			--auxCarryOut <= (((operandA(3 downto 0) + operandB(3 downto 0)) >> 4 and 1
			if ((operatorSelect(0) and carryIn) = '1') then
				tempResult := std_logic_vector(signed(tempResult) + 1);
			end if;
			carryOut <= tempResult(8);
			result <= tempResult(7 downto 0);
			-- auxcar = (((opra[3:0]+oprb[3:0]) >> 4) & 1'b1) ? 1'b1 : 1'b0 ; 
			-- auxcar = (((opra[3:0]+oprb[3:0]+cin) >> 4) & 1'b1) ? 1'b1 : 1'b0;
			auxCarryOut <= '0';
		elsif (operatorSelect(2 downto 1) = "01") then -- sub / sub with borrow
			tempResult := std_logic_vector(signed(operandA(7) & operandA) - signed(operandB(7) & operandB));
			if ((operatorSelect(0) and carryIn) = '1') then
				tempResult := std_logic_vector(signed(tempResult) - 1);
			end if;
			carryOut <= tempResult(8);
			result <= tempResult(7 downto 0);
			auxCarryOut <= '0';
		elsif (operatorSelect = "100") then -- and
			tempResult := "0" & (operandA and operandB);
			result <= operandA and operandB;
		elsif (operatorSelect = "101") then -- xor
			tempResult := "0" & (operandA xor operandB);
			result <= operandA xor operandB;
		elsif (operatorSelect = "110") then -- or
			tempResult := "0" & (operandA or operandB);
			result <= operandA or operandB;
		else -- "111" -- compare 
			tempResult := std_logic_vector(signed(operandA(7) & operandA) - signed(operandB(7) & operandB));
			carryOut <= tempResult(8);
			result <= operandA; --tempResult(7 downto 0);
			auxCarryOut <= '0';
		end if;

		if (signed(tempResult) = 0) then
			zeroOut <= '1';
		else
			zeroOut <= '0';
		end if;
		
      signOut <= tempResult(7); 
      parityOut <= not (tempResult(7) xor tempResult(6) xor tempResult(5) xor tempResult(4) xor tempResult(3) xor tempResult(2) xor tempResult(1) xor tempResult(0)); 
	end if;

end process;

end Behavioral;

