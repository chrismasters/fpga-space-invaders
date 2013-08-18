library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity OneByteRegister is
Port ( 
	clk : in  STD_LOGIC;
	load : in  STD_LOGIC;
	dataIn : in  STD_LOGIC_VECTOR (7 downto 0);
	dataOut : out  STD_LOGIC_VECTOR (7 downto 0)
);

end OneByteRegister;

architecture Behavioral of OneByteRegister is

begin

loadproc:
process (clk)
begin
	if (rising_edge(clk)) then
		if (load = '1') then
			dataOut <= dataIn;
		end if;
	end if;
end process loadproc;

end Behavioral;

