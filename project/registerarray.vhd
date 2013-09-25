library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- selector encoding
-- 00000001 - B
-- 00000010 - C
-- 00000100 - D
-- 00001000 - E
-- 00010000 - H
-- 00100000 - L
-- 00000011 - BC
-- 00001100 - DE
-- 00110000 - HL
-- 01000000 - SP
-- 10000000 - PC

entity RegisterArray is
Port ( 
	clk : in  STD_LOGIC;
	selector : in  STD_LOGIC_VECTOR (7 downto 0);
	dataIn : in  STD_LOGIC_VECTOR (15 downto 0);
	dataOut : out  STD_LOGIC_VECTOR (15 downto 0);
	load : in  STD_LOGIC
);
end RegisterArray;

architecture Behavioral of RegisterArray is
COMPONENT OneByteRegister
PORT(
	clk : IN std_logic;
	load : IN std_logic;
	dataIn : IN std_logic_vector(7 downto 0);          
	dataOut : OUT std_logic_vector(7 downto 0)
);
END COMPONENT;

COMPONENT DoubleByteRegister
PORT(
	clk : IN std_logic;
	load : IN std_logic;
	dataIn : IN std_logic_vector(15 downto 0);
	dataOut : OUT std_logic_vector(15 downto 0)
);
END COMPONENT;

signal bIn : std_logic_vector(7 downto 0);
signal cIn : std_logic_vector(7 downto 0);
signal dIn : std_logic_vector(7 downto 0);
signal eIn : std_logic_vector(7 downto 0);
signal hIn : std_logic_vector(7 downto 0);
signal lIn : std_logic_vector(7 downto 0);
signal spIn : std_logic_vector(15 downto 0);
--signal pcIn : std_logic_vector(15 downto 0);

signal bOut : std_logic_vector(7 downto 0);
signal cOut : std_logic_vector(7 downto 0);
signal dOut : std_logic_vector(7 downto 0);
signal eOut : std_logic_vector(7 downto 0);
signal hOut : std_logic_vector(7 downto 0);
signal lOut : std_logic_vector(7 downto 0);
signal spOut : std_logic_vector(15 downto 0);

begin

dataOut <= 
	"00000000" & bOut when selector = "00000001" else
	"00000000" & cOut when selector = "00000010" else
	"00000000" & dOut when selector = "00000100" else
	"00000000" & eOut when selector = "00001000" else
	"00000000" & hOut when selector = "00010000" else
	"00000000" & lOut when selector = "00100000" else
	bOut & cOut when selector = "00000011" else
	dOut & eOut when selector = "00001100" else
	hOut & lOut when selector = "00110000" else
	spOut when selector = "01000000" else
	--pcOut when selector = "10000000" else
	(others => '0');
	
bIn <= dataIn(15 downto 8) when selector(0) = '1' and selector(1) = '1' else dataIn(7 downto 0);
cIn <= dataIn(7 downto 0);
dIn <= dataIn(15 downto 8) when selector(2) = '1' and selector(3) = '1' else dataIn(7 downto 0);
eIn <= dataIn(7 downto 0);
hIn <= dataIn(15 downto 8) when selector(4) = '1' and selector(5) = '1' else dataIn(7 downto 0);
lIn <= dataIn(7 downto 0);

b: OneByteRegister PORT MAP(clk => clk, load => load and selector(0), dataIn => bIn, dataOut => bOut);
c: OneByteRegister PORT MAP(clk => clk, load => load and selector(1), dataIn => cIn, dataOut => cOut);
d: OneByteRegister PORT MAP(clk => clk, load => load and selector(2), dataIn => dIn, dataOut => dOut);
e: OneByteRegister PORT MAP(clk => clk, load => load and selector(3), dataIn => eIn, dataOut => eOut);
h: OneByteRegister PORT MAP(clk => clk, load => load and selector(4), dataIn => hIn, dataOut => hOut);
l: OneByteRegister PORT MAP(clk => clk, load => load and selector(5), dataIn => lIn, dataOut => lOut);
sp: DoubleByteRegister PORT MAP(clk => clk, load => load and selector(6), dataIn => dataIn, dataOut => spOut);
--pc: DoubleByteRegister PORT MAP(clk => clk, load => load(7), dataIn => dataIn, dataOut => pcOut);

end Behavioral;

