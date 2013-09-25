library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

Library UNISIM;
use UNISIM.vcomponents.all;

entity spaceinvaders is port (
	clk : in  std_logic;
	
	-- video pins
	red : out  STD_LOGIC_VECTOR (3 downto 0);
	green : out  STD_LOGIC_VECTOR (3 downto 0);
	blue : out  STD_LOGIC_VECTOR (3 downto 0);
	hsync : out  STD_LOGIC;
	vsync : out  STD_LOGIC;
	
	-- reset pin
	reset : in std_logic;
	
	leds : out STD_LOGIC_VECTOR(3 downto 0));

end spaceinvaders;

architecture Behavioral of spaceinvaders is

signal clk10mhz : STD_LOGIC;
signal clk133mhz : STD_LOGIC;
signal clk133mhzinv : STD_LOGIC;
signal clk25mhz : STD_LOGIC;

signal addressBus : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
signal dataBusReady : STD_LOGIC := '0';

signal cpuInt : STD_LOGIC := '0';
signal cpuHold : STD_LOGIC := '0';
signal cpuDataIn : STD_LOGIC_VECTOR(7 downto 0);
signal cpuDataOut : STD_LOGIC_VECTOR(7 downto 0);
signal cpuDataBusInputMode : STD_LOGIC;
signal cpuStatus : std_logic_vector(3 downto 0);
signal cpuSync : std_logic;
signal cpuWaitAck : std_logic;
signal cpuWr : std_logic;
signal cpuHlda : std_logic;
signal cpuInte : std_logic;

signal romDataOut : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal romAddr : STD_LOGIC_VECTOR(12 DOWNTO 0) := (others => '0');

signal ramAddr : STD_LOGIC_VECTOR(9 DOWNTO 0) := (others => '0');
signal ramDataOut : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal ramDataIn : STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');
signal ramWea : STD_LOGIC_VECTOR(0 DOWNTO 0) := "0";

signal vramAddrA : STD_LOGIC_VECTOR(12 DOWNTO 0) := (others => '0');
signal vramDataOutA: STD_LOGIC_VECTOR(7 DOWNTO 0);
signal vramDataInA: STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');
signal vramAddrB : STD_LOGIC_VECTOR(12 DOWNTO 0) := (others => '0');
signal vramDataOutB : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal vramWea : STD_LOGIC_VECTOR(0 DOWNTO 0) := "0";

COMPONENT clocks PORT (
	clkin           : in   STD_LOGIC;
	clk10mhz        : out  STD_LOGIC;
	clk133mhz       : out  STD_LOGIC;
	clk133mhzinv    : out  STD_LOGIC;
	clk25mhz        : out  STD_LOGIC);
END COMPONENT;

COMPONENT video PORT (
	clkin25mhz   : in   STD_LOGIC;
	red          : out  STD_LOGIC_VECTOR (3 downto 0);
	green        : out  STD_LOGIC_VECTOR (3 downto 0);
	blue         : out  STD_LOGIC_VECTOR (3 downto 0);
	hsync        : out  STD_LOGIC;
	vsync        : out  STD_LOGIC;
	vramAddr     : out  STD_LOGIC_VECTOR (12 downto 0);
	vramData     : in   STD_LOGIC_VECTOR (7 downto 0);
	colourOutput : in   STD_LOGIC);
END COMPONENT;

COMPONENT vram PORT (
	clka   : IN  STD_LOGIC;
	wea    : IN  STD_LOGIC_VECTOR(0 DOWNTO 0);
	addra  : IN  STD_LOGIC_VECTOR(12 DOWNTO 0);
	dina   : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
	douta  : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
	clkb   : IN  STD_LOGIC;
	web    : IN  STD_LOGIC_VECTOR(0 DOWNTO 0);
	addrb  : IN  STD_LOGIC_VECTOR(12 DOWNTO 0);
	dinb   : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
	doutb  : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
END COMPONENT;

COMPONENT ram PORT (
	clka : IN STD_LOGIC;
	wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
	addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
	dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
	douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
END COMPONENT;

COMPONENT rom PORT (
	clka   : IN  STD_LOGIC;
	addra  : IN  STD_LOGIC_VECTOR(12 DOWNTO 0);
	douta  : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
END COMPONENT;

COMPONENT i8080 PORT (
	clk        : IN  std_logic;
	dataIn     : IN  std_logic_vector(7 downto 0);
	ready      : IN  std_logic;
	hold       : IN  std_logic;
	int        : IN  std_logic;
	reset      : IN  std_logic;          
	addressBus : OUT std_logic_vector(15 downto 0);
	dataOut    : OUT std_logic_vector(7 downto 0);
	sync       : OUT std_logic;
	dataBusIn  : OUT std_logic;
	waitAck    : OUT std_logic;
	wr         : OUT std_logic;
	hlda       : OUT std_logic;
	inte       : OUT std_logic;
	status     : OUT std_logic_vector(3 downto 0));
END COMPONENT;

type memControllerState is (idle, read0, read1, write0);

signal memState : memControllerState := idle;
signal vgaVsync : std_logic;

BEGIN

process (vgaVsync) 

begin

if (rising_edge(vgaVsync)) then
	
end if;

end process;

process (clk10mhz)

variable writing : std_logic := '0';
variable vramAddrAWide : STD_LOGIC_VECTOR(15 DOWNTO 0);
variable ramAddrWide : STD_LOGIC_VECTOR(15 DOWNTO 0);

begin

if (falling_edge(clk10mhz)) then
	case (memState) is 
		when idle =>
			vramWea <= "0";
			ramWea <= "0";
			if (addressBus >= X"2400") then
				vramAddrAWide := (addressBus - X"2400");
				vramAddrA <= vramAddrAWide(12 downto 0);
			elsif (addressBus >= X"2000") then
				ramAddrWide := (addressBus - X"2000");
				ramAddr <= ramAddrWide(9 downto 0);
			else
				romAddr <= addressBus(12 downto 0);
			end if;
			
			if (cpuDataBusInputMode = '1') then
				memState <= read0;
			elsif (cpuWr = '1') then
				memState <= write0;
			end if;
		when read0 =>
			dataBusReady <= '0';
			memState <= read1;
		when read1 =>
			if (addressBus < X"2000") then
				cpuDataIn <= romDataOut;
			elsif (addressBus < X"2400") then
				cpuDataIn <= ramDataOut;
			else
				cpuDataIn <= vramDataOutB;
			end if;
			dataBusReady <= '1';
			memState <= idle;
		when write0 =>
			if (addressBus >= X"2400") then
				vramDataInA <= cpuDataOut;
				vramWea <= "1";
			elsif (addressBus >= X"2000") then
				ramDataIn <= cpuDataOut;
				ramWea <= "1";
			else
				report "Attempt to write to ROM address!";
			end if;
			memState <= idle;
	end case;
end if;

end process;

leds <= cpuStatus;
vsync <= vgaVsync;

videoController : video PORT MAP (
	clkin25mhz => clk25mhz,
	red => red,
	green => green,
	blue => blue,
	hsync => hsync,
	vsync => vgaVsync,
	vramAddr => vramAddrB,
	vramData => vramDataOutB,
	colourOutput => '1');

allClocks : clocks PORT MAP (
	clkin => clk,
	clk10mhz => clk10mhz,
	clk133mhz => clk133mhz,
	clk133mhzinv => clk133mhzinv,
	clk25mhz => clk25mhz);

videoRam : vram PORT MAP (
	clka => clk10mhz,
	wea => vramWea,
	addra => vramAddrA,
	dina => vramDataInA,
	douta => vramDataOutA,
	clkb => clk25mhz,
	web => "0",
	addrb => vramAddrB,
	dinb => "00000000",
	doutb => vramDataOutB);

readWriteMemory : ram PORT MAP (
	clka => clk10mhz,
	wea => ramWea,
	addra => ramAddr,
	dina => ramDataIn,
	douta => ramDataOut);

invadersRom : rom PORT MAP (
	clka => clk10mhz,
	addra => romAddr,
	douta => romDataOut);
  
cpu: i8080 PORT MAP(
	clk => clk10mhz,
	dataIn => cpuDataIn,
	ready => dataBusReady,
	hold => cpuHold,
	int => cpuInt,
	reset => reset,
	addressBus => addressBus,
	dataOut => cpuDataOut,
	sync => cpuSync,
	dataBusIn => cpuDataBusInputMode,
	waitAck => cpuWaitAck,
	wr => cpuWr,
	hlda => cpuHlda,
	inte => cpuInte,
	status => cpuStatus);


END Behavioral;

