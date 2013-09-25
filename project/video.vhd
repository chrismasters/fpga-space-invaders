
-- Fairly simple VGA (640x480 @ 60hz) output for the machine. 
-- Connected to some memory so that it can get bytes in and then outputs the right RGB based
-- on the corresponding bit value in each byte. Fakes the cellophane overlays from the original 
-- if colourOutput is high.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity video is

port ( 
	clkin25mhz : in   STD_LOGIC;
	red        : out  STD_LOGIC_VECTOR (3 downto 0);
	green      : out  STD_LOGIC_VECTOR (3 downto 0);
	blue       : out  STD_LOGIC_VECTOR (3 downto 0);
	hsync      : out  STD_LOGIC;
	vsync      : out  STD_LOGIC;
	vramAddr   : out  STD_LOGIC_VECTOR (12 downto 0);
	vramData   : in   STD_LOGIC_VECTOR (7 downto 0);
	colourOutput : in STD_LOGIC
);

end video;

architecture Behavioral of video is

signal hcount : unsigned(9 downto 0) := (others => '0');
signal vcount : unsigned(9 downto 0) := (others => '0');
signal hindex : unsigned(7 downto 0) := (others => '0');
signal vindex : unsigned(7 downto 0) := "11111111";

begin

vramAddr <= std_logic_vector(hindex) & std_logic_vector(vindex(7 downto 3));

process (clkin25mhz)

begin
	if rising_edge(clkin25mhz) then
		if (hcount = 799) then
			hcount <= (others => '0');
			hindex <= (others => '0');
			if vcount = 524 then
				vcount <= (others => '0');
			else
				vcount <= vcount + 1;
				if (vcount > 111 and vcount < 368) then
					vindex <= vindex - 1;
				else
					vindex <= "11111111";
				end if;
			end if;
		else
			hcount <= hcount + 1;
		end if;
		if vcount >= 490 and vcount < 492 then
			vsync <= '0';
		else 
			vsync <= '1';
		end if;
		if hcount >= 656 and hcount < 752 then
			hsync <= '0';
		else
			hsync <= '1';
		end if;
		if (vcount > 111 and vcount < 368 and hcount > 207 and hcount < 432) then
			if (colourOutput = '1' and vramData(to_integer(unsigned(vindex(2 downto 0)))) = '1') then
				if ((vcount > 111+191 and vcount < 111+241)
					or (vcount >= 352 and hcount > 232 and hcount < 344)) then
					red<="0000";
					green<="1111";
					blue<="0000";
				elsif (vcount > 143 and vcount < 161) then -- was > 142
					red<="1111";
					green<="0000";
					blue<="0000";
				else
					red<="1111";
					green<="1111";
					blue<="1111";
				end if;
			else
				red <= "0000";
				green <= "0000";
				blue <= "0000";			
			end if;
			hindex <= hindex + 1;
		else
			red <= "0000";
			green <= "0000";
			blue <= "0000";			
		end if;
	end if;
end process;


end Behavioral;





