-- TestBench Template 

  LIBRARY ieee;
  USE ieee.std_logic_1164.ALL;
  USE ieee.numeric_std.ALL;

  ENTITY testbench IS
  END testbench;

  ARCHITECTURE behavior OF testbench IS 

  -- Component Declaration
 	COMPONENT spaceinvaders
	PORT(
		clk : IN std_logic;
		reset : IN std_logic;          
		red : OUT std_logic_vector(3 downto 0);
		green : OUT std_logic_vector(3 downto 0);
		blue : OUT std_logic_vector(3 downto 0);
		hsync : OUT std_logic;
		vsync : OUT std_logic;
		leds : OUT std_logic_vector(3 downto 0)
		);
	END COMPONENT;

   --Inputs
   signal clk : std_logic := '0';

 	--Outputs
   signal red : STD_LOGIC_VECTOR (3 downto 0);
   signal green : STD_LOGIC_VECTOR (3 downto 0);
   signal blue : STD_LOGIC_VECTOR (3 downto 0);
   signal hsync : std_logic;
   signal vsync : std_logic;
   signal reset : std_logic;
	
	signal leds : STD_LOGIC_VECTOR (3 downto 0);

   -- Clock period definitions
   constant clk_period : time := 31.25 ns;
   --constant clk_period : time := 10 ns;
 	--constant clk_period : time := 7.518ns;
   

  BEGIN

  -- Component Instantiation
  uut: spaceinvaders PORT MAP(
		clk => clk,
		red => red,
		green => green,
		blue => blue,
		hsync => hsync,
		vsync => vsync,
		reset => reset,
		leds => leds
	);

   -- Clock process definitions
   clk_process :process
   begin
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
   end process;
 

   -- Stimulus process
   stim_proc: process
   begin		
      -- hold reset state for 100 ns.
      wait for 100 ns;	

      wait for clk_period*10;

      wait;
   end process;

  END;
