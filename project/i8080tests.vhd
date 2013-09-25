
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY i8080tests IS
END i8080tests;
 
ARCHITECTURE behavior OF i8080tests IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT i8080
    PORT(
         clk : IN  std_logic;
         addressBus : OUT  std_logic_vector(15 downto 0);
         dataIn : IN  std_logic_vector(7 downto 0);
         dataOut : OUT  std_logic_vector(7 downto 0);
         sync : OUT  std_logic;
         dataBusIn : OUT  std_logic;
         ready : IN  std_logic;
         waitAck : OUT  std_logic;
         wr : OUT  std_logic;
         hold : IN  std_logic;
         hlda : OUT  std_logic;
         inte : OUT  std_logic;
         int : IN  std_logic;
         reset : IN  std_logic;
			status : out STD_LOGIC_VECTOR(3 downto 0)
        );
    END COMPONENT;
    

   --Inputs
   signal clk : std_logic := '0';
   signal dataIn : std_logic_vector(7 downto 0) := (others => '0');
   signal ready : std_logic := '0';
   signal hold : std_logic := '0';
   signal int : std_logic := '0';
   signal reset : std_logic := '0';

 	--Outputs
   signal addressBus : std_logic_vector(15 downto 0);
   signal dataOut : std_logic_vector(7 downto 0);
   signal sync : std_logic;
   signal dataBusIn : std_logic;
   signal waitAck : std_logic;
   signal wr : std_logic;
   signal hlda : std_logic;
   signal inte : std_logic;
	signal status : STD_LOGIC_VECTOR(3 downto 0);
   signal wea: STD_LOGIC_VECTOR(0 downto 0);

   -- Clock period definitions
   --constant clk_period : time := 10 ns;
 	constant clk_period : time := 7.518ns;

COMPONENT testmem
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;

BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: i8080 PORT MAP (
          clk => clk,
          addressBus => addressBus,
          dataIn => dataIn,
          dataOut => dataOut,
          sync => sync,
          dataBusIn => dataBusIn,
          ready => ready,
          waitAck => waitAck,
          wr => wr,
          hold => hold,
          hlda => hlda,
          inte => inte,
          int => int,
          reset => reset,
			 status => status
        );

	mem : testmem
	  PORT MAP (
		 clka => clk,
		 wea(0) => wr,
		 addra => addressBus(5 downto 0),
		 dina => dataOut,
		 douta => dataIn
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

      reset <= '1';
		wait for clk_period;
		reset <= '0';
		assert addressBus = "0000000000000000" report "address bus expected 0";
		assert dataBusIn <= '1' report "dataBusIn expected 1";
		dataIn <= X"76"; -- HALT;
		ready <= '1';
		wait for clk_period*3;
		ready <= '0';
		assert status = "1111" report "status expected halted";
		reset <= '1';
		wait for clk_period;
		reset <= '0';
		
      wait;
   end process;

END;
