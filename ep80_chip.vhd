-- ************************************************************
-- *		 (C) Copyright 2012, Offete Enterprises, Inc.	  *
-- *					 ALL RIGHTS RESERVED				  *
-- *==========================================================*
-- * Project:			 FG in PROASIC					   	  *
-- * File:				 ep16_chip.vhd						  *
-- * Author:			 Chien-Chia Wu						  *
-- * Description:		 Top level block					  *
-- *														  *
-- * Hierarchy:parent:										  *
-- *		   child :										  *  
-- *														  *
-- * Revision History:										  *
-- * Date		 By Who		  Modification					  *
-- * 09/19/02	 Chien-Chia Wu   Branch from ep16a.			  *
-- * 01/02/03	 Chien-Chia Wu   Add SDI.					  *
-- * 01/29/03	 Chien-Chia Wu   Add Boot.					  *
-- * 02/24/03	 Chien-Chia Wu   Modify the module as 32-bits * 
-- *							  version.					  * 
-- * 02/27/03	 Chien-Chia Wu   Modify SDRAM byte-assecable  *
-- * 03/02/03	 Chien-Chia Wu   Add internal SRAM module.    *
-- * 06/29/06	 Chen-Hanson Ting Add HMPP/Shifter/Controller *
-- * 11/18/10	 Chen-Hanson Ting LatticeXP2 Brevia Kit   	  *
-- * 02/29/12	 Chen-Hanson Ting Back to eP16			   	  *
-- * 03/23/16	 Chen-Hanson Ting Ep8080         		   	  *
-- ************************************************************

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_misc.all;
use ieee.std_logic_unsigned.all;--use work.pmi_components.all;
--library pmi_work;

entity ep8080_chip is 
port(
	-- input port
	aclk:			in		std_logic; 
	arst:			in		std_logic;
--	interrupt_i:	in		std_logic_vector(4 downto 0);
	uart_i:			in		std_logic;
	-- output port
	uart_o:			out		std_logic;
	-- GPIO Interface
	ioport_0:			inout 	std_logic;
	ioport_1:			inout 	std_logic;
	ioport_2:			inout 	std_logic;
	ioport_3:			inout 	std_logic;
	ioport_4:			inout 	std_logic;
	ioport_5:			inout 	std_logic;
	ioport_6:			inout 	std_logic;
	ioport_7:			inout 	std_logic
	);
end entity ep8080_chip;


architecture behavioral of ep8080_chip is
  -- component declaration
  component ep8080 is 
	port(
		-- input port
		clk:		in		std_logic;
		clr:		in		std_logic;
		data_i:		in		std_logic_vector(7 downto 0);
		read:		out		std_logic;
		write:		out		std_logic;
		addr:		out		std_logic_vector(15 downto 0);
		data_o:		out		std_logic_vector(7 downto 0)
	);
  end component;

    component ram_memory
        port (Clock: in std_logic; ClockEn: in std_logic; 
        Reset: in std_logic; WE: in std_logic; Address0: in std_logic; 
        Address1: in std_logic; Address2: in std_logic; 
        Address3: in std_logic; Address4: in std_logic; 
        Address5: in std_logic; Address6: in std_logic; 
        Address7: in std_logic; Address8: in std_logic; 
        Address9: in std_logic; Address10: in std_logic; 
        Address11: in std_logic; Address12: in std_logic; 
        Data0: in std_logic; Data1: in std_logic; Data2: in std_logic; 
        Data3: in std_logic; Data4: in std_logic; Data5: in std_logic; 
        Data6: in std_logic; Data7: in std_logic; Q0: out std_logic; 
        Q1: out std_logic; Q2: out std_logic; Q3: out std_logic; 
        Q4: out std_logic; Q5: out std_logic; Q6: out std_logic; 
        Q7: out std_logic
    );
    end component;

  component uart is
	port(
		-- input
		clk_i:		in		std_logic;
		rst_i:		in		std_logic;
		ce_i:		in		std_logic;
		read_i:		in		std_logic;
		write_i:	in		std_logic;
		addr_i:		in		std_logic_vector(1 downto 0);
		data_i:		in		std_logic_vector(7 downto 0);
		-- output   	
		data_o:		out		std_logic_vector(7 downto 0);
		-- external interface
		rxd_i:		in		std_logic;
		txd_o:		out		std_logic
	);
  end component;  

component gpio
	port(
		-- input port
		clr: 		in		std_logic;
		clk: 		in		std_logic;
		write: 		in		std_logic;
		read: 		in		std_logic;
		ce: 		in		std_logic;
		addr: 		in		std_logic_vector(1 downto 0);
		data_in: 	in		std_logic_vector(7 downto 0);
		gpio_in: 	in		std_logic_vector(7 downto 0); 
		-- output port
		data_out: 	out		std_logic_vector(7 downto 0);
		gpio_out: 	out		std_logic_vector(7 downto 0);
		gpio_dir: 	out		std_logic_vector(7 downto 0)
	);
end component;

-- interal globle signal
	signal m_rst:			std_logic;
	signal m_clk:			std_logic;
	signal bclk:			std_logic;
	signal b_counter:		integer range 0 to 2;
	signal memory_data_o:	std_logic_vector(7 downto 0);
	signal memory_data_i:	std_logic_vector(7 downto 0);
	signal memory_addr:		std_logic_vector(12 downto 0);

-- internal signal for system bus
	signal system_addr:		std_logic_vector(15 downto 0);
	signal system_data_o:	std_logic_vector(7 downto 0);
	signal system_read:		std_logic;
	signal system_write:	std_logic;
  
-- internal signal for cpu
	signal cpu_data_i:		std_logic_vector(7 downto 0);
	signal cpu_addr_o:		std_logic_vector(15 downto 0);
	signal cpu_data_o:		std_logic_vector(7 downto 0);
	signal cpu_m_read:		std_logic;
	signal cpu_m_write:		std_logic;
 
-- internal signal for uart
	signal uart_ce:			std_logic; 
	signal uart_addr:		std_logic_vector(1 downto 0);
	signal uart_data_i:		std_logic_vector(7 downto 0);
	signal uart_data_o:		std_logic_vector(7 downto 0);
	signal uart_rxd:		std_logic;
	signal uart_txd:		std_logic;

-- internal signal for gpio
	signal gpio_ce:			std_logic;
	signal gpio_addr:		std_logic_vector(1 downto 0);
	signal gpio_data_i:		std_logic_vector(7 downto 0);
	signal gpio_in:			std_logic_vector(7 downto 0); 
	signal gpio_data_o:		std_logic_vector(7 downto 0);
	signal gpio_out:		std_logic_vector(7 downto 0);
	signal gpio_dir:		std_logic_vector(7 downto 0);

-- ram mmory
    signal Clock: std_logic;
    signal ClockEn: std_logic;
    signal Reset: std_logic;
    signal WE: std_logic;
    signal Address : std_logic_vector(12 downto 0) ;
    signal Data : std_logic_vector(7 downto 0);
    signal Q : std_logic_vector(7 downto 0);

begin
-- ************************************************************   
--			Component Binding
-- ************************************************************   
-- ========================= CPU Block ========================	
	cpu1: ep8080
		port map (
		-- input port
		clk => bclk,
		clr => m_rst,
		data_i => cpu_data_i,
		read => cpu_m_read,
		write => cpu_m_write,
		addr => cpu_addr_o,
		data_o => cpu_data_o
		);

-- ************************************************************   
--			Internal Globle Signal Circuit
-- ************************************************************   

	m_rst <= not arst;
	m_clk <= not bclk;
	system_addr <= cpu_addr_o;
	system_read <= cpu_m_read;
	system_write <= cpu_m_write;
	cpu_data_i <= system_data_o;
	system_data_o <=  cpu_data_o when (system_write='1')
		else 
		memory_data_o when(system_addr(15)='0')
		else
		uart_data_o when (system_addr(15 downto 2)="11111111000000")
		else
		gpio_data_o when (system_addr(15 downto 2)="11111111000001")
		else (others => 'Z');


-- ========================= RAM Block ========================	
ram_memory_0 : ram_memory
        port map (Clock => Clock, ClockEn => ClockEn, Reset => Reset, WE => WE, 
            Address0 => Address(0), Address1 => Address(1), 
            Address2 => Address(2), Address3 => Address(3), 
            Address4 => Address(4), Address5 => Address(5), 
            Address6 => Address(6), Address7 => Address(7), 
            Address8 => Address(8), Address9 => Address(9), 
            Address10 => Address(10), Address11 => Address(11), 
            Address12 => Address(12), Data0 => Data(0), Data1 => Data(1), 
            Data2 => Data(2), Data3 => Data(3), Data4 => Data(4), 
            Data5 => Data(5), Data6 => Data(6), Data7 => Data(7), 
            Q0 => Q(0), Q1 => Q(1), Q2 => Q(2), Q3 => Q(3), Q4 => Q(4), 
            Q5 => Q(5), Q6 => Q(6), Q7 => Q(7)
        );

  Address <= cpu_addr_o(12 downto 0);
  Data <= cpu_data_o ;
  memory_data_o <= Q ;
  Clock <= m_clk;
  ClockEn <= '1';
  Reset <= '0';
  WE <= cpu_m_write;
  
  -- ========================= UART Block =======================	
  uart1 : uart
	port map (
		-- input
		clk_i => bclk,
		rst_i => m_rst,
		ce_i => uart_ce,
		read_i => system_read,
		write_i => system_write,
		addr_i => uart_addr,
		data_i => uart_data_i,
		-- output
		data_o => uart_data_o,
		-- external interface
		rxd_i => uart_rxd,
		txd_o => uart_txd
	);
	uart_ce <= '1' when (system_addr(15 downto 2)="11111111000000")
  		else '0';
	uart_addr <= system_addr(1 downto 0);
	uart_data_i <= system_data_o;
	uart_rxd <= uart_i;
	uart_o <= uart_txd;
 
-- ========================= GPIO Block =======================	
  gpio1 : gpio
   port map (
		-- input port
		clk => bclk,
		clr => m_rst,
		write => system_write,
		read => system_read,
		ce => gpio_ce,
		addr => gpio_addr,
		data_in => gpio_data_i,
		gpio_in => gpio_in, 
		-- output port
		data_out => gpio_data_o,
		gpio_out => gpio_out,
		gpio_dir => gpio_dir
	);
	gpio_ce <= '1' when (system_addr(15 downto 2)="11111111000001")
		else '0';
	gpio_addr <= system_addr(1 downto 0); 
	gpio_data_i <= system_data_o;
	gpio_in <= ioport_7&ioport_6&ioport_5&ioport_4&ioport_3&ioport_2&ioport_1&ioport_0;
	ioport_0  <= gpio_out(0)  when gpio_dir(0)='1'  else 'Z';
	ioport_1  <= gpio_out(1)  when gpio_dir(1)='1'  else 'Z';
	ioport_2  <= gpio_out(2)  when gpio_dir(2)='1'  else 'Z';
	ioport_3  <= gpio_out(3)  when gpio_dir(3)='1'  else 'Z';
	ioport_4  <= gpio_out(4)  when gpio_dir(4)='1'  else 'Z';
	ioport_5  <= gpio_out(5)  when gpio_dir(5)='1'  else 'Z';
	ioport_6  <= gpio_out(6)  when gpio_dir(6)='1'  else 'Z';
	ioport_7  <= gpio_out(7)  when gpio_dir(7)='1'  else 'Z';

-- **********************************************************	
--		Reduce clock rate 
-- **********************************************************	
	clocking:
	process(m_rst, aclk)
	begin
	if (m_rst='1') then
		bclk <= '1';
--		b_counter <= 0;
	elsif ( aclk'event and aclk='1') then
		--if b_counter = 2 then
			bclk <= not bclk;
			--b_counter <= 0;
		--else b_counter <= b_counter + 1 ;
		--end if;
	end if;  
	end process clocking;

  end behavioral;
