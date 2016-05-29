-- **********************************************************
-- *			8080 Microprocessor CPU Core				*
-- *========================================================*
-- * FPGA Project:		8080 CPU in Altera SOPC Builder	*
-- * File:				8080i.vhd							*
-- * Author:			C.H.Ting							*
-- * Description:		8080 CPU Block						*
-- *														*
-- * Revision History:										*
-- * Date		By Who		Modification					*
-- * 06/06/05	C.H. Ting	Convert EP24 to 32-bits.		*
-- * 06/10/05	Robyn King	Made compatible with Altera SOPC*
-- *							Builder.					*
-- * 06/27/05	C.H. Ting	Removed Line Drawing Engine.	*
-- * 07/27/05	Robyn King	Cleaned up code.				*
-- * 08/07/10	C.H. Ting	Return to eP32p					*
-- * 11/18/10	C.H. Ting	Port to LatticeXP2 Brevia Kit	*
-- * 02/29/12	 Chen-Hanson Ting Back to eP16			   	  *
-- * 02/20/16	 Chen-Hanson Ting 8080 core    			   	  *
-- * 02/25/16	 Simulation   			   	  *
-- * 03/08/16	 Simulation ok with z80.mem   			   *
-- * 03/23/16	 Chen-Hanson Ting Ep8080         		   	  *
-- **********************************************************
 
library ieee;  
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_misc.all;
use ieee.std_logic_unsigned.all;

entity ep8080 is 
	generic(width: integer := 8);
	port(
		-- input port
		clk:		in	std_logic;
		clr:		in	std_logic;
		data_i:		in	std_logic_vector(width-1 downto 0);
		read:		out	std_logic;
		write:		out	std_logic;
		addr:		out	std_logic_vector(15 downto 0);
		data_o:		out	std_logic_vector(width-1 downto 0)
  	);
end entity ep8080;

architecture behavioral of ep8080 is

--    signal cpu_addr_o : std_logic_vector(15 downto 0) ;
--    signal data_i : std_logic_vector(7 downto 0);
--    signal cpu_data_o : std_logic_vector(7 downto 0);
    signal cpu_write: std_logic;

	type   stack is
	array(31 downto 0) of std_logic_vector(15 downto 0);
	signal r_stack: stack;
	signal rp,rp1,rp_in: std_logic_vector(4 downto 0); 
	signal i,a,b,c,d,e,h,l,w,z,psw: std_logic_vector(width-1 downto 0);
	signal a_in,b_in,c_in,d_in,e_in,h_in,l_in,w_in,z_in,psw_in: std_logic_vector(width-1 downto 0);
	signal phase,next_phase: integer range 0 to 4;
	signal p: std_logic_vector(15 downto 0); 
	signal alu_out: std_logic_vector(width downto 0);
	signal source: std_logic_vector(width-1 downto 0);
	signal code: std_logic_vector(width-1 downto 0);
	signal alu_sel: std_logic_vector(3 downto 0);
	signal a_sel: std_logic_vector(2 downto 0);
	signal b_sel: std_logic_vector(1 downto 0);
	signal c_sel: std_logic_vector(1 downto 0);
	signal d_sel: std_logic_vector(1 downto 0);
	signal e_sel: std_logic_vector(1 downto 0);
	signal h_sel: std_logic_vector(1 downto 0);
	signal l_sel: std_logic_vector(1 downto 0);
	signal psw_sel: std_logic_vector(1 downto 0);
	signal source_sel: std_logic_vector(2 downto 0);	
	signal addr_sel: std_logic_vector(2 downto 0);	
	signal p_sel: std_logic_vector(1 downto 0);	
	signal x_sel: std_logic_vector(2 downto 0);
	signal xalu_sel: std_logic_vector(1 downto 0);
	signal r_sel: std_logic_vector(2 downto 0);
	signal rp_sel: std_logic_vector(1 downto 0);
	signal w_sel,z_sel,carry_sel: std_logic;
	signal condition,dest,sign,parity,zero,ac: std_logic;
	signal aload,bload,cload,dload,eload,hload,lload,wload,zload,rst: std_logic;
	signal pload,rload,r1load,iload,rpload: std_logic;
	signal pswload,wz_sel: std_logic;
	signal b_t,c_t,d_t,e_t,h_t,l_t,w_t,m_t: std_logic_vector(width-1 downto 0);
	signal carry_in: std_logic;
	signal p_in,r_in,x_in,wz_in: std_logic_vector(15 downto 0);
	signal x_out: std_logic_vector(16 downto 0);
	
-- multiplxers

-- alu_sel
	constant add_t: 	std_logic_vector :="0000";
	constant adc_t: 	std_logic_vector :="0001";
	constant sub_t: 	std_logic_vector :="0010";
	constant sbb_t: 	std_logic_vector :="0011";
	constant ana_t: 	std_logic_vector :="0100";
	constant xra_t: 	std_logic_vector :="0101";
	constant ora_t: 	std_logic_vector :="0110";
	constant cmp_t: 	std_logic_vector :="0111";
	constant rlc_t: 	std_logic_vector :="1000";
	constant rrc_t: 	std_logic_vector :="1001";
	constant ral_t: 	std_logic_vector :="1010";
	constant rar_t: 	std_logic_vector :="1011";
	constant source_t: 	std_logic_vector :="1100";
	constant cma_t: 	std_logic_vector :="1101";
	constant dcr_t: 	std_logic_vector :="1110";
	constant inr_t: 	std_logic_vector :="1111";
-- source and destination
	constant b_a: 		std_logic_vector :="000";
	constant c_a: 		std_logic_vector :="001";
	constant d_a: 		std_logic_vector :="010";
	constant e_a: 		std_logic_vector :="011";
	constant h_a: 		std_logic_vector :="100";
	constant l_a: 		std_logic_vector :="101";
	constant m_a: 		std_logic_vector :="110";
	constant a_a: 		std_logic_vector :="111";
-- conditional
	constant nz_c: 		std_logic_vector :="000";
	constant z_c: 		std_logic_vector :="001";
	constant nc_c: 		std_logic_vector :="010";
	constant c_c: 		std_logic_vector :="011";
	constant po_c: 		std_logic_vector :="100";
	constant pe_c: 		std_logic_vector :="101";
	constant m_c: 		std_logic_vector :="110";
	constant p_c: 		std_logic_vector :="111";
-- x_sel
	constant p_x: 		std_logic_vector :="000";
	constant wz_x: 		std_logic_vector :="001";
	constant b_x: 		std_logic_vector :="100";
	constant d_x: 		std_logic_vector :="101";
	constant h_x: 		std_logic_vector :="110";
	constant rp_x: 		std_logic_vector :="111";
-- xalu_sel
	constant inc_x: 	std_logic_vector :="00";
	constant dec_x: 	std_logic_vector :="01";
	constant dad_x: 	std_logic_vector :="10";
	constant xin_x: 	std_logic_vector :="11";
-- r_sel
	constant b_r: 		std_logic_vector :="000";
	constant d_r: 		std_logic_vector :="001";
	constant h_r: 		std_logic_vector :="010";
	constant a_r: 		std_logic_vector :="011";
	constant p_r: 		std_logic_vector :="100";
-- p_sel
	constant inc_p: 	std_logic_vector :="00";
	constant i_p: 		std_logic_vector :="01";
	constant r_p: 		std_logic_vector :="10";
	constant hl_p: 		std_logic_vector :="11";
-- addr_sel
	constant p_addr: 	std_logic_vector :="000";
	constant rp_addr: 	std_logic_vector :="001";
	constant bc_addr: 	std_logic_vector :="010";
	constant de_addr: 	std_logic_vector :="011";
	constant hl_addr: 	std_logic_vector :="100";
	constant wz_addr: 	std_logic_vector :="101";
	constant r_addr: 	std_logic_vector :="111";
-- a_in
	constant source_a: 	std_logic_vector :="000";
	constant alu_a: 	std_logic_vector :="001";
	constant shift_a: 	std_logic_vector :="010";
	constant com_a: 	std_logic_vector :="011";
	constant data_a:	std_logic_vector :="100";
	constant r_a:		std_logic_vector :="101";
-- b_in
	constant source_b: 	std_logic_vector :="00";
	constant xout_b: 	std_logic_vector :="01";
	constant r_b: 		std_logic_vector :="10";
-- c_in
	constant source_c: 	std_logic_vector :="00";
	constant xout_c: 	std_logic_vector :="01";
	constant r_c: 		std_logic_vector :="10";
-- d_in
	constant source_d: 	std_logic_vector :="00";
	constant xout_d: 	std_logic_vector :="01";
	constant r_d: 		std_logic_vector :="10";
	constant h_d: 		std_logic_vector :="11";
-- e_in
	constant source_e: 	std_logic_vector :="00";
	constant xout_e: 	std_logic_vector :="01";
	constant r_e: 		std_logic_vector :="10";
	constant l_e: 		std_logic_vector :="11";
-- h_in
	constant source_h: 	std_logic_vector :="00";
	constant xout_h: 	std_logic_vector :="01";
	constant r_h: 		std_logic_vector :="10";
	constant d_h: 		std_logic_vector :="11";
-- l_in
	constant source_l: 	std_logic_vector :="00";
	constant xout_l: 	std_logic_vector :="01";
	constant r_l: 		std_logic_vector :="10";
	constant e_l: 		std_logic_vector :="11";
-- psw_in
	constant flag_psw:	std_logic_vector :="00";
	constant stc_psw: 	std_logic_vector :="01";
	constant cmc_psw: 	std_logic_vector :="10";
	constant r_psw: 	std_logic_vector :="11";
-- rp_in
	constant inc_rp: 	std_logic_vector :="00";
	constant dec_rp: 	std_logic_vector :="01";
	constant l_rp: 		std_logic_vector :="10";
	constant z_rp: 		std_logic_vector :="11";
-- z_in
	constant data_z: 	std_logic :='0';
	constant inc_z: 	std_logic :='1';
-- w_in
	constant data_w: 	std_logic :='0';
	constant inc_w: 	std_logic :='1';
-- carry_in
	constant alu_carry:	std_logic :='0';
	constant x_carry: 	std_logic :='1';

begin
	write <= cpu_write;
	read <= '1';
	ac <= '0';
	data_o <= alu_out(7 downto 0);
	
	zero<= not(alu_out(7) or alu_out(6) or alu_out(5) or alu_out(4)
		or alu_out(3) or alu_out(2) or alu_out(1) or alu_out(0));
	sign<= alu_out(7);
	parity<= (alu_out(7) xor alu_out(6) xor alu_out(5) xor alu_out(4)
		xor alu_out(3) xor alu_out(2) xor alu_out(1) xor alu_out(0));
	
	with rp_sel select
	rp_in <= rp+1 when inc_rp,
		rp-1 when dec_rp,
		l(4 downto 0) when l_rp,
		z(4 downto 0) when others;
		
	with phase select
	code <= data_i when 0,
		i when others;

	with source_sel select
	source <= b when b_a,
		c when c_a,
		d when d_a,
		e when e_a,
		h when h_a,
		l when l_a,
		data_i when m_a,
		a when others;
		
	with alu_sel select
	alu_out <= 
		'0'&a + source when add_t,
		'0'&a + source + psw(0) when adc_t,
		'0'&a - source when sub_t,
		'0'&a - source - psw(0) when sbb_t,
		'0'&(a and source) when ana_t,
		'0'&(a or source) when ora_t,
		'0'&(a xor source) when xra_t,
		'0'&a - source when cmp_t,
		a(7 downto 0)&a(7) when rlc_t,
		a(0)&a(0)&a(7 downto 1) when rrc_t,
		a(7 downto 0)&psw(0) when ral_t,
		a(0)&psw(0)&a(7 downto 1) when rar_t,
		psw(0)&(not a)  when cma_t,
		'0'&source + 1  when inr_t,
		'0'&source - 1  when dcr_t,
		'0'&source when others;

	with code(5 downto 3) select
	condition <= not psw(6) when "000",
		psw(6) when "001",
		not psw(0) when "010",
		psw(0) when "011",
		psw(2) when "100",
		not psw(2) when "101",
		psw(7) when "110",
		not psw(7) when others;

	with p_sel select
	p_in <= (w&z) when i_p ,
		(r_stack(conv_integer(rp(4 downto 0)))) when r_p ,
		(h&l) when hl_p ,
		p+1 when others;
		
	with addr_sel select
	addr <= 
		(b&c) when bc_addr ,
		(d&e) when de_addr ,
		(h&l) when hl_addr ,
		(w&z) when wz_addr ,
		(r_stack(conv_integer(rp(4 downto 0)))) when rp_addr ,
		p when others;
		
	with r_sel select
	r_in <= b&c when b_r,
		d&e when d_r,
		h&l when h_r,
		a&psw when a_r,
		p when others;
		
	with x_sel select
	x_in <= w&z when wz_x,
		b&c when b_x ,
		d&e when d_x ,
		h&l when h_x ,
		"00000000000"&rp when rp_x,
		p when others;

	with xalu_sel select 
	x_out <= ('0'&x_in)+1 when inc_x ,
		('0'&x_in)-1 when dec_x ,
		('0'&x_in)+('0'&h&l) when dad_x ,
		('0'&x_in) when others;

	with a_sel select
	a_in <= source when source_a ,
		alu_out(7 downto 0) when alu_a ,
		not a when com_a,
		r_stack(conv_integer(rp(4 downto 0)))(15 downto 8) when r_a,
		data_i when others;

	with b_sel select
	b_in <= source when source_b ,
		x_out(15 downto 8) when xout_b ,
		r_stack(conv_integer(rp(4 downto 0)))(15 downto 8) when r_b,
		h when others;

	with c_sel select
	c_in <= source when source_c ,
		x_out(7 downto 0) when xout_c ,
		r_stack(conv_integer(rp(4 downto 0)))(7 downto 0) when r_c,
		l when others;

	with d_sel select
	d_in <= source when source_d ,
		x_out(15 downto 8) when xout_d ,
		r_stack(conv_integer(rp(4 downto 0)))(15 downto 8) when r_d,
		h when others;

	with e_sel select
	e_in <= source when source_e ,
		x_out(7 downto 0) when xout_e ,
		r_stack(conv_integer(rp(4 downto 0)))(7 downto 0) when r_e,
		l when others;

	with h_sel select
	h_in <= source when source_h ,
		x_out(15 downto 8) when xout_h ,
		r_stack(conv_integer(rp(4 downto 0)))(15 downto 8) when r_h,
		d when others;

	with l_sel select
	l_in <= source when source_l ,
		x_out(7 downto 0) when xout_l ,
		r_stack(conv_integer(rp(4 downto 0)))(7 downto 0) when r_l,
		e when others;

	with w_sel select
	w_in <= data_i when data_w ,
		x_out(15 downto 8) when others;

	with z_sel select
	z_in <= data_i when data_z ,
		x_out(7 downto 0) when others;

	with carry_sel select
	carry_in <= alu_out(8) when alu_carry,
		x_out(16) when others;

	with psw_sel select
	psw_in <= (sign&zero&'0'&ac&'0'&parity&'1'&carry_in) when flag_psw,
		psw(7 downto 1)&'1' when stc_psw,
		psw(7 downto 1)&(not psw(0)) when cmc_psw,
		r_stack(conv_integer(rp(4 downto 0)))(7 downto 0) when others;


-- sequential assignments, with phase and code
	decode: process(code,a,b,c,d,e,h,l,w,z,i,psw,phase,next_phase,condition)
	begin
		addr_sel<="000";
		p_sel<="00";
		x_sel<="000";
		xalu_sel<="00";
		alu_sel<="0000";
		a_sel<="000";
		b_sel<="00";
		c_sel<="00";
		d_sel<="00";
		e_sel<="00";
		h_sel<="00";
		l_sel<="00";
		psw_sel<="00";
		source_sel<="000";	
		p_sel<="00";	
		x_sel<="000";
		r_sel<="000";
		rp_sel<="00";
		w_sel<='0';
		z_sel<='0';
		carry_sel<='0';
		aload<='0';
		bload<='0';
		cload<='0';
		dload<='0';
		eload<='0';
		hload<='0';
		lload<='0';
		wload<='0';
		zload<='0';
		pload<='0'; 
		rpload<='0'; 
		rload<='0'; 
		r1load<='0'; 
		cpu_write<='0'; 
		read<='1';
		iload<='0';
		pswload<='0';
	case code(7 downto 6) is
		when "00" =>
			if code(5 downto 0) = "000000" then -- nop
				next_phase <= 0;
				p_sel <= inc_p;
				pload <= '1';
			elsif code(5 downto 0) = "110111" then -- stc
				next_phase <= 0;
				p_sel <= inc_p;
				pload <= '1';
				psw_sel <= "01";
				pswload <= '1';
			elsif code(5 downto 0) = "111111" then -- cmc
				next_phase <= 0;
				p_sel <= inc_p;
				pload <= '1';
				psw_sel <= "10";
				pswload <= '1';
			elsif code(5 downto 1) = "11010" then -- inr/dcr M
				if phase = 0 then
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';  
					iload <= '1';
				elsif phase = 1 then
					next_phase <= 2;
					addr_sel <= hl_addr;
					source_sel <= m_a;
					alu_sel <= source_t;
					aload <= '1';
				else
					next_phase <= 0;
					addr_sel <= hl_addr;
					source_sel <= a_a;
					if code(0) = '1' then
						alu_sel <= dcr_t;
					else
						alu_sel <= inr_t;
					end if;
					cpu_write <= '1';
				end if;
			elsif code(2 downto 1) = "10" then -- inr/dcr reg
				next_phase <= 0;
				p_sel <= inc_p;
				pload <= '1';
				source_sel <= code(5 downto 3);
				if code(0) = '1' then
					alu_sel <= dcr_t;
				else
					alu_sel <= inr_t;
				end if;
				case code(5 downto 3) is
					when "000" => 
						b_sel <= source_b;
						bload <= '1';
					when "001" => 
						c_sel <= source_c;
						cload <= '1';
					when "010" => 
						d_sel <= source_d;
						dload <= '1';
					when "011" => 
						e_sel <= source_e;
						eload <= '1';
					when "100" => 
						h_sel <= source_h;
						hload <= '1';
					when "101" => 
						l_sel <= source_l;
						lload <= '1';
					when others => 
						a_sel <= source_a;
						aload <= '1';
				end case;
			elsif code(2 downto 0) = "111" then -- rotate
				next_phase <= 0;
				alu_sel <= '1'&code(5 downto 3);
				a_sel <= alu_a;
				aload <= '1';
				p_sel <= inc_p;
				pload <= '1';
				psw_sel <= "00";
				pswload <= '1';
			elsif code(3 downto 0) = "0001" then -- lxi
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';
					iload <= '1';
				elsif phase = 1 then			-- save z
					next_phase <= 2;
					p_sel <= inc_p;
					pload <= '1';
					zload <= '1';
				elsif phase = 2 then			-- save w
					next_phase <= 3;
					p_sel <= inc_p;
					pload <= '1';
					wload <= '1';
				else 							-- load x
					next_phase <= 0;
					x_sel <= wz_x;
					xalu_sel <= xin_x;
					if code(5 downto 4) = "00" then
						b_sel <= xout_b;
						c_sel <= xout_c;
						bload <= '1';
						cload <= '1';
					elsif code(5 downto 4) = "01" then
						d_sel <= xout_d;
						e_sel <= xout_e;
						dload <= '1';
						eload <= '1';
					elsif code(5 downto 4) = "10" then
						h_sel <= xout_h;
						l_sel <= xout_l;
						hload <= '1';
						lload <= '1';
					else
						rp_sel <= z_rp;
						rpload <= '1';
					end if;
				end if;
			elsif code(5 downto 0) = "110010" then -- sta
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';
					iload <= '1';
				elsif phase = 1 then			-- save z
					next_phase <= 2;
					p_sel <= inc_p;
					pload <= '1';
					zload <= '1';
				elsif phase = 2 then			-- save w
					next_phase <= 3;
					p_sel <= inc_p;
					pload <= '1';
					wload <= '1';
				else 							-- store a
					next_phase <= 0;
					addr_sel <= wz_addr;
					source_sel <= a_a;
					alu_sel <= source_t;
					cpu_write <= '1';
				end if;
			elsif code(5 downto 0) = "111010" then -- lda
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';
					iload <= '1';
				elsif phase = 1 then			-- save z
					next_phase <= 2;
					p_sel <= inc_p;
					pload <= '1';
					zload <= '1';
				elsif phase = 2 then			-- save w
					next_phase <= 3;
					p_sel <= inc_p;
					pload <= '1';
					wload <= '1';
				else 							-- load a
					next_phase <= 0;
					addr_sel <= wz_addr;
					a_sel <= data_a;
					aload <= '1';
				end if;
			elsif code(5 downto 0) = "000010" then -- stax b
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';
					iload <= '1';
				else 							-- store a
					next_phase <= 0;
					addr_sel <= bc_addr;
					source_sel <= a_a;
					alu_sel <= source_t;
					cpu_write <= '1';
				end if;
			elsif code(5 downto 0) = "010010" then -- stax d
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';
					iload <= '1';
				else 							-- store a
					next_phase <= 0;
					addr_sel <= de_addr;
					source_sel <= a_a;
					alu_sel <= source_t;
					cpu_write <= '1';
				end if;
			elsif code(5 downto 0) = "001010" then -- ldax b
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';
					iload <= '1';
				else 							-- store a
					next_phase <= 0;
					addr_sel <= bc_addr;
					a_sel <= data_a;
					aload <= '1';
				end if;
			elsif code(5 downto 0) = "011010" then -- ldax d
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';
					iload <= '1';
				else 							-- store a
					next_phase <= 0;
					addr_sel <= de_addr;
					a_sel <= data_a;
					aload <= '1';
				end if;
			elsif code(5 downto 0) = "100010" then -- shld
				next_phase <= 1;
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';
					iload <= '1';
				elsif phase = 1 then			-- save z
					next_phase <= 2;
					p_sel <= inc_p;
					pload <= '1';
					zload <= '1';
				elsif phase = 2 then			-- save w
					next_phase <= 3;
					p_sel <= inc_p;
					pload <= '1';
					wload <= '1';
				elsif phase =3 then				-- store l
					next_phase <= 4;
					addr_sel <= wz_addr;
					x_sel <= wz_x;				-- inc wz
					xalu_sel <= inc_x;
					w_sel <= inc_w;
					wload <= '1';
					z_sel <= inc_z;
					zload <= '1';
					source_sel <= l_a;
					alu_sel <= source_t;
					cpu_write <= '1';
				else 							-- store h
					next_phase <= 0;
					addr_sel <= wz_addr;
					source_sel <= h_a;
					alu_sel <= source_t;
					cpu_write <= '1';
				end if;
			elsif code(5 downto 0) = "101010" then -- lhld
				next_phase <= 1;  
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					pload <= '1';
					iload <= '1';
				elsif phase = 1 then			-- save z
					next_phase <= 2;
					p_sel <= inc_p;
					pload <= '1';
					zload <= '1';
				elsif phase = 2 then			-- save w
					next_phase <= 3;
					p_sel <= inc_p;
					pload <= '1';
					wload <= '1';
				elsif phase = 3 then			-- load l
					next_phase <= 4;
					addr_sel <= wz_addr;
					x_sel <= wz_x;
					xalu_sel <= inc_x;			-- inc wz
					w_sel <= inc_w;
					wload <= '1';
					z_sel <= inc_z;
					zload <= '1';
					source_sel <= m_a;
					alu_sel <= source_t;
					l_sel <= source_l;			-- get data_i
					lload <= '1';
				else 							-- load h
					next_phase <= 0;
					addr_sel <= wz_addr;
					source_sel <= m_a;
					alu_sel <= source_t;
					h_sel <= source_h;			-- get data_i
					hload <= '1';
				end if;
			elsif code(2 downto 0) = "011" then -- inx, dcx
				next_phase <= 0;
				pload <= '1';
				p_sel <= inc_p;
				x_sel <= '1'&code(5 downto 4);
				if code(3) = '1' then
					xalu_sel <= dec_x;
					rp_sel <= dec_rp;
				else
					xalu_sel <= inc_x;
					rp_sel <= inc_rp;
				end if;
				if code(5 downto 4) = "00" then
					b_sel <= xout_b;
					c_sel <= xout_c;
					bload <= '1';
					cload <= '1';
				elsif code(5 downto 4) = "01" then
					d_sel <= xout_d;
					e_sel <= xout_e;
					dload <= '1';
					eload <= '1';
				elsif code(5 downto 4) = "10" then
					h_sel <= xout_h;
					l_sel <= xout_l;
					hload <= '1';
					lload <= '1';
				else
					rpload <= '1';
				end if;
			elsif code(3 downto 0) = "1001" then -- dad
				next_phase <= 0;
				pload <= '1';
				p_sel <= inc_p;
				x_sel <= '1'&code(5 downto 4);
				xalu_sel <= dad_x;
				h_sel <= xout_h;
				l_sel <= xout_l;
				hload <= '1';
				lload <= '1';
				carry_sel <= x_carry;
				pswload <= '1';
			elsif code(3 downto 0) = "110110" then -- mvi M
				if phase = 0 then
					next_phase <= 1;
					iload <= '1';
					p_sel<=inc_p;
					pload<='1';
				else
					next_phase <= 0;
					p_sel<=inc_p;
					pload<='1';
					addr_sel <= hl_addr;
					source_sel <= m_a;
					alu_sel <= source_t;
					cpu_write <= '1';
				end if;
			elsif code(2 downto 0) = "110" then -- mvi r
				if phase = 0 then
					next_phase <= 1;
					iload <= '1';
					p_sel<=inc_p;
					pload<='1';
				else
					next_phase <= 0;
					p_sel<=inc_p;
					pload<='1';
					source_sel <= m_a;
					case code(5 downto 3) is
						when "000" => 
							b_sel <= source_b;
							bload <= '1';
						when "001" => 
							c_sel <= source_c;
							cload <= '1';
						when "010" => 
							d_sel <= source_d;
							dload <= '1';
						when "011" => 
							e_sel <= source_e;
							eload <= '1';
						when "100" => 
							h_sel <= source_h;
							hload <= '1';
						when "101" => 
							l_sel <= source_l;
							lload <= '1';
						when others => 
							a_sel <= source_a;
							aload <= '1';
					end case;
				end if;
			else
			end if;
		when "01" => 
			if code(5 downto 0) = "110110" then -- halt, don't load p
				next_phase <= 0;
				pload <= '0';					-- 
			elsif code(5 downto 3) = "110" then -- mov reg to memory
				if phase = 0 then
					next_phase <= 1;
					iload <= '1';
					p_sel<=inc_p;
					pload<='1';
				else
					next_phase <= 0;
					addr_sel <= hl_addr;
					source_sel <= code(2 downto 0);
					alu_sel <= source_t;
					cpu_write <= '1';
				end if;
			elsif code(2 downto 0) = "110" then -- mov memory to reg
				if phase = 0 then
					next_phase <= 1;
					iload <= '1';
					p_sel<=inc_p;
					pload<='1';
				else
					next_phase <= 0;
					addr_sel <= hl_addr;
					source_sel <= m_a;
					case code(5 downto 3) is
						when "000" => 
							b_sel <= source_b;
							bload <= '1';
						when "001" => 
							c_sel <= source_c;
							cload <= '1';
						when "010" => 
							d_sel <= source_d;
							dload <= '1';
						when "011" => 
							e_sel <= source_e;
							eload <= '1';
						when "100" => 
							h_sel <= source_h;
							hload <= '1';
						when "101" => 
							l_sel <= source_l;
							lload <= '1';
						when others => 
							a_sel <= source_a;
							aload <= '1';
					end case;
				end if;
			else								-- mov reg to reg
				next_phase <= 0;
				p_sel<=inc_p;
				pload<='1';
				source_sel <= code(2 downto 0);
				case code(5 downto 3) is
					when "000" => 
						b_sel <= source_b;
						bload <= '1';
					when "001" => 
						c_sel <= source_c;
						cload <= '1';
					when "010" => 
						d_sel <= source_d;
						dload <= '1';
					when "011" => 
						e_sel <= source_e;
						eload <= '1';
					when "100" => 
						h_sel <= source_h;
						hload <= '1';
					when "101" => 
						l_sel <= source_l;
						lload <= '1';
					when others => 
						a_sel <= source_a;
						aload <= '1';
				end case;
			end if;
		when "10" => 							-- alu
			next_phase <= 0;  
			source_sel <= code(2 downto 0);
			alu_sel <= '0'&code(5 downto 3);
			a_sel <= alu_a ;
			if not(code(5 downto 3) = "111") then
				aload <= '1';
			end if;
			psw_sel <= flag_psw;
			pswload <= '1';
			p_sel<=inc_p;
			pload<='1';
		when others => --"11"
			if code(5 downto 0) = "101011" then 	-- xchg
				next_phase <= 0;
				p_sel<=inc_p;
				pload<='1';
				d_sel <= h_d;
				e_sel <= l_e;
				h_sel <= d_h;
				l_sel <= e_l;
				dload <= '1';
				eload <= '1';
				hload <= '1';
				lload <= '1';
			elsif code(5 downto 0) = "100011" then 	-- xthl
				next_phase <= 0;
				p_sel<=inc_p;
				pload<='1';
				h_sel <= r_h;
				l_sel <= r_l ;
				hload <= '1';
				lload <= '1';
				r_sel <= h_r;
				rload <= '1';
			elsif code(5 downto 0) = "111001" then 	-- sphl
				next_phase <= 0;
				p_sel<=inc_p;
				pload<='1';
				rp_sel <= l_rp;
				rpload <= '1';
			elsif code(5 downto 0) = "101001" then 	-- pchl
				next_phase <= 0;
				p_sel <= hl_p;
				pload <= '1';
			elsif code(5 downto 0) = "000011" then 	-- jmp
				pload <= '1';
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					iload <= '1';
				elsif phase = 1 then			-- save z
					next_phase <= 2;
					p_sel <= inc_p;
					zload <= '1';
				elsif phase = 2 then			-- save w
					next_phase <= 3;
					p_sel <= inc_p;
					wload <= '1';
				else 							-- jump
					next_phase <= 0;
					p_sel <= i_p;
					addr_sel <= wz_addr;
				end if;
			elsif code(5 downto 0) = "001101" then	-- call
				pload <= '1';
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					iload <= '1';
				elsif phase = 1 then			-- save z
					next_phase <= 2;
					p_sel <= inc_p;
					zload <= '1';
				elsif phase = 2 then			-- save w
					next_phase <= 3;
					p_sel <= inc_p;
					wload <= '1';
				else 							-- call
					next_phase <= 0;
					r_sel <= p_r;
					r1load <= '1';
					rpload <= '1';				-- push p
					rp_sel <= inc_rp;
					p_sel <= i_p;
					addr_sel <= wz_addr;
				end if;    
			elsif code(5 downto 0) = "001001" then	-- return
				next_phase <= 0;
				p_sel <= r_p;
				pload <= '1';
				rpload <= '1';
				rp_sel <= dec_rp;
				addr_sel <= r_addr;
			elsif code(2 downto 0) = "110" then	-- immediate op
				next_phase <= 0;
				a_sel <= alu_a;
				aload <= '1';
				p_sel <= inc_p;
				pload <= '1';
				pswload<= '1'; 
			elsif code(2 downto 0) = "010" then	-- c jump
				pload <= '1';
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					iload <= '1';
				elsif phase = 1 then			-- save z
					next_phase <= 2;
					p_sel <= inc_p;
					zload <= '1';
				elsif phase = 2 then			-- save w
					next_phase <= 3;
					p_sel <= inc_p;
					wload <= '1';
				else 							-- jump
					next_phase <= 0;
					if condition = '1' then
						p_sel <= i_p;
						addr_sel <= wz_addr;
					else pload <= '0';
					end if;
				end if;
			elsif code(2 downto 0) = "100" then	-- c call
				pload <= '1';
				if phase = 0 then				-- save i
					next_phase <= 1;
					p_sel <= inc_p;
					iload <= '1';
				elsif phase = 1 then			-- save z
					next_phase <= 2;
					p_sel <= inc_p;
					zload <= '1';
				elsif phase = 2 then			-- save w
					next_phase <= 3;
					p_sel <= inc_p;
					wload <= '1';
				else 							-- call
					next_phase <= 0;
					if condition = '1' then
						p_sel <= i_p;
						r_sel <= p_r;
						r1load <= '1';			-- push p
						rpload <= '1';
						rp_sel <= inc_rp;
						addr_sel <= wz_addr;
					else pload <= '0';
					end if;
				end if;
			elsif code(2 downto 0) = "000" then	-- c return
				next_phase <= 0;
				pload <= '1';
				if condition = '1' then
					p_sel <= r_p;
					rpload <= '1';
					rp_sel <= dec_rp;
					addr_sel <= r_addr;
				else p_sel <= inc_p;
				end if;
			elsif code(2 downto 0) = "101" then	-- push
				next_phase <= 0;
				p_sel <= inc_p;
				pload <= '1';
				r_sel <= '0'&code(5 downto 4);
				r1load <= '1';
				rp_sel <= inc_rp;
				rpload <= '1';
			elsif code(2 downto 0) = "001" then	-- pop
				next_phase <= 0;
				p_sel <= inc_p;
				pload <= '1';
				rp_sel <= dec_rp;
				rpload <= '1';
				case code(5 downto 4) is
				when "00" => 
					b_sel <= r_b;
					c_sel <= r_c;
					bload <= '1';
					cload <= '1';
				when "01" =>
					d_sel <= r_d;
					e_sel <= r_e;
					dload <= '1';
					eload <= '1';
				when "10" =>
					h_sel <= r_h;
					l_sel <= r_l;
					hload <= '1';
					lload <= '1';
				when others =>
					a_sel <= r_a;
					aload <= '1';
					psw_sel <= r_psw;
					pswload <= '1';
				end case;
			else
			end if;
		end case;
	end process decode;

-- finite state machine, processor control unit	
	sync: process(clk,clr)
	begin
		if clr='1' then -- master rst
			phase <= 0;
			p <= "0000000000000000";
			rp  <= "00000";
			rp1 <= "00001";
			a <= "00000001";
			b <= "00000010";			
			c <= "00000011";
			d <= "00000100";
			e <= "00000101";
			h <= "00000111";
			l <= "00001000";			
			w <= "00001001";
			z <= "00001010";
			i <= "11000011";
			psw <= (others => '0');
		elsif (clk'event and clk='1') then
			phase <= next_phase;
			if iload='1' then
				i <= data_i(width-1 downto 0);
			end if;
			if wload='1' then
				w <= w_in;
			end if;
			if zload='1' then
				z <= z_in;
			end if;
			if pload='1' then
				p <= p_in;
			end if;
			if aload='1' then
				a <= a_in;
			end if;
			if bload='1' then
				b <= b_in;
			end if;
			if cload='1' then
				c <= c_in;
			end if;
			if dload='1' then
				d <= d_in;
			end if;
			if eload='1' then
				e <= e_in;
			end if;
			if hload='1' then
				h <= h_in;
			end if;
			if lload='1' then
				l <= l_in;
			end if;
			if pswload='1' then
				psw <= psw_in;
			end if;
			if rpload='1' then
				rp <= rp_in;
				rp1 <= rp_in + 1;
			end if;
			if rload='1' then
				r_stack(conv_integer(rp(4 downto 0))) <= r_in;
			end if;
			if r1load='1' then
				r_stack(conv_integer(rp1(4 downto 0))) <= r_in;
			end if;
		end if;
	end process sync;

end behavioral;
