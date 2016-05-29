;       80eF 2.02, C. H. Ting, 02/15/06
;	ep8080, recreate 8080 proceesor, based on ep16
;	Combine ZEF80 with 86eforth to test 8080 core
;	Assembling 80ef202 with ep8080.bat
;		c:\masm615\bin\ml /Fl 80EF202.asm >80EF202.err 
;		c:\masm615\bin\link 80EF202.obj
;	Covert 80ef202.exe to .mem file used inDiammond IDE
;		copy 80ef202.exe to \F#\F#MIDI
;		Execute COMtoMEM.FEX
;		MIDI @ 200 + 160 COMdump
;		Copy mem dumo to \ep8080x\ep8080.mem
;	Synthesize ram_memory with IPexpress
;		Select EBR component/RAMDQ
;		Enter file name ram_memoery, VHDL output
;		Select 8192 bytes, no output latch
;		Select Address Hex type, enter ep8080.mem file
;		Select None for Bus Order Type 
;	Add ep8080.lpf to File List/LPF Constraint Files
;		Define physical pin outs for ep80_chip
;	Synthesize ep8080 modules in Diamond IDE
;		ep80_chip.vhd
;		ep80.vhd
;		ram_memory.vhd
;		uart80.vhd
;		gpio80.vhd
;	Select Export Files/VHDL Simulation File
;		Click Process/Run All
;	Select Simulation Wizard to simulate
;		Select 10 MHz for aclk
;		Select 1,0ns,0,200ns for arst
;		Run 10ms to see message output on uart_o
;	Select Export Files/JEDEC File 
;		Click Process/Run All
;	Select Programming to burn FPGA
;	Bring up HyperTerminal
;		Set up 57600 baud, 8 data bits, 1 stop bit, no flow
;		Press reset of Brevia2 kit
;		eP8080 v2.2 will sign on