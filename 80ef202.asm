TITLE 80eF202
PAGE 62,132	;62 lines per page, 132 characters per line
;===============================================================
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
;
;===============================================================
;       86eForth 2.02, C. H. Ting, 06/03/99
;       A sample session looks like:
;	       c>86ef202
;	       DOWNLOAD LESSONS.TXT
;	       WORDS
;	       ' THEORY 'BOOT !
;	       UPLOAD TEST.EXE
;	       BYE
;	       c>test
;
;       86eForth 2.01, C. H. Ting, 05/24/99
;       Merge Zen2.asm with eForth 1.12
;1.     Eliminate most of the @EXECUTE thru user variables
;2.     Combine name and code dictionary
;3.     Eliminate code pointer fields
;4.     elimiate catch-throw
;5.     eliminate most user variables
;6.     extend top memory to FFF0H where the stacks and user area are.
;7.     add open, close, read, write; improve BYE
;8      add 1+, 1-, 2/
;
;       
;       eForth 1.12, C. H. Ting, 03/30/99
;	       Change READ and LOAD to 'read' and 'load'.
;	       Make LOAD to read and compile a file.  The file
;	       buffer is from CP+1000 to NP-100.
;	       To load all the lessons, type:
;	       LOAD LESSONS.TXT
;	       and you can test all the examples in this file.
;       eForth 1.11, C. H. Ting, 03/25/99
;	       Change BYE to use function 4CH of INT 21H.
;	       Add read, write, open, close, READ, and LOAD
;	       To read a text file into memory:
;	       HEX 2000 1000 READ TEST.TXT
;	       READ returns the number of byte actually read.
;	       To compile the source code in the text file:
;	       2000 FCD LOAD
;	       where FCD is the length returned by READ.
;	       These additions allow code for other eForth systems
;	       to be tested on PC first.
;	       It is part of the Firmware Engineering Workshop.
;
;
;	eForth 1.0 by Bill Muench and C. H. Ting, 1990
;	Much of the code is derived from the following sources:
;	8086 figForth by Thomas Newman, 1981 and Joe smith, 1983
;	aFORTH by John Rible
;	bFORTH by Bill Muench
;
;	The goal of this implementation is to provide a simple eForth Model
;	which can be ported easily to many 8, 16, 24 and 32 bit CPU's.
;	The following attributes make it suitable for CPU's of the '90:
;
;	small machine dependent kernel and portable high level code
;	source code in the MASM format
;	direct threaded code
;	separated code and name dictionaries
;	simple vectored terminal and file interface to host computer
;	aligned with the proposed ANS Forth Standard
;	easy upgrade path to optimize for specific CPU
;
;	You are invited to implement this Model on your favorite CPU and
;	contribute it to the eForth Library for public use. You may use
;	a portable implementation to advertise more sophisticated and
;	optimized version for commercial purposes. However, you are
;	expected to implement the Model faithfully. The eForth Working
;	Group reserves the right to reject implementation which deviates
;	significantly from this Model.
;
;	As the ANS Forth Standard is still evolving, this Model will
;	change accordingly. Implementations must state clearly the
;	version number of the Model being tracked.
;
;	Representing the eForth Working Group in the Silicon Valley FIG Chapter.
;	Send contributions to:
;
;	Dr. C. H. Ting
;	156 14th Avenue
;	San Mateo, CA 94402
;	(650) 571-7639
;
;===============================================================
;; Version control
VER	     EQU     2	       ;major release version
EXT	     EQU     3	       ;minor extension
;; Constants
TRUEE	EQU	-1	;true flag
COMPO	EQU	040H	;lexicon compile only bit
IMEDD	EQU	080H	;lexicon immediate bit
MASKK	EQU	07F1FH	;lexicon bit mask
CELLL	EQU	2	;size of a cell
BASEE	EQU	10H	;default radix
VOCSS	EQU	8	;depth of vocabulary stack
BKSPP	EQU	8	;back space
LF	EQU	10	;line feed
CRR	EQU	13	;carriage return
ERR	EQU	27	;error escape
TIC	EQU	39	;tick
CALLL	EQU	0CDH	;NOP CALL opcodes
;; Memory allocation
EM	EQU     01FFFH	  ;top of memory
US	EQU	15*CELLL	;user area size in cells
RTS	EQU	64*CELLL	;return stack/TIB size
UPP	EQU     1FE0H	 ;start of user area (UP0)
RPP	EQU     1F80H	 ;start of return stack (RP0)
TIBB	EQU     1F90H	 ;terminal input buffer (TIB)
SPP	EQU     0H	    ;start of data stack (SP0)
COLDD	EQU     0	   ;cold start vector
;; Initialize assembly variables
_LINK	= 0	;force a null link
_USER   = 0	     ;first user variable offset
;; Define assembly macros
;	Compile a code definition header.
$CODE	MACRO	LEX,NAME,LABEL
	DW      _LINK	    ;;token pointer and link
	_LINK	= $	;;link points to a name string
	DB	LEX,NAME	;;name string
LABEL:	;;assembly label
	ENDM
;	Compile a colon definition header.
$COLON	MACRO	LEX,NAME,LABEL
	$CODE	LEX,NAME,LABEL
	DB	CALLL
	DW	DOLST	;;include CALL doLIST
	ENDM
;	Compile a user variable header.
$USER	MACRO	LEX,NAME,LABEL
	$CODE	LEX,NAME,LABEL
	DB	CALLL
	DW	DOLST	;;include CALL doLIST
	DW	DOUSE,_USER	;;followed by doUSER and offset
	_USER = _USER+CELLL	;;update user area offset
	ENDM
;	Assemble inline direct threaded code ending.
$NEXT	MACRO
	DB	0C3H	\;;read the next code address into AX
	DW	NextStep	;;jump directly to the code address
	ENDM
;; Main entry points and COLD start data
MAIN	SEGMENT
	ASSUME	CS:MAIN,DS:MAIN,ES:MAIN,SS:MAIN
ORIG:	
; COLD start moves the following to USER variables.
; MUST BE IN SAME ORDER AS USER VARIABLES.
ORG	COLDD	;beginning of cold boot area
	DB	21h, 080h,1Fh	 ; LD HL, 007Ch ; init RP to 1FFEh
	DB	22h, 0FEh,1Fh    ; LD (RP), HL ;
	DB	0C3H
	DW	COLD
UZERO:
	DW	BASEE	;BASE
	DW	0	;tmp
	DW	0	;>IN
	DW	10	;#TIB
	DW	TIBB	;TIB
	DW	INTER	;'EVAL
	DW	0	;HLD
	DW	0	;CONTEXT pointer
	DW	CTOP	;CP
	DW      LASTN	;LAST
	DW	1F80H	;RP at 94h
	DW	0	;SP
ULAST:
	  
;; Device dependent I/O
;       All channeled to DOS 21H services
;;ORG	0100H
PUSHDE:	
	DB	0D5H
PUSHHL:
	DB	0E5H
;;   NextStep
;The Forth Inner Interpreter--IP (=BC)is pointing the To-Be-Exec one
NextStep: ;The Forth Inner Interpreter--IP (=BC)is pointing the To-Be-Exec one
	DB    0Ah	      ;       LD  A, (BC); 7t
	DB    03h	      ;       INC BC     ; 6t
	DB    6Fh	      ;       LD  L, A   ; 4t
	DB    0Ah	      ;       LD  A, (BC); 7t
	DB    03h	      ;       INC BC     ; 6t
	DB    67h	      ;       LD  H, A   ; 4t
	DB   0E9h	      ;       JP  (HL)   ; 4t
	       ;	  ; 38t==(10MHz)3.8 usec.
;   ?RX	( -- c T | F )
;	Return input character and true, or a false if no input.
	$COLON   4,'?KEY',QKEY
	DW      DOLIT,0FF02H,CAT,DUPP   ; UART80 RX C@
	DW      QBRAN,RX1	     
	DW      DOLIT,0FF03H,CAT,SWAP	; UART80 RX C!
RX1: 
	DW	EXIT     ; $1A PC! ;
;   TX!	( c -- )
;	Send character c to the output device.
	$COLON  4,'EMIT',EMIT 
TX1: 
	DW      DOLIT,0FF01H,CAT	  ; UART80 TX C@
	DW      QBRAN,TX1	     ; UNTIL
	DW      DOLIT,0FF01H,CSTOR,EXIT     ; UART80 TX C! ;
;; The kernel
;   doLIT	( -- w )
;	Push an inline literal.
	$CODE   COMPO+5,'doLit',DOLIT
	DB    0Ah	      ;       LD  A, (BC)  ; 7t
	DB    03h	      ;       INC BC       ; 6t
	DB    6Fh	      ;       LD  L,A      ; 4t
	DB    0Ah	      ;       LD  A,(BC)   ; 7t
	DB    03h	      ;       INC BC       ; 6t
	DB    67h	      ;       LD  H,A      ; 4t
	DB   0E5h	      ;       PUSH HL      ;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t    
;   doLIST	( a -- )
;	Process colon list.
	$CODE   COMPO+6,'doList',DOLST
	DB    2Ah, 0FEh,1Fh    ;       LD  HL, (RP) ;16t
	DB    2Bh	      ;       DEC HL       ; 6t
	DB    70h	      ;       LD  (HL), B  ; 7t      \ end of r-push simulation
	DB    2Bh	      ;       DEC HL       ; 6t
	DB    71h	      ;       LD  (HL), C  ; 7t      \ end of r-push simulation
	DB    22h, 0FEh,1Fh    ;       LD  (RP), HL ;16t      \ IP is r-pushed (simulated)
	       ;	    ;
	DB   0C1h	      ;       POP BC       ;10t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	      ;99t==9.9usec (+ 2.4usec for NextStep)
;   next	( -- )
;	Run time code for the single index loop.
;	: next ( -- ) \ hilevel model
;	  r> r> dup if 1 - >r @ >r exit then drop cell+ >r ;
	$CODE   COMPO+4,'next',DONXT
	DB   2Ah, 0FEh,1Fh     ;       LD  HL, (RP) ;16t
	DB   7Eh	       ;       LD  A, (HL)  ; 7t
	DB  0B7h	       ;       OR  A	; 4t
	DB  0C2h	 ;       JR  NZ, DECLOW;12/7t a fast dec is ok, only failed every 255 time
	DW  DECLOW	       ;	    ; low byte 0
	DB   23h	       ;       INC HL       ; 6t
	DB   7Eh	       ;       LD  A, (HL)  ; 7t
	DB  0B7h	       ;       OR  A	; 4t
	DB  0C2h	  ;       JR  NZ, DECHILO;12/7t Hi-byte no-zero, it is also a re-loop case
	DW  DECHILOW	       ;zero bound now .. .
	DB   23h	       ;       INC HL       ; 6tdiscard the loop count on R-stack
	DB   22h, 0FEh,1Fh     ;       LD  (RP), HL ;16t
	DB   03h	       ;       INC BC       ; 6t\ IP slip over the re-loop-addr
	DB   03h	       ;       INC BC       ; 6t
	DB  0C3h
	DW  NextStep	   ;       JP  NextStep ;10t loop is over
DECHILOW:	       ;	    ; 98t==(10MHz)9.8usec
	DB   35h	      ;DECHILO:DEC (HL)     ;11t hi-byte
	DB   2Bh	       ;       DEC HL       ; 6t back to low byte
DECLOW:
	DB   35h	       ;DECLOW:DEC (HL)     ;11t low byte non-zero, just dec it and re-loop
	DB   69h	       ;       LD  L, C     ; 4t get loop-start-adr to IP and keep stepping
	DB   60h	       ;       LD  H, B     ; 4t
	DB   4Eh	       ;       LD  C, (HL)  ; 7t
	DB   23h	       ;       INC HL       ; 6t
	DB   46h	       ;       LD  B, (HL)  ; 7t
	DB  0C3h
	DW  NextStep	   ;       JP  NextStep ;10t
	      ; low byte dec:    88t==(10MHz)8.8usec
	      ; lo&Hi byte dec: 134t==(10MHz)13.4usec
;   ?branch	( f -- )
;	Branch if flag is zero.
	$CODE   COMPO+7,'?branch',QBRAN
	DB   0E1h	      ;       POP HL	;10t
	DB    7Dh	      ;       LD  A, L      ; 4t    ?branch adr is just after ?branch
	DB   0B4h	      ;       OR  H	 ; 4t    and IP is pointing it
	DB    0CAh	 ;       JR  Z,ZEROO   ; 12/7t (Z=1,12t)
	DW  BRAN
	DB    03h	      ;       INC BC	; 6t    IP slip over the retun addr
	DB    03h	      ;       INC BC	; 6t    ex: 'TRUE IF' will slip over
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep  ;10t  ;47t==(10MHz)4.7usec
	       ;
;   branch	( -- )
;	Branch to an inline address.
	$CODE   COMPO+6,'branch',BRAN
	DB    69h	      ;       LD  L, C      ; 4t   get IP :=[IP] and go
	DB    60h	      ;       LD  H, B      ; 4t
	DB    4Eh	      ;       LD  C, (HL)   ; 7t
	DB    23h	      ;       INC HL	; 6t
	DB    46h	      ;       LD  B, (HL)   ; 7t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep  ;10t
	       ;	     ;38t==(10MHz)3.8usec
;   EXECUTE	( ca -- )
;	Execute the word at ca.
	$CODE	7,'EXECUTE',EXECU
	DB   0E1h	      ;       POP HL	;10t
	DB   0E9h	      ;       JP  (HL)      ; 4t
;   EXIT	( -- )
;	Terminate a colon definition.
	$CODE	4,'EXIT',EXIT
	DB    2Ah, 0FEh,1Fh    ;       LD  HL, (RP) ;16t
	DB    4Eh	      ;       LD  C, (HL)  ; 7t
	DB    23h	      ;       INC HL       ; 6t
	DB    46h	      ;       LD  B, (HL)  ; 7t
	DB    23h	      ;       INC HL       ; 6t
	DB    22h, 0FEh,1Fh    ;       LD  (RP),HL  ;16t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;68t==6.8us
;   !	( w a -- )
;	Pop the data stack to memory.
	$CODE	1,'!',STORE
	DB   0E1h	      ;       POP HL       ;10t
	DB   0D1h	      ;       POP DE       ;10t
	DB    73h	      ;       LD  (HL), E  ; 7t
	DB    23h	      ;       INC HL       ; 6t
	DB    72h	      ;       LD  (HL), D  ; 7t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;50t==(10Mhz)5.0 usec
;   @	( a -- w )
;	Push memory location to the data stack.
	$CODE	1,'@',AT
	DB   0E1h	      ;       POP HL       ;10t
	DB    5Eh	      ;       LD  E, (HL)  ; 7t
	DB    23h	      ;       INC HL       ; 6t
	DB    56h	      ;       LD  D, (HL)  ; 7t
	DB   0D5h	      ;       PUSH DE      ;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;51t==(10Mhz)5.1usec
;   C!	( c b -- )
;	Pop the data stack to byte memory.
	$CODE	2,'C!',CSTOR
	DB   0E1h	      ;       POP HL	;10t
	DB   0D1h	      ;       POP DE	;10t
	DB    73h	      ;       LD  (HL), E   ; 7t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep  ;10t
	       ;	     ;37t==(10Mhz)3.7usec
;   C@	( b -- c )
;	Push byte memory location to the data stack.
	$CODE	2,'C@',CAT
	DB   0E1h	      ;       POP HL       ;10t
	DB    5Eh	      ;       LD  E, (HL)  ; 7t
	DB    16h, 00h	 ;       LD  D, 0     ; 7t
	DB   0D5h	      ;       PUSH DE      ;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;45t==(10Mhz)4.5usec
;   R>	( -- w )
;	Pop the return stack to the data stack.
	$CODE	COMPO+2,'R>',RFROM
	DB    2Ah, 0FEh,1Fh    ;       LD  HL, (RP) ;16t
	DB    5Eh	      ;       LD  E, (HL)  ; 7t
	DB    23h	      ;       INC HL       ; 6t
	DB    56h	      ;       LD  D, (HL)  ; 7t
	DB    23h	      ;       INC HL       ; 6t
	DB    22h, 0FEh,1Fh    ;       LD  (RP), HL ;16t
	DB   0D5h	      ;       PUSH DE      ;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;67t==(10MHz)5.7.usec
;   R@	( -- w )
;	Copy top of return stack to the data stack.
	$CODE	2,'R@',RAT
	DB    2Ah, 0FEh,1Fh    ;       LD  HL, (RP) ;16t
	DB    5Eh	      ;       LD  E, (HL)  ; 7t
	DB    23h	      ;       INC HL       ; 6t
	DB    56h	      ;       LD  D, (HL)  ; 7t
	DB   0D5h	      ;       PUSH DE      ;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;57t==(10MHz)5.7usec
;   >R	( w -- )
;	Push the data stack to the return stack.
	$CODE	COMPO+2,'>R',TOR
	DB   0D1h	      ;       POP DE       ;10t
	DB    2Ah, 0FEh,1Fh    ;       LD  HL, (RP) ;16t
	DB    2Bh	      ;       DEC HL       ; 6t
	DB    72h	      ;       LD  (HL), D  ; 7t
	DB    2Bh	      ;       DEC HL       ; 6t
	DB    73h	      ;       LD  (HL), E  ; 7t
	DB    22h, 0FEh,1Fh    ;       LD  (RP), HL ;16t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;78t==(10MHz)7.8usec
;   RP@	 ( -- a )
;	       Push the current RP to the data stack.
	$CODE   3,'RP@',RPAT
	DB    2Ah, 0FEh,1Fh    ;       LD  HL, (RP)
	DB   0E5h	      ;       PUSH HL
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep
;   RP!	 ( a -- )
;	       Set the return stack pointer.
	$CODE   COMPO+3,'RP!',RPSTO
	DB   0E1h	      ;       POP HL       ;
	DB   22h, 0FEh,1Fh    ;       LD  (RP), HL ;
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;
;   SP@	( -- a )
;	Push the current data stack pointer.
	$CODE   3,'sp@',SPAT
	DB    21h, 00h, 00h    ;       LD  HL, 0
	DB    39h	      ;       ADD HL, SP
	DB   0E5h	      ;       PUSH HL
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep
;   SP!	( a -- )
;	Set the data stack pointer.
	$CODE   3,'sp!',SPSTO
	DB   0E1h	      ;       POP HL
	DB   0F9h	      ;       LD  SP, HL
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep
;   DROP	( w -- )
;	Discard top stack item.
	$CODE	4,'DROP',DROP
	DB   0E1h	      ;       POP HL       ;10t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;20t==(10MHz)2.0usec
;   DUP	( w -- w w )
;	Duplicate the top stack item.
	$CODE	3,'DUP',DUPP
	DB   0E1h	      ;       POP HL       ;10t
	DB   0E5h	      ;       PUSH HL      ;11t
	DB   0E5h	      ;       PUSH HL      ;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;42t==(10MHz)4.2usec
;   SWAP	( w1 w2 -- w2 w1 )
;	Exchange top two stack items.
	$CODE	4,'SWAP',SWAP
	DB   0E1h	      ;       POP HL       ;10t
	DB   0E3h	      ;       EX  (SP), HL ;19t
	DB   0E5h	      ;       PUSH HL      ;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;50t==(10MHz)5.0usec
;   OVER	( w1 w2 -- w1 w2 w1 )
;	Copy second stack item to top.
	$CODE	4,'OVER',OVER
	DB   0D1h	      ;       POP DE       ;10t
	DB   0E1h	      ;       POP HL       ;10t
	DB   0E5h	      ;       PUSH HL      ;11t
	DB   0D5h	      ;       PUSH DE      ;11t
	DB   0E5h	      ;       PUSH HL      ;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep ;10t
	       ;	    ;63t==(10MHz)6.3usec
;   0<	( n -- t )
;	Return true if n is negative.
	$CODE	2,'0<',ZLESS
	DB   0E1h	      ;       POP HL	 ;10t
	DB    29h	      ;       ADD HL, HL     ;11t
	DB   0DAh	 ;       JR  C, LESSZ   ;12/7t
	DW  LESSZ
	DB    21h, 00h, 00h    ;       LD  HL, 0      ;10t
	DB   0C3h
	DW   PUSHHL	  ;       JP  NextStep   ;10t   59t==(10MHz)5.9usec
LESSZ:	       ;
	DB    21h,0FFh,0FFh    ;LESSZ: LD  HL, 0FFFFH ;10t
	DB   0C3h
	DW   PUSHHL	 ;       JP  NextStep   ;10t   64t==(10MHz)6.4usec
;   AND	( w w -- w )
;	Bitwise AND.
	$CODE	3,'AND',ANDD
	DB   0D1h	      ;       POP DE
	DB   0E1h	      ;       POP HL
	DB    7Bh	      ;       LD  A, E
	DB   0A5h	      ;       AND L
	DB    6Fh	      ;       LD  L, A
	DB    7Ah	      ;       LD  A, D
	DB   0A4h	      ;       AND H
	DB    67h	      ;       LD  H, A
	DB   0E5h	      ;       PUSH HL
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep
;   OR	( w w -- w )
;	Bitwise inclusive OR.
	$CODE	2,'OR',ORR
	DB   0D1h	      ;       POP DE
	DB   0E1h	      ;       POP HL
	DB    7Bh	      ;       LD  A, E
	DB   0B5h	      ;       OR  L
	DB    6Fh	      ;       LD  L, A
	DB    7Ah	      ;       LD  A, D
	DB   0B4h	      ;       OR  H
	DB    67h	      ;       LD  H, A
	DB   0E5h	      ;       PUSH HL
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep
;   XOR	( w w -- w )
;	Bitwise exclusive OR.
	$CODE	3,'XOR',XORR
	DB   0D1h	      ;       POP DE
	DB   0E1h	      ;       POP HL
	DB    7Bh	      ;       LD  A, E
	DB   0ADh	      ;       XOR L
	DB    6Fh	      ;       LD  L, A
	DB    7Ah	      ;       LD  A, D
	DB   0ACh	      ;       XOR H
	DB    67h	      ;       LD  H, A
	DB   0E5h	      ;       PUSH HL
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep
;   UM+	( u u -- udsum )
;	Add two unsigned single numbers and return a double sum.
	$CODE   3,'UM+',UPLUS
	DB   0D1h	      ;       POP DE	 ;10t
	DB   0E1h	      ;       POP HL	 ;10t
	DB    19h	      ;       ADD HL, DE     ;11t
	DB   0E5h	      ;       PUSH HL	;11t
	DB    0DAh	 ;       JR  C, CARRY   ;12/7t
	DW   CARRY
	DB    21h, 00h, 00h    ;       LD  HL, 0      ;10t
	DB   0E5h	      ;       PUSH HL	;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep   ;10t
CARRY:	       ;
	DB    21h, 01h, 00h    ;CARRY: LD  HL, 1      ;10t
	DB   0E5h	      ;       PUSH HL	;11t
	DB   0C3h
	DW   NextStep	  ;       JP  NextStep   ;10t
;; System and user variables
;   doVAR	( -- a )
;	Run time routine for VARIABLE and CREATE.
	$COLON  COMPO+5,'doVar',DOVAR
	DW	RFROM,EXIT
;   UP	( -- a )
;	Pointer to the user area.
	$COLON  2,'up',UP
	DW	DOVAR
	DW	UPP
;   doUSER	( -- a )
;	Run time routine for user variables.
	$COLON  COMPO+6,'doUser',DOUSE
	DW	RFROM,AT,UP,AT,PLUS,EXIT
;   BASE	( -- a )
;	Storage of the radix base for numeric I/O.
	$USER	4,'BASE',BASE
;   tmp	( -- a )
;	A temporary storage location used in parse and find.
	$USER   COMPO+3,'tmp',TEMP
;   >IN	( -- a )
;	Hold the character pointer while parsing input stream.
	$USER	3,'>IN',INN
;   #TIB	( -- a )
;	Hold the current count in and address of the terminal input buffer.
	$USER	4,'#TIB',NTIB
	_USER = _USER+CELLL	;hold the base address of the terminal input buffer
;   'EVAL	( -- a )
;	Execution vector of EVAL.
	$USER   5,"'eval",TEVAL
;   HLD	( -- a )
;	Hold a pointer in building a numeric output string.
	$USER   3,'hld',HLD
;   CONTEXT	( -- a )
;	A area to specify vocabulary search order.
	$USER   7,'CONTEXT',CNTXT
;   CP	( -- a )
;	Point to the top of the code dictionary.
	$USER   2,'cp',CP
;   LAST	( -- a )
;	Point to the last name in the name dictionary.
	$USER   4,'last',LAST
;; Common functions
;   ?DUP	( w -- w w | 0 )
;	Dup tos if its is not zero.
	$COLON	4,'?DUP',QDUP
	DW	DUPP
	DW	QBRAN,QDUP1
	DW	DUPP
QDUP1:
	DW	EXIT
;   ROT	( w1 w2 w3 -- w2 w3 w1 )
;	Rot 3rd item to top.
	$COLON	3,'ROT',ROT
	DW	TOR,SWAP,RFROM,SWAP,EXIT
;   2DROP	( w w -- )
;	Discard two items on stack.
	$COLON	5,'2DROP',DDROP
	DW	DROP,DROP,EXIT
;   2DUP	( w1 w2 -- w1 w2 w1 w2 )
;	Duplicate top two items.
	$COLON	4,'2DUP',DDUP
	DW	OVER,OVER,EXIT
;   +	( w w -- sum )
;	Add top two items.
	$COLON	1,'+',PLUS
	DW	UPLUS,DROP,EXIT
;   NOT	( w -- w )
;	One's complement of tos.
	$COLON	3,'NOT',INVER
	DW	DOLIT,-1,XORR,EXIT
;   NEGATE	( n -- -n )
;	Two's complement of tos.
	$COLON	6,'NEGATE',NEGAT
	DW      INVER,ONEP,EXIT
;   DNEGATE	( d -- -d )
;	Two's complement of top double.
	$COLON	7,'DNEGATE',DNEGA
	DW	INVER,TOR,INVER
	DW	DOLIT,1,UPLUS
	DW	RFROM,PLUS,EXIT
;   -	( n1 n2 -- n1-n2 )
;	Subtraction.
	$COLON	1,'-',SUBBB
	DW	NEGAT,PLUS,EXIT
;   ABS	( n -- n )
;	Return the absolute value of n.
	$COLON	3,'ABS',ABSS
	DW	DUPP,ZLESS
	DW	QBRAN,ABS1
	DW	NEGAT
ABS1:
	DW	EXIT
;   =	( w w -- t )
;	Return true if top two are equal.
	$COLON	1,'=',EQUAL
	DW	XORR
	DW	QBRAN,EQU1
	DW	DOLIT,0,EXIT
EQU1:
	DW	DOLIT,TRUEE,EXIT
;   U<	( u u -- t )
;	Unsigned compare of top two items.
	$COLON	2,'U<',ULESS
	DW	DDUP,XORR,ZLESS
	DW	QBRAN,ULES1
	DW	SWAP,DROP,ZLESS,EXIT
ULES1:
	DW	SUBBB,ZLESS,EXIT
;   <	( n1 n2 -- t )
;	Signed compare of top two items.
	$COLON	1,'<',LESS
	DW	DDUP,XORR,ZLESS
	DW	QBRAN,LESS1
	DW	DROP,ZLESS,EXIT
LESS1:
	DW	SUBBB,ZLESS,EXIT
;   MAX	( n n -- n )
;	Return the greater of two top stack items.
	$COLON	3,'MAX',MAX
	DW	DDUP,LESS
	DW	QBRAN,MAX1
	DW	SWAP
MAX1:
	DW	DROP,EXIT
;   MIN	( n n -- n )
;	Return the smaller of top two stack items.
	$COLON	3,'MIN',MIN
	DW	DDUP,SWAP,LESS
	DW	QBRAN,MIN1
	DW	SWAP
MIN1:
	DW	DROP,EXIT
;   WITHIN	( u ul uh -- t )
;	Return true if u is within the range of ul and uh. ( ul <= u < uh )
	$COLON	6,'WITHIN',WITHI
	DW	OVER,SUBBB,TOR
	DW	SUBBB,RFROM,ULESS,EXIT
;; Divide
;   UM/MOD	( udl udh un -- ur uq )
;	Unsigned divide of a double by a single. Return mod and quotient.
	$COLON	6,'UM/MOD',UMMOD
	DW	DDUP,ULESS
	DW	QBRAN,UMM4
	DW	NEGAT,DOLIT,15,TOR
UMM1:
	DW	TOR,DUPP,UPLUS
	DW	TOR,TOR,DUPP,UPLUS
	DW	RFROM,PLUS,DUPP
	DW	RFROM,RAT,SWAP,TOR
	DW	UPLUS,RFROM,ORR
	DW	QBRAN,UMM2
	DW      TOR,DROP,ONEP,RFROM
	DW	BRAN,UMM3
UMM2:
	DW	DROP
UMM3:
	DW	RFROM
	DW	DONXT,UMM1
	DW	DROP,SWAP,EXIT
UMM4:
	DW	DROP,DDROP
	DW	DOLIT,-1,DUPP,EXIT
;   M/MOD	( d n -- r q )
;	Signed floored divide of double by single. Return mod and quotient.
	$COLON	5,'M/MOD',MSMOD
	DW	DUPP,ZLESS,DUPP,TOR
	DW	QBRAN,MMOD1
	DW	NEGAT,TOR,DNEGA,RFROM
MMOD1:
	DW	TOR,DUPP,ZLESS
	DW	QBRAN,MMOD2
	DW	RAT,PLUS
MMOD2:
	DW	RFROM,UMMOD,RFROM
	DW	QBRAN,MMOD3
	DW	SWAP,NEGAT,SWAP
MMOD3:
	DW	EXIT
;   /MOD	( n n -- r q )
;	Signed divide. Return mod and quotient.
	$COLON	4,'/MOD',SLMOD
	DW	OVER,ZLESS,SWAP,MSMOD,EXIT
;   MOD	( n n -- r )
;	Signed divide. Return mod only.
	$COLON	3,'MOD',MODD
	DW	SLMOD,DROP,EXIT
;   /	( n n -- q )
;	Signed divide. Return quotient only.
	$COLON	1,'/',SLASH
	DW	SLMOD,SWAP,DROP,EXIT
;; Multiply
;   UM*	( u u -- ud )
;	Unsigned multiply. Return double product.
	$COLON	3,'UM*',UMSTA
	DW	DOLIT,0,SWAP,DOLIT,15,TOR
UMST1:
	DW	DUPP,UPLUS,TOR,TOR
	DW	DUPP,UPLUS,RFROM,PLUS,RFROM
	DW	QBRAN,UMST2
	
	DW	TOR,OVER,UPLUS,RFROM,PLUS
UMST2:
	DW	DONXT,UMST1
	DW	ROT,DROP,EXIT
;   *	( n n -- n )
;	Signed multiply. Return single product.
	$COLON	1,'*',STAR
	DW	UMSTA,DROP,EXIT
;   M*	( n n -- d )
;	Signed multiply. Return double product.
	$COLON	2,'M*',MSTAR
	DW	DDUP,XORR,ZLESS,TOR
	DW	ABSS,SWAP,ABSS,UMSTA
	DW	RFROM
	DW	QBRAN,MSTA1
	DW	DNEGA
MSTA1:
	DW	EXIT
;   */MOD	( n1 n2 n3 -- r q )
;	Multiply n1 and n2, then divide by n3. Return mod and quotient.
	$COLON	5,'*/MOD',SSMOD
	DW	TOR,MSTAR,RFROM,MSMOD,EXIT
;   */	( n1 n2 n3 -- q )
;	Multiply n1 by n2, then divide by n3. Return quotient only.
	$COLON	2,'*/',STASL
	DW	SSMOD,SWAP,DROP,EXIT
;; Miscellaneous
;   CELL+	( a -- a )
;	Add cell size in byte to address.
	$COLON  5,'CELL+',CELLP
	DW	DOLIT,CELLL,PLUS,EXIT
;   CELL-	( a -- a )
;	Subtract cell size in byte from address.
	$COLON  5,'CELL-',CELLM
	DW	DOLIT,0-CELLL,PLUS,EXIT
;   CELLS	( n -- n )
;	Multiply tos by cell size in bytes.
	$COLON  5,'CELLS',CELLS
	DW	DOLIT,CELLL,STAR,EXIT
;   1+	  ( a -- a )
;	Add cell size in byte to address.
	$COLON  2,'1+',ONEP
	DW      DOLIT,1,PLUS,EXIT
;   1-	  ( a -- a )
;	Subtract cell size in byte from address.
	$COLON  2,'1-',ONEM
	DW      DOLIT,-1,PLUS,EXIT
;   2/	  ( n -- n )
;	Multiply tos by cell size in bytes.
	$COLON  2,'2/',TWOSL
	DW      DOLIT,CELLL,SLASH,EXIT
;   BL	( -- 32 )
;	Return 32, the blank character.

	$COLON	2,'BL',BLANK
	DW	DOLIT,' ',EXIT
;   >CHAR       ( c -- c )
;	Filter non-printing characters.
	$COLON  5,'>CHAR',TCHAR
	DW	DOLIT,07FH,ANDD,DUPP	;mask msb
	DW	DOLIT,127,BLANK,WITHI	;check for printable
	DW	QBRAN,TCHA1
	DW	DROP,DOLIT,'_'	;replace non-printables
TCHA1:
	DW	EXIT
;; Memory access
;   +!	( n a -- )
;	Add n to the contents at address a.
	$COLON	2,'+!',PSTOR
	DW	SWAP,OVER,AT,PLUS
	DW	SWAP,STORE,EXIT
;   2!	( d a -- )
;	Store the double integer to address a.
	$COLON	2,'2!',DSTOR
	DW	SWAP,OVER,STORE
	DW	CELLP,STORE,EXIT
;   2@	( a -- d )
;	Fetch double integer from address a.
	$COLON	2,'2@',DAT
	DW	DUPP,CELLP,AT
	DW	SWAP,AT,EXIT
;   COUNT	( b -- b +n )
;	Return count byte of a string and add 1 to byte address.
	$COLON	5,'COUNT',COUNT
	DW      DUPP,ONEP
	DW	SWAP,CAT,EXIT
;   HERE	( -- a )
;	Return the top of the code dictionary.
	$COLON	4,'HERE',HERE
	DW	CP,AT,EXIT
;   PAD	( -- a )
;	Return the address of the text buffer above the code dictionary.
	$COLON	3,'PAD',PAD
	DW	HERE,DOLIT,80,PLUS,EXIT
;   TIB	( -- a )
;	Return the address of the terminal input buffer.
	$COLON	3,'TIB',TIB
	DW	DOLIT,TIBB,EXIT
;   @EXECUTE	( a -- )
;	Execute vector stored in address a.
	$COLON	8,'@EXECUTE',ATEXE
	DW	AT,QDUP	;?address or zero
	DW	QBRAN,EXE1
	DW	EXECU	;execute if non-zero
EXE1:
	DW	EXIT	;do nothing if zero
;   CMOVE	( b1 b2 u -- )
;	Copy u bytes from b1 to b2.
	$COLON	5,'CMOVE',CMOVEE
	DW	TOR
	DW	BRAN,CMOV2
CMOV1:
	DW	TOR,DUPP,CAT
	DW	RAT,CSTOR
	DW      ONEP
	DW      RFROM,ONEP
CMOV2:
	DW	DONXT,CMOV1
	DW	DDROP,EXIT
;   FILL	( b u c -- )
;	Fill u bytes of character c to area beginning at b.
	$COLON	4,'FILL',FILL
	DW	SWAP,TOR,SWAP
	DW	BRAN,FILL2
FILL1:
	  DW      DDUP,CSTOR,ONEP
FILL2:
	DW	DONXT,FILL1
	DW	DDROP,EXIT
;   ERASE       ( b u -- )
;	       Erase u bytes beginning at b.
	$COLON  5,'ERASE',ERASE
	DW      DOLIT,0,FILL
	DW      EXIT
;   PACK$	( b u a -- a )
;	Build a counted string with u characters from b. Null fill.
	$COLON  5,'PACK$',PACKS
	DW      DUPP,TOR	  ;strings only on cell boundary
	DW      DDUP,CSTOR,ONEP ;save count
	DW	SWAP,CMOVEE,RFROM,EXIT	;move string
;; Numeric output, single precision
;   DIGIT	( u -- c )
;	Convert digit u to a character.
	$COLON  5,'DIGIT',DIGIT
	DW	DOLIT,9,OVER,LESS
	DW	DOLIT,7,ANDD,PLUS
	DW	DOLIT,'0',PLUS,EXIT
;   EXTRACT	( n base -- n c )
;	Extract the least significant digit from n.
	$COLON  7,'EXTRACT',EXTRC
	DW	DOLIT,0,SWAP,UMMOD
	DW	SWAP,DIGIT,EXIT
;   <#	( -- )
;	Initiate the numeric output process.
	$COLON	2,'<#',BDIGS
	DW	PAD,HLD,STORE,EXIT
;   HOLD	( c -- )
;	Insert a character into the numeric output string.
	$COLON	4,'HOLD',HOLD
	DW      HLD,AT,ONEM
	DW	DUPP,HLD,STORE,CSTOR,EXIT
;   #	( u -- u )
;	Extract one digit from u and append the digit to output string.
	$COLON	1,'#',DIG
	DW	BASE,AT,EXTRC,HOLD,EXIT
;   #S	( u -- 0 )
;	Convert u until all digits are added to the output string.
	$COLON	2,'#S',DIGS
DIGS1:
	DW	DIG,DUPP
	DW	QBRAN,DIGS2
	DW	BRAN,DIGS1
DIGS2:
	DW	EXIT
;   SIGN	( n -- )
;	Add a minus sign to the numeric output string.
	$COLON	4,'SIGN',SIGN
	DW	ZLESS
	DW	QBRAN,SIGN1
	DW	DOLIT,'-',HOLD
SIGN1:
	DW	EXIT
;   #>	( w -- b u )
;	Prepare the output string to be TYPE'd.
	$COLON	2,'#>',EDIGS
	DW	DROP,HLD,AT
	DW	PAD,OVER,SUBBB,EXIT
;   str	( w -- b u )
;	Convert a signed integer to a numeric string.
	$COLON  3,'str',STRR
	DW	DUPP,TOR,ABSS
	DW	BDIGS,DIGS,RFROM
	DW	SIGN,EDIGS,EXIT
;   HEX	( -- )
;	Use radix 16 as base for numeric conversions.
	$COLON	3,'HEX',HEX
	DW	DOLIT,16,BASE,STORE,EXIT
;   DECIMAL	( -- )
;	Use radix 10 as base for numeric conversions.
	$COLON	7,'DECIMAL',DECIM
	DW	DOLIT,10,BASE,STORE,EXIT
;; Numeric input, single precision
;   DIGIT?	( c base -- u t )
;	Convert a character to its numeric value. A flag indicates success.
	$COLON  6,'DIGIT?',DIGTQ
	DW	TOR,DOLIT,'0',SUBBB
	DW	DOLIT,9,OVER,LESS
	DW	QBRAN,DGTQ1
	DW	DOLIT,7,SUBBB
	DW	DUPP,DOLIT,10,LESS,ORR
DGTQ1:
	DW	DUPP,RFROM,ULESS,EXIT
;   NUMBER?	( a -- n T | a F )
;	Convert a number string to integer. Push a flag on tos.
	$COLON	7,'NUMBER?',NUMBQ
	DW	BASE,AT,TOR,DOLIT,0,OVER,COUNT
	DW	OVER,CAT,DOLIT,'$',EQUAL
	DW	QBRAN,NUMQ1
	DW      HEX,SWAP,ONEP
	DW      SWAP,ONEM
NUMQ1:
	DW	OVER,CAT,DOLIT,'-',EQUAL,TOR
	DW	SWAP,RAT,SUBBB,SWAP,RAT,PLUS,QDUP
	DW	QBRAN,NUMQ6
	DW      ONEM,TOR
NUMQ2:
	DW	DUPP,TOR,CAT,BASE,AT,DIGTQ
	DW	QBRAN,NUMQ4
	DW	SWAP,BASE,AT,STAR,PLUS,RFROM
	DW      ONEP
	DW	DONXT,NUMQ2
	DW	RAT,SWAP,DROP
	DW	QBRAN,NUMQ3
	DW	NEGAT
NUMQ3:
	DW	SWAP
	DW	BRAN,NUMQ5
NUMQ4:
	DW	RFROM,RFROM,DDROP,DDROP,DOLIT,0
NUMQ5:
	DW	DUPP
NUMQ6:
	DW	RFROM,DDROP
	DW	RFROM,BASE,STORE,EXIT
;; Basic I/O
;   KEY	( -- c )
;	Wait for and return an input character.
	$COLON	3,'KEY',KEY
KEY1:
	DW	QKEY
	DW	QBRAN,KEY1
	DW	EXIT
;   NUF?	( -- t )
;	Return false if no input, else pause and if CR return true.
	$COLON	4,'NUF?',NUFQ
	DW	QKEY,DUPP
	DW	QBRAN,NUFQ1
	DW	DDROP,KEY,DOLIT,CRR,EQUAL
NUFQ1:
	DW	EXIT
;   SPACE	( -- )
;	Send the blank character to the output device.
	$COLON	5,'SPACE',SPACE
	DW	BLANK,EMIT,EXIT
;   SPACES	( +n -- )
;	Send n spaces to the output device.
	$COLON	6,'SPACES',SPACS
	DW	DOLIT,0,MAX,TOR
	DW	BRAN,CHAR2
CHAR1:
	DW	SPACE
CHAR2:
	DW	DONXT,CHAR1
	DW	EXIT
;   TYPE	( b u -- )
;	Output u characters from b.
	$COLON	4,'TYPE',TYPES
	DW	TOR
	DW	BRAN,TYPE2
TYPE1:
	DW	DUPP,CAT,TCHAR,EMIT
	DW      ONEP
TYPE2:
	DW	DONXT,TYPE1
	DW	DROP,EXIT
;   CR	( -- )
;	Output a carriage return and a line feed.
	$COLON	2,'CR',CR
	DW	DOLIT,CRR,EMIT
	DW	DOLIT,LF,EMIT,EXIT
;   do$	( -- a )
;	Return the address of a compiled string.
	$COLON  COMPO+3,'do$',DOSTR
	DW	RFROM,RAT,RFROM,COUNT,PLUS
;	       DW      TOR,SWAP,TOR,EXIT
	DW      TOR,SWAP,TOR,EXIT
;   $"|	( -- a )
;	Run time routine compiled by $". Return address of a compiled string.
	$COLON  COMPO+3,'$"|',STRQP
	DW	DOSTR,EXIT	;force a call to do$
;   ."|	( -- )
;	Run time routine of ." . Output a compiled string.
	$COLON  COMPO+3,'."|',DOTQP
	DW	DOSTR,COUNT,TYPES,EXIT
;   .R	( n +n -- )
;	Display an integer in a field of n columns, right justified.
	$COLON	2,'.R',DOTR
	DW	TOR,STRR,RFROM,OVER,SUBBB
	DW	SPACS,TYPES,EXIT
;   U.R	( u +n -- )
;	Display an unsigned integer in n column, right justified.
	$COLON	3,'U.R',UDOTR
	DW	TOR,BDIGS,DIGS,EDIGS
	DW	RFROM,OVER,SUBBB
	DW	SPACS,TYPES,EXIT
;   U.	( u -- )
;	Display an unsigned integer in free format.
	$COLON	2,'U.',UDOT
	DW	BDIGS,DIGS,EDIGS
	DW	SPACE,TYPES,EXIT
;   .	( w -- )
;	Display an integer in free format, preceeded by a space.
	$COLON	1,'.',DOT
	DW	BASE,AT,DOLIT,10,XORR	;?decimal
	DW	QBRAN,DOT1
	DW	UDOT,EXIT	;no, display unsigned
DOT1:
	DW	STRR,SPACE,TYPES,EXIT	;yes, display signed
;   ?	( a -- )
;	Display the contents in a memory cell.
	$COLON	1,'?',QUEST
	DW	AT,DOT,EXIT
;; Parsing
;   parse	( b u c -- b u delta ; <string> )
;	Scan string delimited by c. Return found string and its offset.
	$COLON  5,'parse',PARS
	DW	TEMP,STORE,OVER,TOR,DUPP
	DW	QBRAN,PARS8
	DW      ONEM,TEMP,AT,BLANK,EQUAL
	DW	QBRAN,PARS3
	DW	TOR
PARS1:
	DW	BLANK,OVER,CAT	;skip leading blanks ONLY
	DW	SUBBB,ZLESS,INVER
	DW	QBRAN,PARS2
	DW      ONEP
	DW	DONXT,PARS1
	DW	RFROM,DROP,DOLIT,0,DUPP,EXIT
PARS2:
	DW	RFROM
PARS3:
	DW	OVER,SWAP
	DW	TOR
PARS4:
	DW	TEMP,AT,OVER,CAT,SUBBB	;scan for delimiter
	DW	TEMP,AT,BLANK,EQUAL
	DW	QBRAN,PARS5
	DW	ZLESS
PARS5:
	DW	QBRAN,PARS6
	DW      ONEP
	DW	DONXT,PARS4
	DW	DUPP,TOR
	DW	BRAN,PARS7
PARS6:
	DW	RFROM,DROP,DUPP
	DW      ONEP,TOR
PARS7:
	DW	OVER,SUBBB
	DW	RFROM,RFROM,SUBBB,EXIT
PARS8:
	DW	OVER,RFROM,SUBBB,EXIT
;   PARSE	( c -- b u ; <string> )
;	Scan input stream and return counted string delimited by c.
	$COLON	5,'PARSE',PARSE
	DW	TOR,TIB,INN,AT,PLUS	;current input buffer pointer
	DW	NTIB,AT,INN,AT,SUBBB	;remaining count
	DW	RFROM,PARS,INN,PSTOR,EXIT
;   .(	( -- )
;	Output following string up to next ) .
	$COLON	IMEDD+2,'.(',DOTPR
	DW	DOLIT,')',PARSE,TYPES,EXIT
;   (	( -- )
;	Ignore following string up to next ) . A comment.
	$COLON	IMEDD+1,'(',PAREN
	DW	DOLIT,')',PARSE,DDROP,EXIT
;   \	( -- )
;	Ignore following text till the end of line.
	$COLON	IMEDD+1,'\',BKSLA
	DW	NTIB,AT,INN,STORE,EXIT
;   WORD	( c -- a ; <string> )
;	Parse a word from input stream and copy it to code dictionary.
	$COLON	4,'WORD',WORDD
	DW      PARSE
	DW      HERE,CELLP
	DW	PACKS,EXIT
;   TOKEN	( -- a ; <string> )
;	Parse a word from input stream and copy it to name dictionary.
	$COLON  5,'TOKEN',TOKEN
	DW      BLANK,WORDD,EXIT
;; Dictionary search
;   NAME>	( na -- ca )
;	Return a code address given a name address.
	$COLON  5,'NAME>',NAMET
	DW      COUNT,DOLIT,31,ANDD
	DW      PLUS,EXIT
;   SAME?	( a a u -- a a f \ -0+ )
;	Compare u cells in two strings. Return 0 if identical.
	$COLON  5,'SAME?',SAMEQ
	DW      ONEM,TOR
	DW	BRAN,SAME2
SAME1:
	  DW      OVER,RAT,PLUS,CAT
	DW      OVER,RAT,PLUS,CAT
	DW	SUBBB,QDUP
	DW	QBRAN,SAME2
	DW	RFROM,DROP,EXIT
SAME2:
	DW	DONXT,SAME1
	DW	DOLIT,0,EXIT
;   find	( a va -- ca na | a F )
;	Search a vocabulary for a string. Return ca and na if succeeded.
	$COLON  4,'find',FIND
	DW	SWAP,DUPP,CAT
	DW      TEMP,STORE
	DW	DUPP,AT,TOR,CELLP,SWAP
FIND1:
	DW	AT,DUPP
	DW	QBRAN,FIND6
	DW	DUPP,AT,DOLIT,MASKK,ANDD,RAT,XORR
	DW	QBRAN,FIND2
	DW	CELLP,DOLIT,-1
	DW	BRAN,FIND3
FIND2:
	DW	CELLP,TEMP,AT,SAMEQ
FIND3:
	DW	BRAN,FIND4
FIND6:
	DW	RFROM,DROP
	DW	SWAP,CELLM,SWAP,EXIT
FIND4:
	DW	QBRAN,FIND5
	DW	CELLM,CELLM
	DW	BRAN,FIND1
FIND5:
	DW	RFROM,DROP,SWAP,DROP
	DW	CELLM
	DW	DUPP,NAMET,SWAP,EXIT
;   NAME?	( a -- ca na | a F )
;	Search all context vocabularies for a string.
	$COLON  5,'NAME?',NAMEQ
	DW      CNTXT,FIND,EXIT
;; Terminal response
;   ^H	( bot eot cur -- bot eot cur )
;	Backup the cursor by one character.
	$COLON  2,'^h',BKSP
	DW	TOR,OVER,RFROM,SWAP,OVER,XORR
	DW	QBRAN,BACK1
	DW      DOLIT,BKSPP,EMIT,ONEM
	DW      BLANK,EMIT
	DW      DOLIT,BKSPP,EMIT
BACK1:
	DW	EXIT
;   TAP	( bot eot cur c -- bot eot cur )
;	Accept and echo the key stroke and bump the cursor.
	$COLON  3,'TAP',TAP
	DW      DUPP,EMIT
	DW      OVER,CSTOR,ONEP,EXIT
;   kTAP	( bot eot cur c -- bot eot cur )
;	Process a key stroke, CR or backspace.
	$COLON  4,'kTAP',KTAP
	DW	DUPP,DOLIT,CRR,XORR
	DW	QBRAN,KTAP2
	DW	DOLIT,BKSPP,XORR
	DW	QBRAN,KTAP1
	DW	BLANK,TAP,EXIT
KTAP1:
	DW	BKSP,EXIT
KTAP2:
	DW	DROP,SWAP,DROP,DUPP,EXIT
;   accept	( b u -- b u )
;	Accept characters to input buffer. Return with actual count.
	$COLON  6,'ACCEPT',ACCEP
	DW	OVER,PLUS,OVER
ACCP1:
	DW	DDUP,XORR
	DW	QBRAN,ACCP4
	DW	KEY,DUPP
;	DW	BLANK,SUBBB,DOLIT,95,ULESS
	DW	BLANK,DOLIT,127,WITHI
	DW	QBRAN,ACCP2
	DW	TAP
	DW	BRAN,ACCP3
ACCP2:
	  DW      KTAP
ACCP3:
	DW	BRAN,ACCP1
ACCP4:
	DW	DROP,OVER,SUBBB,EXIT
;   QUERY	( -- )
;	Accept input stream to terminal input buffer.
	$COLON	5,'QUERY',QUERY
	DW      TIB,DOLIT,80,ACCEP,NTIB,STORE
	DW	DROP,DOLIT,0,INN,STORE,EXIT
;   ABORT	( -- )
;	Reset data stack and jump to QUIT.
	$COLON	5,'ABORT',ABORT
	DW      PRESE,QUIT

;   abort"	( f -- )
;	Run time routine of ABORT" . Abort with a message.
	$COLON  COMPO+6,'abort"',ABORQ
	DW      QBRAN,ABOR2	     ;text flag
	DW      DOSTR
ABOR1:
	  DW      SPACE,COUNT,TYPES
	DW      DOLIT,'?',EMIT,CR,ABORT  ;pass error string
ABOR2:
	  DW      DOSTR,DROP,EXIT	 ;drop error
;; The text interpreter
;   $INTERPRET	( a -- )
;	Interpret a word. If failed, try to convert it to an integer.
	$COLON  10,'$INTERPRET',INTER
	DW	NAMEQ,QDUP	;?defined
	DW	QBRAN,INTE1
	DW	AT,DOLIT,COMPO,ANDD	;?compile only lexicon bits
	DW	ABORQ
	DB	13,' compile only'
	DW	EXECU,EXIT	;execute defined word
INTE1:
	  DW      NUMBQ	    ;convert a number
	DW      QBRAN,ABOR1
	DW	EXIT
;   [	( -- )
;	Start the text interpreter.
	$COLON	IMEDD+1,'[',LBRAC
	DW	DOLIT,INTER,TEVAL,STORE,EXIT
;   EVAL	( -- )
;	Interpret the input stream.
	$COLON	4,'EVAL',EVAL
EVAL1:
	DW	TOKEN,DUPP,CAT	;?input stream empty
	DW	QBRAN,EVAL2
	DW	TEVAL,ATEXE	;evaluate input, check stack
	DW	BRAN,EVAL1
EVAL2:
	  DW      DROP,DOTS,EXIT   ;prompt
;   PRESET	( -- )
;	Reset data stack pointer and the terminal input buffer.
	$COLON  6,'PRESET',PRESE
	DW	DOLIT,TIBB,NTIB,CELLP,STORE,EXIT
;   QUIT	( -- )
;	Reset return stack pointer and start text interpreter.
	$COLON	4,'QUIT',QUIT
	DW      DOLIT,RPP,RPSTO	  ;reset return stack pointer
QUIT1:
	DW	LBRAC	;start interpretation
QUIT2:
	DW	QUERY	;get input
	DW      EVAL
	DW      BRAN,QUIT2	     ;continue till error
;; The compiler
;   '	( -- ca )
;	Search context vocabularies for the next word in input stream.
	$COLON	1,"'",TICK
	DW	TOKEN,NAMEQ	;?defined
	DW      QBRAN,ABOR1
	DW	EXIT	;yes, push code address
;   ALLOT	( n -- )
;	Allocate n bytes to the code dictionary.
	$COLON	5,'ALLOT',ALLOT
	DW	CP,PSTOR,EXIT	;adjust code pointer
;   ,	( w -- )
;	Compile an integer into the code dictionary.
	$COLON	1,',',COMMA
	DW	HERE,DUPP,CELLP	;advance 1 cell 
	DW	CP,STORE,STORE,EXIT	;adjust code pointer and compile
;   C,	( b -- )
;	Compile a byte into the code dictionary.
	$COLON	2,'C,',CCOMMA
	DW	HERE,DUPP,ONEP	; advance 1 byte
	DW	CP,STORE,CSTOR,EXIT	;adjust code pointer and compile
;   [COMPILE]	( -- ; <string> )
;	Compile the next immediate word into code dictionary.
	$COLON	IMEDD+9,'[COMPILE]',BCOMP
	DW	TICK,COMMA,EXIT
;   COMPILE	( -- )
;	Compile the next address in colon list to code dictionary.
	$COLON	COMPO+7,'COMPILE',COMPI
	DW	RFROM,DUPP,AT,COMMA	;compile address
	DW	CELLP,TOR,EXIT	;adjust return address
;   LITERAL	( w -- )
;	Compile tos to code dictionary as an integer literal.
	$COLON	IMEDD+7,'LITERAL',LITER
	DW	COMPI,DOLIT,COMMA,EXIT
;   $,"	( -- )
;	Compile a literal string up to next " .
	$COLON  3,'$,"',STRCQ
	DW      DOLIT,'"',PARSE,HERE,PACKS   ;string to code dictionary
	DW      COUNT,PLUS	;calculate aligned end of string
	DW	CP,STORE,EXIT	;adjust the code pointer
;; Structures
;   FOR	( -- a )
;	Start a FOR-NEXT loop structure in a colon definition.
	$COLON	IMEDD+3,'FOR',FORR
	DW	COMPI,TOR,HERE,EXIT
;   BEGIN	( -- a )
;	Start an infinite or indefinite loop structure.
	$COLON	IMEDD+5,'BEGIN',BEGIN
	DW	HERE,EXIT
;   NEXT	( a -- )
;	Terminate a FOR-NEXT loop structure.
	$COLON	IMEDD+4,'NEXT',NEXT
	DW	COMPI,DONXT,COMMA,EXIT
;   UNTIL	( a -- )
;	Terminate a BEGIN-UNTIL indefinite loop structure.
	$COLON	IMEDD+5,'UNTIL',UNTIL
	DW	COMPI,QBRAN,COMMA,EXIT
;   AGAIN	( a -- )
;	Terminate a BEGIN-AGAIN infinite loop structure.
	$COLON	IMEDD+5,'AGAIN',AGAIN
	DW	COMPI,BRAN,COMMA,EXIT
;   IF	( -- A )
;	Begin a conditional branch structure.
	$COLON	IMEDD+2,'IF',IFF
	DW	COMPI,QBRAN,HERE
	DW	DOLIT,0,COMMA,EXIT
;   AHEAD	( -- A )
;	Compile a forward branch instruction.
	$COLON	IMEDD+5,'AHEAD',AHEAD
	DW	COMPI,BRAN,HERE,DOLIT,0,COMMA,EXIT
;   REPEAT	( A a -- )
;	Terminate a BEGIN-WHILE-REPEAT indefinite loop.
	$COLON	IMEDD+6,'REPEAT',REPEA
	DW	AGAIN,HERE,SWAP,STORE,EXIT
;   THEN	( A -- )
;	Terminate a conditional branch structure.
	$COLON	IMEDD+4,'THEN',THENN
	DW	HERE,SWAP,STORE,EXIT
;   AFT	( a -- a A )
;	Jump to THEN in a FOR-AFT-THEN-NEXT loop the first time through.
	$COLON	IMEDD+3,'AFT',AFT
	DW	DROP,AHEAD,BEGIN,SWAP,EXIT
;   ELSE	( A -- A )
;	Start the false clause in an IF-ELSE-THEN structure.
	$COLON	IMEDD+4,'ELSE',ELSEE
	DW	AHEAD,SWAP,THENN,EXIT
;   WHILE	( a -- A a )
;	Conditional branch out of a BEGIN-WHILE-REPEAT loop.
	$COLON	IMEDD+5,'WHILE',WHILEE
	DW	IFF,SWAP,EXIT
;   ABORT"	( -- ; <string> )
;	Conditional abort with an error message.
	$COLON	IMEDD+6,'ABORT"',ABRTQ
	DW	COMPI,ABORQ,STRCQ,EXIT
;   $"	( -- ; <string> )
;	Compile an inline string literal.
	$COLON	IMEDD+2,'$"',STRQ
	DW	COMPI,STRQP,STRCQ,EXIT
;   ."	( -- ; <string> )
;	Compile an inline string literal to be typed out at run time.
	$COLON	IMEDD+2,'."',DOTQ
	DW	COMPI,DOTQP,STRCQ,EXIT
;; Name compiler
;   ?UNIQUE	( a -- a )
;	Display a warning message if the word already exists.
	$COLON  7,'?UNIQUE',UNIQU
	DW	DUPP,NAMEQ	;?name exists
	DW	QBRAN,UNIQ1
	DW	DOTQP	;redefinitions are OK
	DB	7,' reDef '	;but the user should be warned
	DW	OVER,COUNT,TYPES	;just in case its not planned
UNIQ1:
	DW	DROP,EXIT
;   $,n	( na -- )
;	Build a new dictionary name using the string at na.
	$COLON  3,'$,n',SNAME
	DW	DUPP,CAT	;?null input
	DW	QBRAN,PNAM1
	DW	UNIQU	;?redefinition
	DW      DUPP,COUNT,PLUS
	DW      CP,STORE
	DW	DUPP,LAST,STORE	;save na for vocabulary link
	DW	CELLM	;link address
	DW      CNTXT,AT,SWAP
	DW	STORE,EXIT	;save code pointer
PNAM1:
	  DW      STRQP
	DB	5,' name'	;null input
	DW      BRAN,ABOR1
;; FORTH compiler
;   $COMPILE	( a -- )
;	Compile next word to code dictionary as a token or literal.
	$COLON  8,'$COMPILE',SCOMP
	DW	NAMEQ,QDUP	;?defined
	DW	QBRAN,SCOM2
	DW	AT,DOLIT,IMEDD,ANDD	;?immediate
	DW	QBRAN,SCOM1
	DW	EXECU,EXIT	;its immediate, execute
SCOM1:
	DW	COMMA,EXIT	;its not immediate, compile
SCOM2:
	  DW      NUMBQ	    ;try to convert to number
	DW      QBRAN,ABOR1
	DW	LITER,EXIT	;compile number as integer
;   OVERT	( -- )
;	Link a new word into the current vocabulary.
	$COLON  5,'OVERT',OVERT
	DW      LAST,AT,CNTXT,STORE,EXIT
;   ;	( -- )
;	Terminate a colon definition.
	$COLON	IMEDD+COMPO+1,';',SEMIS
	DW	COMPI,EXIT,LBRAC,OVERT,EXIT
;   ]	( -- )
;	Start compiling the words in the input stream.
	$COLON	1,']',RBRAC
	DW	DOLIT,SCOMP,TEVAL,STORE,EXIT
;   call,	( ca -- )
;	Assemble a call instruction to ca.
	$COLON  5,'call,',CALLC
	DW	DOLIT,CALLL,CCOMMA	;Direct Threaded Code
	DW	COMMA,EXIT	;DTC 8080 absolute call
;   :	( -- ; <string> )
;	Start a new colon definition using next word as its name.
	$COLON	1,':',COLON
	DW	TOKEN,SNAME,DOLIT,DOLST
	DW	CALLC,RBRAC,EXIT
;   IMMEDIATE	( -- )
;	Make the last compiled word an immediate word.
	$COLON	9,'IMMEDIATE',IMMED
	DW	DOLIT,IMEDD,LAST,AT,AT,ORR
	DW	LAST,AT,STORE,EXIT
;; Defining words
;   CREATE	( -- ; <string> )
;	Compile a new array entry without allocating code space.
	$COLON	6,'CREATE',CREAT
	DW	TOKEN,SNAME,OVERT
	DW	DOLIT,DOLST,CALLC
	DW	DOLIT,DOVAR,COMMA,EXIT
;   VARIABLE	( -- ; <string> )
;	Compile a new variable initialized to 0.
	$COLON	8,'VARIABLE',VARIA
	DW	CREAT,DOLIT,0,COMMA,EXIT
;; Tools
;   dm+	( a u -- a )
;	Dump u bytes from , leaving a+u on the stack.
	$COLON  3,'dm+',DUMPP
	DW	OVER,DOLIT,4,UDOTR	;display address
	DW	SPACE,TOR	;start count down loop
	DW	BRAN,PDUM2	;skip first pass
PDUM1:
	DW	DUPP,CAT,DOLIT,3,UDOTR	;display numeric data
	DW      ONEP	    ;increment address
PDUM2:
	DW	DONXT,PDUM1	;loop till done
	DW	EXIT
;   DUMP	( a u -- )
;	Dump u bytes from a, in a formatted manner.
	$COLON	4,'DUMP',DUMP
	DW	BASE,AT,TOR,HEX	;save radix, set hex
	DW	DOLIT,16,SLASH	;change count to lines
	DW	TOR	;start count down loop
DUMP1:
	DW	CR,DOLIT,16,DDUP,DUMPP	;display numeric
	DW	ROT,ROT
	DW	DOLIT,2,SPACS,TYPES	;display printable characters
	DW	NUFQ,INVER	;user control
	DW	QBRAN,DUMP2
	DW	DONXT,DUMP1	;loop till done
	DW	BRAN,DUMP3
DUMP2:
	DW	RFROM,DROP	;cleanup loop stack, early exit
DUMP3:
	DW	DROP,RFROM,BASE,STORE	;restore radix
	DW	EXIT
;   .S	( ... -- ... )
;	Display the contents of the data stack.
	$COLON	2,'.S',DOTS
	DW	CR,TOR,TOR,TOR	;stack depth
	DW	TOR,DUPP,DOT	;skip first pass
	DW	RFROM,DUPP,DOT	;skip first pass
	DW	RFROM,DUPP,DOT	;skip first pass
	DW	RFROM,DUPP,DOT	;skip first pass
	DW	RFROM,DUPP,DOT	;skip first pass
	DW	DOTQP
	DB      5,'<top '
	DW	EXIT
;   >NAME	( ca -- na | F )
;	Convert code address to a name address.
	$COLON  5,'>NAME',TNAME
	DW      CNTXT	   ;vocabulary link
TNAM2:
	DW	AT,DUPP	;?last word in a vocabulary
	DW      QBRAN,TNAM4
	DW	DDUP,NAMET,XORR	;compare
	DW	QBRAN,TNAM3
	DW	CELLM	;continue with next word
	DW	BRAN,TNAM2
TNAM3:
	  DW      SWAP,DROP,EXIT
TNAM4:
	  DW      DDROP,DOLIT,0,EXIT
;   .ID	( na -- )
;	Display the name at address.
	$COLON  3,'.ID',DOTID
	DW	QDUP	;if zero no name
	DW	QBRAN,DOTI1
	DW	COUNT,DOLIT,01FH,ANDD	;mask lexicon bits
	DW	TYPES,EXIT	;display name string
DOTI1:
	DW	DOTQP
	DB	9,' {noName}'
	DW	EXIT
;   SEE	( -- ; <string> )
;	       A simple decompiler. Updated for byte machines, 08mar98cht
	$COLON	3,'SEE',SEE
	DW	TICK	;starting address
	DW      CR,CELLP
SEE1:
	   DW      ONEP,DUPP,AT,DUPP      ;?does it contain a zero
	DW	QBRAN,SEE2
	DW	TNAME	;?is it a name
SEE2:
	DW	QDUP	;name address or zero
	DW	QBRAN,SEE3
	DW	SPACE,DOTID	;display name
	DW      ONEP
	DW	BRAN,SEE4
SEE3:
	   DW      DUPP,CAT,UDOT	    ;display number
SEE4:
	DW	NUFQ	;user control
	DW	QBRAN,SEE1
	DW	DROP,EXIT
;   WORDS	( -- )
;	Display the names in the context vocabulary.
	$COLON	5,'WORDS',WORDS
	DW      CR,CNTXT	     ;only in context
WORS1:
	DW	AT,QDUP	;?at end of list
	DW	QBRAN,WORS2
	DW	DUPP,SPACE,DOTID	;display a name
	DW	CELLM,NUFQ	;user control
	DW	QBRAN,WORS1
	DW	DROP
WORS2:
	DW	EXIT
       
;; Hardware reset
;   hi	( -- )
;	Display the sign-on message of eForth.
	$COLON	2,'hi',HI
	DW      CR,DOTQP	  ;initialize I/O
	DB      11,'eP8080 v',VER+'0','.',EXT+'0'     ;version
	DW	CR,EXIT
;   'BOOT       ( -- a )
;	       The application startup vector.
	$COLON  5,"'BOOT",TBOOT
	DW      DOVAR
	DW      HI	      ;application to boot
;   COLD	( -- )
;	The hilevel cold start sequence.
	$COLON	4,'COLD',COLD
COLD1:
	DW      DOLIT,UZERO,DOLIT,UPP
	DW      DOLIT,14H,CMOVEE ;initialize user area
	DW	PRESE	;initialize data stack and TIB
	DW      TBOOT,ATEXE	     ;application boot
	DW      OVERT
	DW	QUIT	;start interpretation
	DW	BRAN,COLD1	;just in case
;===============================================================
LASTN	   =       _LINK	   ;last name address in name dictionary
CTOP	    EQU     $	       ;next available memory in code dictionary
MAIN	ENDS
END	ORIG
;===============================================================

