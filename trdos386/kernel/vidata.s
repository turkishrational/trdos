; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.7 - vidata.s
; ----------------------------------------------------------------------------
; Last Update: 17/10/2023 (Previous: 24/11/2020)
; ----------------------------------------------------------------------------
; Beginning: 16/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; vidata.inc (11/03/2015)
;
; Derived from 'IBM PC-AT' BIOS source code (1985) 
; ****************************************************************************

; Retro UNIX 386 v1 Kernel - VIDATA.S
; Last Modification: 11/03/2015
;		    (Data section for 'VIDEO.INC')	
;
; ///////// VIDEO DATA ///////////////

;----------------------------------------
;	VIDEO DISPLAY DATA AREA		;
;----------------------------------------
CRT_MODE:	db	3	; CURRENT DISPLAY MODE (TYPE)
CRT_MODE_SET:	db	29h	; CURRENT SETTING OF THE 3X8 REGISTER
				; (29h default setting for video mode 3)
				; Mode Select register Bits
				;   BIT 0 - 80x25 (1), 40x25 (0)
				;   BIT 1 - ALPHA (0), 320x200 GRAPHICS (1)
				;   BIT 2 - COLOR (0), BW (1)
				;   BIT 3 - Video Sig. ENABLE (1), DISABLE (0)
				;   BIT 4 - 640x200 B&W Graphics Mode (1)
				;   BIT 5 - ALPHA mode BLINKING (1)
				;   BIT 6, 7 - Not Used

; Mode 0 - 2Ch = 101100b	; 40x25 text, 16 gray colors
; Mode 1 - 28h = 101000b	; 40x25 text, 16 fore colors, 8 back colors
; Mode 2 - 2Dh = 101101b	; 80x25 text, 16 gray colors	
; Mode 3 - 29h = 101001b	; 80x25 text, 16 fore color, 8 back color
; Mode 4 - 2Ah = 101010b	; 320x200 graphics, 4 colors
; Mode 5 - 2Eh = 101110b	; 320x200 graphics, 4 gray colors
; Mode 6 - 1Eh = 011110b	; 640x200 graphics, 2 colors
; Mode 7 - 29h = 101001b	; 80x25 text, black & white colors
; Mode & 37h = Video signal OFF

; 24/06/2016
CRT_COLS:	db	80	; Number of columns

; 01/07/2016
CRT_PALETTE:	db 	0	; Current palette setting

; 03/07/2016
CHAR_HEIGHT:	db	16	; Default character height
VGA_VIDEO_CTL:	db	60h	; ROM BIOS DATA AREA Offset 87h
VGA_SWITCHES:	db 	0F9h	; Feature Bit Switches (the basic screen)
VGA_MODESET_CTL: db	051h	; Basic mode set options (VGA video flags)
				; ROM BIOS DATA AREA Offset 89h
				; Bit 7, 4 : Mode
				;	  01 : 400-line mode
				; Bit 6	: Display switch enabled = 1			
				; Bit 5	: Reserved = 0
				; Bit 3	: Default palette loading 
				;	  disabled = 0
				; Bit 2 : Color monitor = 0
				; Bit 1 = Gray scale summing 
				;	  disabled = 0
				; Bit 0 = VGA active = 1
VGA_ROWS:	db	25

; 16/01/2016
chr_attrib:  ; Character color/attributes for video pages (0 to 7)
	db	07h, 07h, 07h, 07h, 07h, 07h, 07h, 07h
; 30/01/2016
vmode:
	db	3,3,3,3,3,3,3,3 ; video modes for pseudo screens 

CURSOR_MODE: ; cursor start (ch) = 14, cursor end (cl) = 15
	db	15, 14 ; 07/07/2016 - TRDOS 386 (TRDOS v2.0)

;align 4
;VGA_BASE: ; 26/07/2016
;	dd	 0B8000h  ; (Mode < 0Dh) or 0A0000h (mode >= 0Dh)

align 2

vga_modes:
	; 25/07/2016
	; 09/07/2016
	; 03/07/2016
	; valid (implemented) video modes (>7, extension to IBM PC CGA modes)
	db 	03h, 02h, 01h, 00h, 07h, 04h, 05h, 06h
vga_g_modes: ; 31/07/2016
	db	13h, 0F0h, 12h, 6Ah, 0Dh, 0Eh, 10h, 11h
vga_mode_count equ $ - vga_modes
vga_g_mode_count equ $ - vga_g_modes

vga_mode_tbl_ptr:
	; 25/07/2016
	dd	vga_mode_03h
	dd	vga_mode_03h ; mode 02h -> mode 03h 
	dd	vga_mode_01h
	dd	vga_mode_01h ; mode 00h -> mode 01h
	;dd	vga_mode_07h
	dd	vga_mode_03h ; mode 07h -> mode 03h
	dd	vga_mode_04h
	dd	vga_mode_04h ; mode 05h -> mode 04h	
	dd	vga_mode_06h
	dd	vga_mode_13h
	dd	vga_mode_F0h
	dd	vga_mode_12h
	dd	vga_mode_6Ah
	dd	vga_mode_0Dh
	dd	vga_mode_0Eh
	dd	vga_mode_10h
	dd	vga_mode_11h	

vga_memmodel: 
	; 25/07/2016
	; 07/07/2016
	CTEXT	equ 0
	;MTEXT	equ 1
	MTEXT	equ 0 ; mode 07h -> mode 03h
	CGA	equ 2
	LINEAR8 equ 5
	PLANAR4	equ 4
	PLANAR1	equ 3
	db	CTEXT, CTEXT, CTEXT, CTEXT, MTEXT, CGA, CGA, CGA
vga_g_memmodel: ; 31/07/2016
	db	LINEAR8, PLANAR4, PLANAR4, PLANAR4, PLANAR4, PLANAR4, PLANAR4, PLANAR1
;vga_pixbits:
;	; 25/07/2016
;	; 08/07/2016
;	db 	4, 4, 4, 4, 4, 2, 2, 1, 8, 4, 4, 4, 4, 4, 4, 1
vga_dac_s:
	db	2, 2, 2, 2, 0, 1, 1, 1, 3, 3, 2, 2, 1, 1, 2, 2  
						; (vgatables.h, VGAMODES, dac) 
						; 17/11/2020
vga_params:
	; 23/11/2020
	; 16/11/2020
	; 09/11/2020, 10/11/2020, 11/11/2020 (TRDOS 386 v2.0.3)
	; 25/07/2016 
	; 19/07/2016
	; 03/07/2016
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios Developers Team (2001-2008)
	; 'vgatables.h'
	; Oracle VirtualBox 5.0.24 VGABios Source Code
	; ('vgabios.c', 'vgatables.h', 'vgafonts.h', 'vgarom.asm')

	; 09/11/2020
	; Block Structure of Video Parameter Table
	;
	; Offset	# Bytes		Contents
	;
	;  0		1		# columns
	;  1		1  		# rows - 1 
	;  2		1		Pixels/character
	;  3-4		2		Page length		
	;  5-8		4		Sequencer Registers
	;  9		1		Miscellaneous Register	
	;  10-34	25		CRTC Registers
	;  35-54	20		Attribute Registers
	;  55-63	9		Graphics Controller Registers
	;
	; Ref: Programmer's Guide to EGA, VGA, and Super VGA cards
	; (Richard F. Ferraro, 1994)  

	;
vga_mode_03h:  ; mode 03h, 80*25 text, CGA colors
	; 11/11/2020
 	db	80, 24, 16, 00h, 10h ; tw, th-1, ch, slength (5)
 	db	00h, 03h, 00h, 02h ; sequ regs (4)
 	db	67h	; misc reg (1)
 	db	5Fh, 4Fh, 50h, 82h, 55h, 81h, 0BFh, 1Fh
 	; 09/11/2020
	;db	5Fh, 4Fh, 50h, 82h, 54h, 80h, 0BFh, 1Fh
	db	00h, 4Fh
vga_p_cm_pos equ $ - vga_mode_03h
	db	0Dh, 0Eh, 00h, 00h, 00h, 00h
 	db	9Ch, 8Eh, 8Fh, 28h, 1Fh, 96h, 0B9h, 0A3h
	db	0FFh	; crtc_regs (25)
	db	00h, 01h, 02h, 03h, 04h, 05h, 14h, 07h
 	db	38h, 39h, 3Ah, 3Bh, 3Ch, 3Dh, 3Eh, 3Fh
 	; 17/10/2023
	db	0Ch, 00h, 0Fh, 08h  ; actl regs (20)
	;db	0Ch, 00h, 0Fh, 00h  ; 19/11/2020
	db	00h, 00h, 00h, 00h, 00h, 10h, 0Eh, 0Fh, 0FFh ; grdc regs (9)
	; 09/11/2020
	;db	00h, 00h, 00h, 00h, 00h, 10h, 0Eh, 00h, 0FFh ; grdc regs (9)
vga_mode_01h:	; mode 01h, 40*25 text, CGA colors
	db	40, 24, 16, 00h, 08h ; tw, th-1, ch, slength
	db	08h, 03h, 00h, 02h  ; sequ regs
	db	67h	; misc reg
	db	2Dh, 27h, 28h, 90h, 2Bh, 0A0h, 0BFh, 1Fh
	db	00h, 4Fh, 0Dh, 0Eh, 00h, 00h, 00h, 00h
	db	9Ch, 8Eh, 8Fh, 14h, 1Fh, 96h, 0B9h, 0A3h
	db	0FFh	; crtc_regs
	db	00h, 01h, 02h, 03h, 04h, 05h, 14h, 07h
	db	38h, 39h, 3Ah, 3Bh, 3Ch, 3Dh, 3Eh, 3Fh
 	;db	0Ch, 00h, 0Fh, 08h  ; actl regs (20)
	db	0Ch, 00h, 0Fh, 00h  ; 19/11/2020
	db	00h, 00h, 00h, 00h, 00h, 10h, 0Eh, 0Fh, 0FFh ; grdc regs
;vga_mode_07h:	; mode 07h, 80*25 text, mono color
;	db	80, 24, 16, 00h, 10h  ; tw, th-1, ch, slength
;	db	00h, 03h, 00h, 02h ; sequ regs
;	db	66h	; misc reg
;	db	5Fh, 4Fh, 50h, 82h, 55h, 81h, 0BFh, 1Fh
;	db	00h, 4Fh, 0Dh, 0Eh, 00h, 00h, 00h, 00h
;	db	9Ch, 8Eh, 8Fh, 28h, 0Fh, 96h, 0B9h, 0A3h
;	db	0FFh	; crtc regs
;	db	00h, 08h, 08h, 08h, 08h, 08h, 08h, 08h
;	db	10h, 18h, 18h, 18h, 18h, 18h, 18h, 18h
;	db	0Eh, 00h, 0Fh, 08h  ; actl regs
;	db	00h, 00h, 00h, 00h, 00h, 10h, 0Ah, 0Fh, 0FFh ; grdc regs
vga_mode_04h:	; 320*200 graphics, 4 colors, CGA
	; 11/11/2020
	db	40, 24, 8, 00h, 40h ; tw, th-1, ch, slength
	db	09h, 03h, 00h, 02h ; sequ regs
	db	63h	; misc reg
	db	2Dh, 27h, 28h, 90h, 2Bh, 80h, 0BFh, 1Fh
	db	00h, 0C1h, 00h, 00h, 00h, 00h, 00h, 00h
	db	9Ch, 8Eh, 8Fh, 14h, 00h, 96h, 0B9h, 0A2h
	db	0FFh	; crtc_regs
	db	00h, 13h, 15h, 17h, 02h, 04h, 06h, 07h
	db	10h, 11h, 12h, 13h, 14h, 15h, 16h, 17h
	db	01h, 00h, 03h, 00h ; actl regs
	db 	00h, 00h, 00h, 00h, 00h, 30h, 0Fh, 0Fh, 0FFh ; grdc regs
vga_mode_06h:	; 640*200 graphics, 2 colors, CGA
	; 11/11/2020
	db	80, 24, 8, 00h, 40h   ;	tw, th-1, ch, slength
	db	01h, 01h, 00h, 06h ; sequ regs
	db	63h	; misc reg
	db	5Fh, 4Fh, 50h, 82h, 54h, 80h, 0BFh, 1Fh
	db	00h, 0C1h, 00h, 00h, 00h, 00h, 00h, 00h
	db	9Ch, 8Eh, 8Fh, 28h, 00h, 96h, 0B9h, 0C2h
	db	0FFh	; crtc regs
	db	00h, 17h, 17h, 17h, 17h, 17h, 17h, 17h
	db	17h, 17h, 17h, 17h, 17h, 17h, 17h, 17h
	db	01h, 00h, 01, 00h  ; actl regs
	db	00h, 00h, 00h, 00h, 00h, 00h, 0Dh, 0Fh, 0FFh ; grdc regs
vga_mode_13h:  ; mode 13h, 300*200, 256 colors, linear
	; 11/11/2020
	;db 	40, 24, 8, 00h, 20h ; tw, th-1, ch, slength (5)
	; 23/11/2020 - 10/11/2020
	db 	40, 24, 8, 00h, 0FAh ; tw, th-1, ch, slength (5)
	db	01h, 0Fh, 00h, 0Eh ; sequ regs (4)
	db	63h	; misc reg (1)
	db	5Fh, 4Fh, 50h, 82h, 54h, 80h, 0BFh, 1Fh 
	db 	00h, 041h, 00h, 00h, 00h, 00h, 00h, 00h
	db	9Ch, 8Eh, 8Fh, 28h, 40h, 96h, 0B9h, 0A3h
	;db	9Ch, 0Eh, 8Fh, 28h, 40h, 96h, 0B9h, 0A3h ; 17/10/2023
	db	0FFh	; crtc regs (25)
	db	00h, 01h, 02h, 03h, 04h, 05h, 06h, 07h
 	db	08h, 09h, 0Ah, 0Bh, 0Ch, 0Dh, 0Eh, 0Fh
 	db	41h, 00h, 0Fh, 00h  ; actl regs (20)
	; 10/11/2020
 	;db	41h, 01h, 0Fh, 13h  ; actl regs (20) 
	db	00h, 00h, 00h, 00h, 00h, 40h, 05h, 0Fh, 0FFh ; grdc regs (9)
vga_mode_setl equ $ - vga_mode_13h  ; = 64
vga_mode_F0h:  ; mode X ; 320*240, 256 colors, planar
	db 	40, 24, 8, 00h, 00h ; tw, th-1, ch, slength
	db	01h, 0Fh, 00h, 06h ; sequ regs
	db	0E3h	; misc reg
	db	5Fh, 4Fh, 50h, 82h, 54h, 80h, 0Dh, 3Eh 
 	db 	00h, 41h, 00h, 00h, 00h, 00h, 00h, 00h
	db	0EAh, 0ACh, 0DFh, 28h, 00h, 0E7h, 06h, 0E3h
	db	0FFh	; crtc regs (25)
	db	00h, 01h, 02h, 03h, 04h, 05h, 06h, 07h
 	db	08h, 09h, 0Ah, 0Bh, 0Ch, 0Dh, 0Eh, 0Fh
 	db	41h, 00h, 0Fh, 00h  ; actl regs
	db	00h, 00h, 00h, 00h, 00h, 40h, 05h, 0Fh, 0FFh ; grdc regs
vga_mode_12h:  ; mode 12h, 640*480, 16 colors, planar
	; 11/11/2020
 	;db 	80, 29, 16, 0, 0 ; tw, th-1, ch, slength
	; 09/11/2020
 	db 	80, 29, 16, 00h, 0A0h ; tw, th-1, ch, slength
	db	01h, 0Fh, 00h, 06h ; sequ regs
	db 	0E3h	; misc reg
	db	5Fh, 4Fh, 50h, 82h, 54h, 80h, 0Bh, 3Eh
	; 09/11/2020
	;db	5Fh, 4Fh, 50h, 82h, 53h, 9Fh, 0Bh, 3Eh
	db	00h, 40h, 00h, 00h, 00h, 00h, 00h, 00h
 	db	0EAh, 8Ch, 0DFh, 28h, 00h, 0E7h, 04h, 0E3h
	; 09/11/2020
 	;db	0E9h, 8Bh, 0DFh, 28h, 00h, 0E7h, 04h, 0E3h
	db	0FFh	; crtc regs
	db	00h, 01h, 02h, 03h, 04h, 05h, 14h, 07h
	db	38h, 39h, 3Ah, 3Bh, 3Ch, 3Dh, 3Eh, 3Fh
	db	01h, 00h, 0Fh, 00h  ; actl regs
	db	00h, 00h, 00h, 00h, 00h, 00h, 05h, 0Fh, 0FFh ; grdc regs
vga_mode_6Ah:  ; mode 6Ah, 800*600, 16 colors, planar
	; 11/11/2020
 	db 	100, 36, 16, 00h, 00h ; tw, th-1, ch, slength
	db	01h, 0Fh, 00h, 06h ; sequ regs
	db 	0E3h	; misc reg
	db	7Fh, 63h, 63h, 83h, 6Bh, 1Bh, 72h, 0F0h
	db	00h, 60h, 00h, 00h, 00h, 00h, 00h, 00h
 	db	59h, 8Dh, 57h, 32h, 00h, 57h, 73h, 0E3h
	db	0FFh	; crtc regs
	db	00h, 01h, 02h, 03h, 04h, 05h, 14h, 07h
	db	38h, 39h, 3Ah, 3Bh, 3Ch, 3Dh, 3Eh, 3Fh
	db	01h, 00h, 0Fh, 00h  ; actl regs
	db	00h, 00h, 00h, 00h, 00h, 00h, 05h, 0Fh, 0FFh ; grdc regs
vga_mode_0Dh:  ; mode 0Dh, 320*200, 16 colors, planar
 	db 	40, 24, 8, 00h, 20h ; tw, th-1, ch, slength
	db	09h, 0Fh, 00h, 06h ; sequ regs
	db 	63h	; misc reg
	db	2Dh, 27h, 28h, 90h, 2Bh, 80h, 0BFh, 1Fh
	db	00h, 0C0h, 00h, 00h, 00h, 00h, 00h, 00h
 	db	9Ch, 8Eh, 8Fh, 14h, 00h, 96h, 0B9h, 0E3h
	db	0FFh	; crtc regs
	db	00h, 01h, 02h, 03h, 04h, 05h, 06h, 07h
	db	10h, 11h, 12h, 13h, 14h, 15h, 16h, 17h
	db	01h, 00h, 0Fh, 00h  ; actl regs
	db	00h, 00h, 00h, 00h, 00h, 00h, 05h, 0Fh, 0FFh ; grdc regs
vga_mode_0Eh:  ; mode 0Eh, 640*200, 16 colors, planar
 	db 	80, 24, 8, 00h, 40h ; tw, th-1, ch, slength
	db	01h, 0Fh, 00h, 06h ; sequ regs
	db 	63h	; misc reg
	db	5Fh, 4Fh, 50h, 82h, 54h, 80h, 0BFh, 1Fh
	db	00h, 0C0h, 00h, 00h, 00h, 00h, 00h, 00h
 	db	9Ch, 8Eh, 8Fh, 28h, 00h, 96h, 0B9h, 0E3h
	db	0FFh	; crtc regs
	db	00h, 01h, 02h, 03h, 04h, 05h, 06h, 07h
	db	10h, 11h, 12h, 13h, 14h, 15h, 16h, 17h
	db	01h, 00h, 0Fh, 00h  ; actl regs
	db	00h, 00h, 00h, 00h, 00h, 00h, 05h, 0Fh, 0FFh ; grdc regs
vga_mode_10h: ; mode 10h, 640*350, 16 colors, planar
 	db 	80, 24, 14, 00h, 80h ; tw, th-1, ch, slength
	db	01h, 0Fh, 00h, 06h ; sequ regs
	db 	0A3h	; misc reg
	db	5Fh, 4Fh, 50h, 82h, 54h, 80h, 0BFh, 1Fh
	db	00h, 40h, 00h, 00h, 00h, 00h, 00h, 00h
 	db	83h, 85h, 5Dh, 28h, 0Fh, 63h, 0BAh, 0E3h
	db	0FFh	; crtc regs
	db	00h, 01h, 02h, 03h, 04h, 05h, 14h, 07h
	db	38h, 39h, 3Ah, 3Bh, 3Ch, 3Dh, 3Eh, 3Fh
	db	01h, 00h, 0Fh, 00h  ; actl regs
	db	00h, 00h, 00h, 00h, 00h, 00h, 05h, 0Fh, 0FFh ; grdc regs
vga_mode_11h: ; mode 11h, 640*480, mono color, planar
 	; 11/11/2020
	db 	80, 29, 16, 00h, 0A0h ; tw, th-1, ch, slength
	db	01h, 0Fh, 00h, 06h ; sequ regs
	db 	0E3h	; misc reg
	db	5Fh, 4Fh, 50h, 82h, 54h, 80h, 0Bh, 3Eh
	db	00h, 40h, 00h, 00h, 00h, 00h, 00h, 00h
 	db	0EAh, 8Ch, 0DFh, 28h, 00h, 0E7h, 04h, 0C3h ; 11/11/2020
	db	0FFh	; crtc regs
	db	00h, 3Fh, 00h, 3Fh, 00h, 3Fh, 00h, 3Fh
	db	00h, 3Fh, 00h, 3Fh, 00h, 3Fh, 00h, 3Fh
	db	01h, 00h, 0Fh, 00h  ; actl regs
	db	00h, 00h, 00h, 00h, 00h, 00h, 05h, 0Fh, 0FFh ; grdc regs
end_of_vga_params:

; /// End Of VIDEO DATA ///

; 23/11/2020
; VBE 2 BOCHS/QEMU emulator extensions 
;	for TRDOS 386 v2 kernel (video bios)  

; vbetables.h by Volker Rupper (02/01/2020)

b_vbe_modes:
	;/* standard VESA modes */	
	dw 100h,  640, 400,  8
	dw 101h,  640, 480,  8
	dw 103h,  800, 600,  8
	dw 105h, 1024, 768,  8
	dw 10Eh,  320, 200, 16
	dw 10Fh,  320, 200, 24
	dw 111h,  640, 480, 16
	dw 112h,  640, 480, 24
	dw 114h,  800, 600, 16
	dw 115h,  800, 600, 24
	dw 117h, 1024, 768, 16
	dw 118h, 1024, 768, 24

;/* BOCHS/PLEX86 'own' mode numbers */
	dw 140h,  320,  200, 32
	dw 141h,  640,  400, 32
	dw 142h,  640,  480, 32
	dw 143h,  800,  600, 32
	dw 144h, 1024,  768, 32
	dw 146h,  320,  200,  8
	dw 18Dh, 1280,  720, 16
	dw 18Eh, 1280,  720, 24
	dw 18Fh, 1280,  720, 32
	dw 190h, 1920, 1080, 16
	dw 191h, 1920, 1080, 24
	dw 192h, 1920, 1080, 32

end_of_b_vbe_modes:

MA1 equ VBE_MODE_ATTRIBUTE_SUPPORTED
MA2 equ VBE_MODE_ATTRIBUTE_EXTENDED_INFO_AVAILABLE
MA3 equ VBE_MODE_ATTRIBUTE_COLOR_MODE
MA4 equ VBE_MODE_ATTRIBUTE_LINEAR_FRAME_BUFFER_MODE
MA5 equ	VBE_MODE_ATTRIBUTE_GRAPHICS_MODE

MODE_ATTRIBUTES equ MA1|MA2|MA3|MA4|MA5

WA1 equ VBE_WINDOW_ATTRIBUTE_RELOCATABLE
WA2 equ	VBE_WINDOW_ATTRIBUTE_READABLE
WA3 equ VBE_WINDOW_ATTRIBUTE_WRITEABLE

WINA_ATTRIBUTES equ WA1|WA2|WA3

; 24/11/2020

%if 0

MODE_INFO_LIST: 

; 24/11/2020
; '%if 0' disables 24 mode info tables here, until %endif 
; ('set_mode_info_list' will set only 1 list for selected mode)
; (Purpose: To save about 1 KB kernel size by removing fixed data)

			dw	0100h ; 640x400x8 
ModeAttributes1: 	dw	MODE_ATTRIBUTES
WinAAttributes1: 	db	WINA_ATTRIBUTES
WinBAttributes1: 	db	0
WinGranularity1: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize1: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment1:		dw	VGAMEM_GRAPH
WinBSegment1:		dw	0000h
WinFuncPtr1:		dd	0
BytesPerScanLine1: 	dw	640
XResolution1:		dw	640
YResolution1:		dw	400
XCharSize1:		db	8
YCharSize1:		db	16
NumberOfPlanes1: 	db	1
BitsPerPixel1:		db	8
NumberOfBanks1:		db	4
MemoryModel1:		db	VBE_MEMORYMODEL_PACKED_PIXEL
BankSize1:		db	0
NumberOfImagePages1: 	db	64
Reserved_page1:		db	0
RedMaskSize1:		db	0
RedFieldPosition1: 	db	0
GreenMaskSize1:		db	0
GreenFieldPosition1: 	db	0
BlueMaskSize1:		db	0
BlueFieldPosition1: 	db	0
RsvdMaskSize1: 		db	0
RsvdFieldPosition1: 	db	0
DirectColorModeInfo1: 	db	0
PhysBasePtr1:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset1: 	dd	0
OffScreenMemSize1: 	dw	0
LinBytesPerScanLine1: 	dw	640
BnkNumberOfPages1: 	db	0
LinNumberOfPages1: 	db	0
LinRedMaskSize1:	db	0
LinRedFieldPosition1: 	db	0
LinGreenMaskSize1: 	db	0
LinGreenFieldPosition1: db	0
LinBlueMaskSize1: 	db	0
LinBlueFieldPosition1: 	db	0
LinRsvdMaskSize1: 	db	0
LinRsvdFieldPosition1: 	db	0
MaxPixelClock1:		dd	0

			dw	0101h ; 640x480x8
ModeAttributes2: 	dw	MODE_ATTRIBUTES
WinAAttributes2: 	db	WINA_ATTRIBUTES
WinBAttributes2: 	db	0
WinGranularity2: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize2: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment2:		dw	VGAMEM_GRAPH
WinBSegment2:		dw	0000h
WinFuncPtr2:		dd	0
BytesPerScanLine2:	dw	640
XResolution2:		dw	640
YResolution2:		dw	480
XCharSize2:		db	8
YCharSize2:		db	16
NumberOfPlanes2:	db	1
BitsPerPixel2:		db	8
NumberOfBanks2:		db	5
MemoryModel2:		db	VBE_MEMORYMODEL_PACKED_PIXEL
BankSize2:		db	0
NumberOfImagePages2:	db	53
Reserved_page2:		db	0
RedMaskSize2:		db	0
RedFieldPosition2:	db	0
GreenMaskSize2:		db	0
GreenFieldPosition2:	db	0
BlueMaskSize2:		db	0
BlueFieldPosition2:	db	0
RsvdMaskSize2:		db	0
RsvdFieldPosition2:	db	0
DirectColorModeInfo2:	db	0
PhysBasePtr2:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset2:	dd	0
OffScreenMemSize2:	dw	0
LinBytesPerScanLine2:	dw	640
BnkNumberOfPages2: 	db	0
LinNumberOfPages2: 	db	0
LinRedMaskSize2:	db	0
LinRedFieldPosition2: 	db	0
LinGreenMaskSize2: 	db	0
LinGreenFieldPosition2: db	0
LinBlueMaskSize2: 	db	0
LinBlueFieldPosition2: 	db	0
LinRsvdMaskSize2: 	db	0
LinRsvdFieldPosition2: 	db	0
MaxPixelClock2:		dd	0
			
			dw	0103h ; 800x600x8
ModeAttributes3: 	dw	MODE_ATTRIBUTES
WinAAttributes3: 	db	WINA_ATTRIBUTES
WinBAttributes3: 	db	0
WinGranularity3: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize3: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment3:		dw	VGAMEM_GRAPH
WinBSegment3:		dw	0000h
WinFuncPtr3:		dd	0
BytesPerScanLine3:	dw	800
XResolution3:		dw	800
YResolution3:		dw	600
XCharSize3:		db	8
YCharSize3:		db	16
NumberOfPlanes3:	db	1
BitsPerPixel3:		db	8
NumberOfBanks3:		db	8
MemoryModel3:		db	VBE_MEMORYMODEL_PACKED_PIXEL
BankSize3:		db	0
NumberOfImagePages3:	db	33
Reserved_page3:		db	0
RedMaskSize3:		db	0
RedFieldPosition3:	db	0
GreenMaskSize3:		db	0
GreenFieldPosition3:	db	0
BlueMaskSize3:		db	0
BlueFieldPosition3:	db	0
RsvdMaskSize3:		db	0
RsvdFieldPosition3:	db	0
DirectColorModeInfo3:	db	0
PhysBasePtr3:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset3:	dd	0
OffScreenMemSize3:	dw	0
LinBytesPerScanLine3:	dw	800
BnkNumberOfPages3: 	db	0
LinNumberOfPages3: 	db	0
LinRedMaskSize3:	db	0
LinRedFieldPosition3: 	db	0
LinGreenMaskSize3: 	db	0
LinGreenFieldPosition: 	db	0
LinBlueMaskSize: 	db	0
LinBlueFieldPosition: 	db	0
LinRsvdMaskSize: 	db	0
LinRsvdFieldPosition: 	db	0
MaxPixelClock:		dd	0
			
			dw	0105h ; 1024x768x8
ModeAttributes4: 	dw	MODE_ATTRIBUTES
WinAAttributes4: 	db	WINA_ATTRIBUTES
WinBAttributes4: 	db	0
WinGranularity4: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize4: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment4:		dw	VGAMEM_GRAPH
WinBSegment4:		dw	0000h
WinFuncPtr4:		dd	0
BytesPerScanLine4:	dw	1024
XResolution4:		dw	1024
YResolution4:		dw	768
XCharSize4:		db	8
YCharSize4:		db	16
NumberOfPlanes4:	db	1
BitsPerPixel4:		db	8
NumberOfBanks4:		db	12
MemoryModel4:		db	VBE_MEMORYMODEL_PACKED_PIXEL
BankSize4:		db	0
NumberOfImagePages4:	db	20
Reserved_page4:		db	0
RedMaskSize4:		db	0
RedFieldPosition4:	db	0
GreenMaskSize4:		db	0
GreenFieldPosition4:	db	0
BlueMaskSize4:		db	0
BlueFieldPosition4:	db	0
RsvdMaskSize4:		db	0
RsvdFieldPosition4:	db	0
DirectColorModeInfo4:	db	0
PhysBasePtr4:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset4:	dd	0
OffScreenMemSize4:	dw	0
LinBytesPerScanLine4:	dw	1024
BnkNumberOfPages4: 	db	0
LinNumberOfPages4: 	db	0
LinRedMaskSize4:	db	0
LinRedFieldPosition4: 	db	0
LinGreenMaskSize4: 	db	0
LinGreenFieldPosition4: db	0
LinBlueMaskSize4: 	db	0
LinBlueFieldPosition4: 	db	0
LinRsvdMaskSize4: 	db	0
LinRsvdFieldPosition4: 	db	0
MaxPixelClock4:		dd	0

			dw	010Eh ; 320x200x16
ModeAttributes5: 	dw	MODE_ATTRIBUTES
WinAAttributes5: 	db	WINA_ATTRIBUTES
WinBAttributes5: 	db	0
WinGranularity5: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize5: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment5:		dw	VGAMEM_GRAPH
WinBSegment5:		dw	0000h
WinFuncPtr5:		dd	0
BytesPerScanLine5:	dw	640
XResolution5:		dw	320
YResolution5:		dw	200
XCharSize5:		db	8
YCharSize5:		db	16
NumberOfPlanes5:	db	1
BitsPerPixel5:		db	16
NumberOfBanks5:		db	2
MemoryModel5:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize5:		db	0
NumberOfImagePages5:	db	130
Reserved_page5:		db	0
RedMaskSize5:		db	5
RedFieldPosition5:	db	11
GreenMaskSize5:		db	6
GreenFieldPosition5:	db	5
BlueMaskSize5:		db	5
BlueFieldPosition5:	db	0
RsvdMaskSize5:		db	0
RsvdFieldPosition5:	db	0
DirectColorModeInfo5:	db	0
PhysBasePtr5:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset5:	dd	0
OffScreenMemSize5:	dw	0
LinBytesPerScanLine5:	dw	640
BnkNumberOfPages5: 	db	0
LinNumberOfPages5: 	db	0
LinRedMaskSize5:	db	5
LinRedFieldPosition5:	db	11
LinGreenMaskSize5:	db	6
LinGreenFieldPosition5:	db	5
LinBlueMaskSize5:	db	5
LinBlueFieldPosition5:	db	0
LinRsvdMaskSize5:	db	0
LinRsvdFieldPosition5:	db	0
MaxPixelClock5:		dd	0
	
			dw	010Fh ; 320x200x24
ModeAttributes6: 	dw	MODE_ATTRIBUTES
WinAAttributes6: 	db	WINA_ATTRIBUTES
WinBAttributes6: 	db	0
WinGranularity6: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize6: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment6:		dw	VGAMEM_GRAPH
WinBSegment6:		dw	0000h
WinFuncPtr6:		dd	0
BytesPerScanLine6:	dw	960
XResolution6:		dw	320
YResolution6:		dw	200
XCharSize6:		db	8
YCharSize6:		db	16
NumberOfPlanes6:	db	1
BitsPerPixel6:		db	24
NumberOfBanks6:		db	3
MemoryModel6:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize6:		db	0
NumberOfImagePages6:	db	86
Reserved_page6:		db	0
RedMaskSize6:		db	8
RedFieldPosition6:	db	16
GreenMaskSize6:		db	8
GreenFieldPosition6:	db	8
BlueMaskSize6:		db	8
BlueFieldPosition6:	db	0
RsvdMaskSize6:		db	0
RsvdFieldPosition6:	db	0
DirectColorModeInfo6:	db	0
PhysBasePtr6:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset6:	dd	0
OffScreenMemSize6:	dw	0
LinBytesPerScanLine6:	dw	960
BnkNumberOfPages6: 	db	0
LinNumberOfPages6: 	db	0
LinRedMaskSize6:	db	8
LinRedFieldPosition6:	db	16
LinGreenMaskSize6:	db	8
LinGreenFieldPosition6:	db	8
LinBlueMaskSize6:	db	8
LinBlueFieldPosition6:	db	0
LinRsvdMaskSize6:	db	0
LinRsvdFieldPosition6:	db	0
MaxPixelClock6:		dd	0
	
			dw	0111h ; 640x480x16
ModeAttributes7: 	dw	MODE_ATTRIBUTES
WinAAttributes7: 	db	WINA_ATTRIBUTES
WinBAttributes7: 	db	0
WinGranularity7: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize7: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment7:		dw	VGAMEM_GRAPH
WinBSegment7:		dw	0000h
WinFuncPtr7:		dd	0
BytesPerScanLine7:	dw	1280
XResolution7:		dw	640
YResolution7:		dw	480
XCharSize7:		db	8
YCharSize7:		db	16
NumberOfPlanes7:	db	1
BitsPerPixel7:		db	16
NumberOfBanks7:		db	10
MemoryModel7:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize7:		db	0
NumberOfImagePages7:	db	26
Reserved_page7:		db	0
RedMaskSize7:		db	5
RedFieldPosition7:	db	11
GreenMaskSize7:		db	6
GreenFieldPosition7:	db	5
BlueMaskSize7:		db	5
BlueFieldPosition7:	db	0
RsvdMaskSize7:		db	0
RsvdFieldPosition7:	db	0
DirectColorModeInfo7:	db	0
PhysBasePtr7:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS,
OffScreenMemOffset7:	dd	0
OffScreenMemSize7:	dw	0
LinBytesPerScanLine7:	dw	1280
BnkNumberOfPages7: 	db	0
LinNumberOfPages7: 	db	0
LinRedMaskSize7:	db	5
LinRedFieldPosition7:	db	11
LinGreenMaskSize7:	db	6
LinGreenFieldPosition7:	db	5
LinBlueMaskSize7:	db	5
LinBlueFieldPosition7:	db	0
LinRsvdMaskSize7:	db	0
LinRsvdFieldPosition7:	db	0
MaxPixelClock7:		dd	0

			dw	0112h ; 640x480x24
ModeAttributes8: 	dw	MODE_ATTRIBUTES
WinAAttributes8: 	db	WINA_ATTRIBUTES
WinBAttributes8: 	db	0
WinGranularity8: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize8: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment8:		dw	VGAMEM_GRAPH
WinBSegment8:		dw	0000h
WinFuncPtr8:		dd	0
BytesPerScanLine8:	dw	1920
XResolution8:		dw	640
YResolution8:		dw	480
XCharSize8:		db	8
YCharSize8:		db	16
NumberOfPlanes8:	db	1
BitsPerPixel8:		db	24
NumberOfBanks8:		db	15
MemoryModel8:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize8:		db	0
NumberOfImagePages8:	db	17
Reserved_page8:		db	0
RedMaskSize8:		db	8
RedFieldPosition8:	db	16
GreenMaskSize8:		db	8
GreenFieldPosition8:	db	8
BlueMaskSize8:		db	8
BlueFieldPosition8:	db	0
RsvdMaskSize8:		db	0
RsvdFieldPosition8:	db	0
DirectColorModeInfo8:	db	0
PhysBasePtr8:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset8:	dd	0
OffScreenMemSize8:	dw	0
LinBytesPerScanLine8:	dw	1920
BnkNumberOfPages8: 	db	0
LinNumberOfPages8: 	db	0
LinRedMaskSize8:	db	8
LinRedFieldPosition8:	db	16
LinGreenMaskSize8:	db	8
LinGreenFieldPosition8:	db	8
LinBlueMaskSize8:	db	8
LinBlueFieldPosition8:	db	0
LinRsvdMaskSize8:	db	0
LinRsvdFieldPosition8:	db	0
MaxPixelClock8:		dd	0

			dw	0114h ; 800x600x16
ModeAttributes9: 	dw	MODE_ATTRIBUTES
WinAAttributes9: 	db	WINA_ATTRIBUTES
WinBAttributes9: 	db	0
WinGranularity9: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize9: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment9:		dw	VGAMEM_GRAPH
WinBSegment9:		dw	0000h
WinFuncPtr9:		dd	0
BytesPerScanLine9:	dw	1600
XResolution9:		dw	800
YResolution9:		dw	600
XCharSize9:		db	8
YCharSize9:		db	16
NumberOfPlanes9:	db	1
BitsPerPixel9:		db	16
NumberOfBanks9:		db	15
MemoryModel9:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize9:		db	0
NumberOfImagePages9:	db	16
Reserved_page9:		db	0
RedMaskSize9:		db	5
RedFieldPosition9:	db	11
GreenMaskSize9:		db	6
GreenFieldPosition9:	db	5
BlueMaskSize9:		db	5
BlueFieldPosition9:	db	0
RsvdMaskSize9:		db	0
RsvdFieldPosition9:	db	0
DirectColorModeInfo9:	db	0
PhysBasePtr9:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset9:	dd	0
OffScreenMemSize9:	dw	0
LinBytesPerScanLine9:	dw	1600
BnkNumberOfPages9: 	db	0
LinNumberOfPages9: 	db	0
LinRedMaskSize9:	db	5
LinRedFieldPosition9:	db	11
LinGreenMaskSize9:	db	6
LinGreenFieldPosition9:	db	5
LinBlueMaskSize9:	db	5
LinBlueFieldPosition9:	db	0
LinRsvdMaskSize9:	db	0
LinRsvdFieldPosition9:	db	0
MaxPixelClock9:		dd	0
	
			dw	0115h ; 800x600x24
ModeAttributes10: 	dw	MODE_ATTRIBUTES
WinAAttributes10: 	db	WINA_ATTRIBUTES
WinBAttributes10: 	db	0
WinGranularity10: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize10: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment10:		dw	VGAMEM_GRAPH
WinBSegment10:		dw	0000h
WinFuncPtr10:		dd	0
BytesPerScanLine10:	dw	2400
XResolution10:		dw	800
YResolution10:		dw	600
XCharSize10:		db	8
YCharSize10:		db	16
NumberOfPlanes10:	db	1
BitsPerPixel10:		db	24
NumberOfBanks10:	db	22
MemoryModel10:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize10:		db	0
NumberOfImagePages10:	db	10
Reserved_page10:	db	0
RedMaskSize10:		db	8
RedFieldPosition10:	db	16
GreenMaskSize10:	db	8
GreenFieldPosition10:	db	8
BlueMaskSize10:		db	8
BlueFieldPosition10:	db	0
RsvdMaskSize10:		db	0
RsvdFieldPosition10:	db	0
DirectColorModeInfo10:	db	0
PhysBasePtr10:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset10:	dd	0
OffScreenMemSize10:	dw	0
LinBytesPerScanLine10:	dw	2400
BnkNumberOfPages10: 	db	0
LinNumberOfPages10: 	db	0
LinRedMaskSize10:	db	8
LinRedFieldPosition10:	db	16
LinGreenMaskSize10:	db	8
LinGreenFieldPosition10:db	8
LinBlueMaskSize10:	db	8
LinBlueFieldPosition10:	db	0
LinRsvdMaskSize10:	db	0
LinRsvdFieldPosition10:	db	0
MaxPixelClock10:	dd	0

			dw	0117h ; 1024x768x16
ModeAttributes11: 	dw	MODE_ATTRIBUTES
WinAAttributes11: 	db	WINA_ATTRIBUTES
WinBAttributes11: 	db	0
WinGranularity11: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize11: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment11:		dw	VGAMEM_GRAPH
WinBSegment11:		dw	0000h
WinFuncPtr11:		dd	0
BytesPerScanLine11:	dw	2048
XResolution11:		dw	1024
YResolution11:		dw	768
XCharSize11:		db	8
YCharSize11:		db	16
NumberOfPlanes11:	db	1
BitsPerPixel11:		db	16
NumberOfBanks11:	db	24
MemoryModel11:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize11:		db	0
NumberOfImagePages11:	db	9
Reserved_page11:	db	0
RedMaskSize11:		db	5
RedFieldPosition11:	db	11
GreenMaskSize11:	db	6
GreenFieldPosition11:	db	5
BlueMaskSize11:		db	5
BlueFieldPosition11:	db	0
RsvdMaskSize11:		db	0
RsvdFieldPosition11:	db	0
DirectColorModeInfo11:	db	0
PhysBasePtr11:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS,
OffScreenMemOffset11:	dd	0
OffScreenMemSize11:	dw	0
LinBytesPerScanLine11:	dw	2048
BnkNumberOfPages11: 	db	0
LinNumberOfPages11: 	db	0
LinRedMaskSize11:	db	5
LinRedFieldPosition11:	db	11
LinGreenMaskSize11:	db	6
LinGreenFieldPosition11:db	5
LinBlueMaskSize11:	db	5
LinBlueFieldPosition11:	db	0
LinRsvdMaskSize11:	db	0
LinRsvdFieldPosition11:	db	0
MaxPixelClock11:	dd	0

			dw	0118h ; 1024x768x24
ModeAttributes12: 	dw	MODE_ATTRIBUTES
WinAAttributes12: 	db	WINA_ATTRIBUTES
WinBAttributes12: 	db	0
WinGranularity12: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize12: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment12:		dw	VGAMEM_GRAPH
WinBSegment12:		dw	0000h
WinFuncPtr12:		dd	0
BytesPerScanLine12:	dw	3072
XResolution12:		dw	1024
YResolution12:		dw	768
XCharSize12:		db	8
YCharSize12:		db	16
NumberOfPlanes12:	db	1
BitsPerPixel12:		db	24
NumberOfBanks12:	db	36
MemoryModel12:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize12:		db	0
NumberOfImagePages12:	db	6
Reserved_page12:	db	0
RedMaskSize12:		db	8
RedFieldPosition12:	db	16
GreenMaskSize12:	db	8
GreenFieldPosition12:	db	8
BlueMaskSize12:		db	8
BlueFieldPosition12:	db	0
RsvdMaskSize12:		db	0
RsvdFieldPosition12:	db	0
DirectColorModeInfo12:	db	0
PhysBasePtr12:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset12:	dd	0
OffScreenMemSize12:	dw	0
LinBytesPerScanLine12:	dw	3072
BnkNumberOfPages12: 	db	0
LinNumberOfPages12: 	db	0
LinRedMaskSize12:	db	8
LinRedFieldPosition12:	db	16
LinGreenMaskSize12:	db	8
LinGreenFieldPosition12:db	8
LinBlueMaskSize12:	db	8
LinBlueFieldPosition12:	db	0
LinRsvdMaskSize12:	db	0
LinRsvdFieldPosition12:	db	0
MaxPixelClock12:	dd	0

			dw	0140h ; 320x200x32
ModeAttributes13: 	dw	MODE_ATTRIBUTES
WinAAttributes13: 	db	WINA_ATTRIBUTES
WinBAttributes13: 	db	0
WinGranularity13: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize13: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment13:		dw	VGAMEM_GRAPH
WinBSegment13:		dw	0000h
WinFuncPtr13:		dd	0
BytesPerScanLine13:	dw	1280
XResolution13:		dw	320
YResolution13:		dw	200
XCharSize13:		db	8
YCharSize13:		db	16
NumberOfPlanes13:	db	1
BitsPerPixel13:		db	32
NumberOfBanks13:	db	4
MemoryModel13:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize13:		db	0
NumberOfImagePages13:	db	64
Reserved_page13:	db	0
RedMaskSize13:		db	8
RedFieldPosition13:	db	16
GreenMaskSize13:	db	8
GreenFieldPosition13:	db	8
BlueMaskSize13:		db	8
BlueFieldPosition13:	db	0
RsvdMaskSize13:		db	8
RsvdFieldPosition13:	db	24
DirectColorModeInfo13:	db	VBE_DIRECTCOLOR_RESERVED_BITS_AVAILABLE
PhysBasePtr13:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset13:	dd	0
OffScreenMemSize13:	dw	0
LinBytesPerScanLine13:	dw	1280
BnkNumberOfPages13: 	db	0
LinNumberOfPages13: 	db	0
LinRedMaskSize13:	db	8
LinRedFieldPosition13:	db	16
LinGreenMaskSize13:	db	8
LinGreenFieldPosition13:db	8
LinBlueMaskSize13:	db	8
LinBlueFieldPosition13:	db	0
LinRsvdMaskSize13:	db	8
LinRsvdFieldPosition13:	db	24
MaxPixelClock13:	dd	0

			dw	0141h ; 640x400x32
ModeAttributes14: 	dw	MODE_ATTRIBUTES
WinAAttributes14: 	db	WINA_ATTRIBUTES
WinBAttributes14: 	db	0
WinGranularity14: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize14: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment14:		dw	VGAMEM_GRAPH
WinBSegment14:		dw	0000h
WinFuncPtr14:		dd	0
BytesPerScanLine14:	dw	2560
XResolution14:		dw	640
YResolution14:		dw	400
XCharSize14:		db	8
YCharSize14:		db	16
NumberOfPlanes14:	db	1
BitsPerPixel14:		db	32
NumberOfBanks14:	db	16
MemoryModel14:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize14:		db	0
NumberOfImagePages14:	db	15
Reserved_page14:	db	0
RedMaskSize14:		db	8
RedFieldPosition14:	db	16
GreenMaskSize14:	db	8
GreenFieldPosition14:	db	8
BlueMaskSize14:		db	8
BlueFieldPosition14:	db	0
RsvdMaskSize14:		db	8
RsvdFieldPosition14:	db	24
DirectColorModeInfo14:	db	VBE_DIRECTCOLOR_RESERVED_BITS_AVAILABLE
PhysBasePtr14:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset14:	dd  	0
OffScreenMemSize14:	dw	0
LinBytesPerScanLine14:	dw	2560
BnkNumberOfPages14: 	db	0
LinNumberOfPages14: 	db	0
LinRedMaskSize14:	db	8
LinRedFieldPosition14:	db	16
LinGreenMaskSize14:	db	8
LinGreenFieldPosition14:db	8
LinBlueMaskSize14:	db	8
LinBlueFieldPosition14:	db	0
LinRsvdMaskSize14:	db	8
LinRsvdFieldPosition14:	db	24
MaxPixelClock14:	dd	0

			dw	0142 ; 640x480x32
ModeAttributes15: 	dw	MODE_ATTRIBUTES
WinAAttributes15: 	db	WINA_ATTRIBUTES
WinBAttributes15: 	db	0
WinGranularity15: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize15: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment15:		dw	VGAMEM_GRAPH
WinBSegment15:		dw	0000h
WinFuncPtr15:		dd	0
BytesPerScanLine15:	dw	2560
XResolution15:		dw	640
YResolution15:		dw	480
XCharSize15:		db	8
YCharSize15:		db	16
NumberOfPlanes15:	db	1
BitsPerPixel15:		db	32
NumberOfBanks15:	db	19
MemoryModel15:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize15:		db	0
NumberOfImagePages15:	db	12
Reserved_page15:	db	0
RedMaskSize15:		db	8
RedFieldPosition15:	db	16
GreenMaskSize15:	db	8
GreenFieldPosition15:	db	8
BlueMaskSize15:		db	8
BlueFieldPosition15:	db	0
RsvdMaskSize15:		db	8
RsvdFieldPosition15:	db	24
DirectColorModeInfo15:	db	VBE_DIRECTCOLOR_RESERVED_BITS_AVAILABLE,
PhysBasePtr15:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS,
OffScreenMemOffset15:	dd	0
OffScreenMemSize15:	dw	0
LinBytesPerScanLine15:	dw	2560
BnkNumberOfPages15: 	db	0
LinNumberOfPages15: 	db	0
LinRedMaskSize15:	db	8
LinRedFieldPosition15:	db	16
LinGreenMaskSize15:	db	8
LinGreenFieldPosition15:db	8
LinBlueMaskSize15:	db	8
LinBlueFieldPosition15:	db	0
LinRsvdMaskSize15:	db	8
LinRsvdFieldPosition15:	db	24
MaxPixelClock15:	dd	0

			dw	0143h ; 800x600x32
ModeAttributes16: 	dw	MODE_ATTRIBUTES
WinAAttributes16: 	db	WINA_ATTRIBUTES
WinBAttributes16: 	db	0
WinGranularity16: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize16: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment16:		dw	VGAMEM_GRAPH
WinBSegment16:		dw	0000h
WinFuncPtr16:		dd	0
BytesPerScanLine16:	dw	3200
XResolution16:		dw	800
YResolution16:		dw	600
XCharSize16:		db	8
YCharSize16:		db	16
NumberOfPlanes16:	db	1
BitsPerPixel16:		db	32
NumberOfBanks16:	db	30
MemoryModel16:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize16:		db	0
NumberOfImagePages16:	db	7
Reserved_page16:	db	0
RedMaskSize16:		db	8
RedFieldPosition16:	db	16
GreenMaskSize16:	db	8
GreenFieldPosition16:	db	8
BlueMaskSize16:		db	8
BlueFieldPosition16:	db	0
RsvdMaskSize16:		db	8
RsvdFieldPosition16:	db	24
DirectColorModeInfo16:	db	VBE_DIRECTCOLOR_RESERVED_BITS_AVAILABLE,
PhysBasePtr16:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS,
OffScreenMemOffset16:	dd	0
OffScreenMemSize16:	dw	0
LinBytesPerScanLine16:	dw	3200
BnkNumberOfPages16: 	db	0
LinNumberOfPages16: 	db	0
LinRedMaskSize16:	db	8
LinRedFieldPosition16:	db	16
LinGreenMaskSize16:	db	8
LinGreenFieldPosition16:db	8
LinBlueMaskSize16:	db	8
LinBlueFieldPosition16:	db	0
LinRsvdMaskSize16:	db	8
LinRsvdFieldPosition16:	db	24
MaxPixelClock16:	dd	0
	
			dw	0144h ; 1024x768x32
ModeAttributes17: 	dw	MODE_ATTRIBUTES
WinAAttributes17: 	db	WINA_ATTRIBUTES
WinBAttributes17: 	db	0
WinGranularity17: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize17: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment17:		dw	VGAMEM_GRAPH
WinBSegment17:		dw	0000h
WinFuncPtr17:		dd	0
BytesPerScanLine17:	dw	4096
XResolution17:		dw	1024
YResolution17:		dw	768
XCharSize17:		db	8
YCharSize17:		db	16
NumberOfPlanes17:	db	1
BitsPerPixel17:		db	32
NumberOfBanks17:	db	48
MemoryModel17:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize17:		db	0
NumberOfImagePages17:	db	4
Reserved_page17:	db	0
RedMaskSize17:		db	8
RedFieldPosition17:	db	16
GreenMaskSize17:	db	8
GreenFieldPosition17:	db	8
BlueMaskSize17:		db	8
BlueFieldPosition17:	db	0
RsvdMaskSize17:		db	8
RsvdFieldPosition17:	db	24
DirectColorModeInfo17:	db	VBE_DIRECTCOLOR_RESERVED_BITS_AVAILABLE
PhysBasePtr17:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset17:	dd	0
OffScreenMemSize17:	dw	0
LinBytesPerScanLine17:	dw	4096
BnkNumberOfPages17: 	db	0
LinNumberOfPages17: 	db	0
LinRedMaskSize17:	db	8
LinRedFieldPosition17:	db	16
LinGreenMaskSize17:	db	8
LinGreenFieldPosition17:db	8
LinBlueMaskSize17:	db	8
LinBlueFieldPosition17:	db	0
LinRsvdMaskSize17:	db	8
LinRsvdFieldPosition17:	db	24
MaxPixelClock17:	dd	0
	
			dw	0146h ; 320x200x8
ModeAttributes18: 	dw	MODE_ATTRIBUTES
WinAAttributes18: 	db	WINA_ATTRIBUTES
WinBAttributes18: 	db	0
WinGranularity18: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize18: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment18:		dw	VGAMEM_GRAPH
WinBSegment18:		dw	0000h
WinFuncPtr18:		dd	0
BytesPerScanLine18:	dw	320
XResolution18:		dw	320
YResolution18:		dw	200
XCharSize18:		db	8
YCharSize18:		db	16
NumberOfPlanes18:	db	1
BitsPerPixel18:		db	8
NumberOfBanks18:	db	1
MemoryModel18:		db	VBE_MEMORYMODEL_PACKED_PIXEL
BankSize18:		db	0
NumberOfImagePages18:	db	255 ; 261 in vbetables.h (03/01/2020) !
Reserved_page18:	db	0
RedMaskSize18:		db	0
RedFieldPosition18:	db	0
GreenMaskSize18:	db	0
GreenFieldPosition18:	db	0
BlueMaskSize18:		db	0
BlueFieldPosition18:	db	0
RsvdMaskSize18:		db	0
RsvdFieldPosition18:	db	0
DirectColorModeInfo18:	db	0
PhysBasePtr18:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset18:	dd	0
OffScreenMemSize18:	dw	0
LinBytesPerScanLine18:	dw	320
BnkNumberOfPages18: 	db	0
LinNumberOfPages18: 	db	0
LinRedMaskSize18:	db	0
LinRedFieldPosition18: 	db	0
LinGreenMaskSize18: 	db	0
LinGreenFieldPosition18:db	0
LinBlueMaskSize18: 	db	0
LinBlueFieldPosition18: db	0
LinRsvdMaskSize18: 	db	0
LinRsvdFieldPosition18: db	0
MaxPixelClock18:	dd	0

			dw	018Dh ; 1280x720x16
ModeAttributes19: 	dw	MODE_ATTRIBUTES
WinAAttributes19: 	db	WINA_ATTRIBUTES
WinBAttributes19: 	db	0
WinGranularity19: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize19: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment19:		dw	VGAMEM_GRAPH
WinBSegment19:		dw	0000h
WinFuncPtr19:		dd	0
BytesPerScanLine19:	dw	2560
XResolution19:		dw	1280
YResolution19:		dw	720
XCharSize19:		db	8
YCharSize19:		db	16
NumberOfPlanes19:	db	1
BitsPerPixel19:		db	16
NumberOfBanks19:	db	29
MemoryModel19:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize19:		db	0
NumberOfImagePages19:	db	8
Reserved_page19:	db	0
RedMaskSize19:		db	5
RedFieldPosition19:	db	11
GreenMaskSize19:	db	6
GreenFieldPosition19:	db	5
BlueMaskSize19:		db	5
BlueFieldPosition19:	db	0
RsvdMaskSize19:		db	0
RsvdFieldPosition19:	db	0
DirectColorModeInfo19:	db	0
PhysBasePtr19:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset19:	dd	0
OffScreenMemSize19:	dw	0
LinBytesPerScanLine19:	dw	2560
BnkNumberOfPages19: 	db	0
LinNumberOfPages19: 	db	0
LinRedMaskSize19:	db	5
LinRedFieldPosition19:	db	11
LinGreenMaskSize19:	db	6
LinGreenFieldPosition19:db	5
LinBlueMaskSize19:	db	5
LinBlueFieldPosition19:	db	0
LinRsvdMaskSize19:	db	0
LinRsvdFieldPosition19:	db	0
MaxPixelClock19:	dd	0

			dw	018Eh ; 1280x720x24
ModeAttributes20: 	dw	MODE_ATTRIBUTES
WinAAttributes20: 	db	WINA_ATTRIBUTES
WinBAttributes20: 	db	0
WinGranularity20: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize20: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment20:		dw	VGAMEM_GRAPH
WinBSegment20:		dw	0000h
WinFuncPtr20:		dd	0
BytesPerScanLine20:	dw	3840
XResolution20:		dw	1280
YResolution20:		dw	720
XCharSize20:		db	8
YCharSize20:		db	16
NumberOfPlanes20:	db	1
BitsPerPixel20:		db	24
NumberOfBanks20:	db	43
MemoryModel20:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize20:		db	0
NumberOfImagePages20:	db	5
Reserved_page20:	db	0
RedMaskSize20:		db	8
RedFieldPosition20:	db	16
GreenMaskSize20:	db	8
GreenFieldPosition20:	db	8
BlueMaskSize20:		db	8
BlueFieldPosition20:	db	0
RsvdMaskSize20:		db	0
RsvdFieldPosition20:	db	0
DirectColorModeInfo20:	db	0
PhysBasePtr20:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset20:	dd	0
OffScreenMemSize20:	dw	0
LinBytesPerScanLine20:	dw	3840
BnkNumberOfPages20: 	db	0
LinNumberOfPages20: 	db	0
LinRedMaskSize20:	db	8
LinRedFieldPosition20:	db	16
LinGreenMaskSize20:	db	8
LinGreenFieldPosition20:db	8
LinBlueMaskSize20:	db	8
LinBlueFieldPosition20:	db	0
LinRsvdMaskSize20:	db	0
LinRsvdFieldPosition20:	db	0
MaxPixelClock20:	dd	0
	
			dw	018Fh ; 1280x720x32
ModeAttributes21: 	dw	MODE_ATTRIBUTES
WinAAttributes21: 	db	WINA_ATTRIBUTES
WinBAttributes21: 	db	0
WinGranularity21: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize21: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment21:		dw	VGAMEM_GRAPH
WinBSegment21:		dw	0000h
WinFuncPtr21:		dd	0
BytesPerScanLine21:	dw	5120
XResolution21:		dw	1280
YResolution21:		dw	720
XCharSize21:		db	8
YCharSize21:		db	16
NumberOfPlanes21:	db	1
BitsPerPixel21:		db	32
NumberOfBanks21:	db	57
MemoryModel21:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize21:		db	0
NumberOfImagePages21:	db	3
Reserved_page21:	db	0
RedMaskSize21:		db	8
RedFieldPosition21:	db	16
GreenMaskSize21:	db	8
GreenFieldPosition21:	db	8
BlueMaskSize21:		db	8
BlueFieldPosition21:	db	0
RsvdMaskSize21:		db	8
RsvdFieldPosition21:	db	24
DirectColorModeInfo21:	db	VBE_DIRECTCOLOR_RESERVED_BITS_AVAILABLE
PhysBasePtr21:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset21:	dd	0
OffScreenMemSize21:	dw	0
LinBytesPerScanLine21:	dw	5120
BnkNumberOfPages21: 	db	0
LinNumberOfPages21: 	db	0
LinRedMaskSize21:	db	8
LinRedFieldPosition21:	db	16
LinGreenMaskSize21:	db	8
LinGreenFieldPosition21:db	8
LinBlueMaskSize21:	db	8
LinBlueFieldPosition21:	db	0
LinRsvdMaskSize21:	db	8
LinRsvdFieldPosition21:	db	24
MaxPixelClock21:	dd	0
			
			dw	0190h ; 1920x1080x16
ModeAttributes22: 	dw	MODE_ATTRIBUTES
WinAAttributes22: 	db	WINA_ATTRIBUTES
WinBAttributes22: 	db	0
WinGranularity22: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize22: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment22:		dw	VGAMEM_GRAPH
WinBSegment22:		dw	0000h
WinFuncPtr22:		dd	0
BytesPerScanLine22:	dw	3840
XResolution22:		dw	1920
YResolution22:		dw	1080
XCharSize22:		db	8
YCharSize22:		db	16
NumberOfPlanes22:	db	1
BitsPerPixel22:		db	16
NumberOfBanks22:	db	64
MemoryModel22:		db	VBE_MEMORYMODEL_DIRECT_COLOR,
BankSize22:		db	0
NumberOfImagePages22:	db	3
Reserved_page22:	db	0
RedMaskSize22:		db	5
RedFieldPosition22:	db	11
GreenMaskSize22:	db	6
GreenFieldPosition22:	db	5
BlueMaskSize22:		db	5
BlueFieldPosition22:	db	0
RsvdMaskSize22:		db	0
RsvdFieldPosition22:	db	0
DirectColorModeInfo22:	db	0
PhysBasePtr22:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset22:	dd	0
OffScreenMemSize22:	dw	0
LinBytesPerScanLine22:	dw	3840
BnkNumberOfPages22: 	db	0
LinNumberOfPages22: 	db	0
LinRedMaskSize22:	db	5
LinRedFieldPosition22:	db	11
LinGreenMaskSize22:	db	6
LinGreenFieldPosition22:db	5
LinBlueMaskSize22:	db	5
LinBlueFieldPosition22:	db	0
LinRsvdMaskSize22:	db	0
LinRsvdFieldPosition22:	db	0
MaxPixelClock22:	dd	0
			
			dw	0191h ; 1920x1080x24
ModeAttributes23: 	dw	MODE_ATTRIBUTES
WinAAttributes23: 	db	WINA_ATTRIBUTES
WinBAttributes23: 	db	0
WinGranularity23: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize23: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment23:		dw	VGAMEM_GRAPH
WinBSegment23:		dw	0000h
WinFuncPtr23:		dd	0
BytesPerScanLine23:	dw	5760
XResolution23:		dw	1920
YResolution23:		dw	1080
XCharSize23:		db	8
YCharSize23:		db	16
NumberOfPlanes23:	db	1
BitsPerPixel23:		db	24
NumberOfBanks23:	db	95
MemoryModel23:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize23:		db	0
NumberOfImagePages23:	db	1
Reserved_page23:	db	0
RedMaskSize23:		db	8
RedFieldPosition23:	db	16
GreenMaskSize23:	db	8
GreenFieldPosition23:	db	8
BlueMaskSize23:		db	8
BlueFieldPosition23:	db	0
RsvdMaskSize23:		db	0
RsvdFieldPosition23:	db	0
DirectColorModeInfo23:	db	0
PhysBasePtr23:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset23:	dd	0
OffScreenMemSize23:	dw	0
LinBytesPerScanLine23:	dw	5760
BnkNumberOfPages23: 	db	0
LinNumberOfPages23: 	db	0
LinRedMaskSize23:	db	8
LinRedFieldPosition23:	db	16
LinGreenMaskSize23:	db	8
LinGreenFieldPosition23:db	8
LinBlueMaskSize23:	db	8
LinBlueFieldPosition23:	db	0
LinRsvdMaskSize23:	db	0
LinRsvdFieldPosition23:	db	0
MaxPixelClock23:	dd	0

			dw	0192h ; 1920x1080x32
ModeAttributes24: 	dw	MODE_ATTRIBUTES
WinAAttributes24: 	db	WINA_ATTRIBUTES
WinBAttributes24: 	db	0
WinGranularity24: 	dw	VBE_DISPI_BANK_SIZE_KB
WinSize24: 		dw	VBE_DISPI_BANK_SIZE_KB
WinASegment24:		dw	VGAMEM_GRAPH
WinBSegment24:		dw	0000h
WinFuncPtr24:		dd	0
BytesPerScanLine24:	dw	7680
XResolution24:		dw	1920
YResolution24:		dw	1080
XCharSize24:		db	8
YCharSize24:		db	16
NumberOfPlanes24:	db	1
BitsPerPixel24:		db	32
NumberOfBanks24:	db	127
MemoryModel24:		db	VBE_MEMORYMODEL_DIRECT_COLOR
BankSize24:		db	0
NumberOfImagePages24:	db	1
Reserved_page24:	db	0
RedMaskSize24:		db	8
RedFieldPosition24:	db	16
GreenMaskSize24:	db	8
GreenFieldPosition24:	db	8
BlueMaskSize24:		db	8
BlueFieldPosition24:	db	0
RsvdMaskSize24:		db	8
RsvdFieldPosition24:	db	24
DirectColorModeInfo24:	db	VBE_DIRECTCOLOR_RESERVED_BITS_AVAILABLE
PhysBasePtr24:		dd	VBE_DISPI_LFB_PHYSICAL_ADDRESS
OffScreenMemOffset24:	dd	0
OffScreenMemSize24:	dw	0
LinBytesPerScanLine24:	dw 	7680
BnkNumberOfPages24: 	db	0
LinNumberOfPages24: 	db	0
LinRedMaskSize24:	db	8
LinRedFieldPosition24:	db	16
LinGreenMaskSize24:	db	8
LinGreenFieldPosition24:db	8
LinBlueMaskSize24:	db	8
LinBlueFieldPosition24:	db	0
LinRsvdMaskSize24:	db	8
LinRsvdFieldPosition24:	db	24
MaxPixelClock24:	dd	0

VBE_VESA_MODE_END_OF_LIST: dw	0

%endif

; 24/11/2020

direct_color_fields:
	; 24/11/2020

; (vbetables-gen.c)
; // Direct Color fields 
; (required for direct/6 and YUV/7 memory models)
; switch(pm->depth) {
    
;case 8:
r_size_8:  db 0
r_pos_8:   db 0
g_size_8:  db 0
g_pos_8:   db 0
b_size_8:  db 0
b_pos_8:   db 0
a_size_8:  db 0
a_pos_8:   db 0

;case 16:
r_size_16:  db 5
r_pos_16:   db 11
g_size_16:  db 6
g_pos_16:   db 5
b_size_16:  db 5
b_pos_16:   db 0
a_size_16:  db 0
a_pos_16:   db 0

;case 24:
r_size_24:  db 8
r_pos_24:   db 16
g_size_24:  db 8
g_pos_24:   db 8
b_size_24:  db 8
b_pos_24:   db 0
a_size_24:  db 0
a_pos_24:   db 0

;case 32:
r_size_32:  db 8
r_pos_32:   db 16
g_size_32:  db 8
g_pos_32:   db 8
b_size_32:  db 8
b_pos_32:   db 0
a_size_32:  db 8
a_pos_32:   db 24