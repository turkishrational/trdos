; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.10 - video.s
; ----------------------------------------------------------------------------
; Last Update: 22/12/2025 (Previous: 29/11/2023 - Kernel v2.0.7)
; ----------------------------------------------------------------------------
; Beginning: 16/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; video.inc (13/08/2015)
;
; Derived from 'IBM PC-AT' BIOS source code (1985) 
; ****************************************************************************

; Retro UNIX 386 v1 Kernel - VIDEO.INC
; Last Modification: 13/08/2015
;		  (Video Data is in 'VIDATA.INC')
;
; ///////// VIDEO (CGA) FUNCTIONS ///////////////

; 16/01/2016 (32 bit modifications, TRDOS386 - TRDOS v2.0, video.s)
; INT 31H (TRDOS 386) = INT 10H (IBM PC/AT REAL MODE)

; IBM PC-AT BIOS Source Code
; TITLE VIDEO1 --- 06/10/85 VIDEO DISPLAY BIOS

_int10h:
	; 23/03/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	pushfd
	push	cs
	call	VIDEO_IO_1
	retn

;--- INT 10 H -------------------------------------------------------------------
; VIDEO_IO									:
;	THESE ROUTINES PROVIDE THE CRT DISPLAY INTERFACE			:
;	THE FOLLOWING FUNCTIONS ARE PROVIDED:					:
;										:
;    (AH)= 00H	SET MODE (AL) CONTAINS MODE VALUE				:
;		(AL) = 00H  40X25 BW MODE (POWER ON DEFAULT)			:
;		(AL) = 01H  40X25 COLOR						:
;		(AL) = 02H  80X25 BW						:
;		(AL) = 03H  80X25 COLOR						:
;		              GRAPHICS MODES					:
;		(AL) = 04H  320X200 COLOR					:
;		(AL) = 05H  320X200 BW MODE					:
;		(AL) = 06H  640X200 BW MODE					:
;		(AL) = 07H   80X25 MONOCHROME (USED INTERNAL TO VIDEO ONLY)	:
;		*** NOTES -BW MODES OPERATE SAME AS COLOR MODES, BUT COLOR	:
;		           BURST IS NOT ENABLED					:
;		          -CURSOR IS NOT DISPLAYED IN GRAPHICS MODE		:
;    (AH)= 01H	SET CURSOR TYPE							:
;		(CH) = BITS 4-0 = START LINE FOR CURSOR				:
;		       ** HARDWARE WILL ALWAYS CAUSE BLINK			:
;		       ** SETTING BIT 5 OR 6 WILL CAUSE ERRATIC BLINKING	:
;		          OR NO CURSOR AT ALL					:
;		(CL) = BITS 4-0 = END LINE FOR CURSOR				:
;    (AH)= 02H	SET CURSOR POSITION						:
;		(DH,DL) = ROW,COLUMN  (00H,00H) IS UPPER LEFT			:
;		(BH) = A PAGE NUMBER (MUST BE 00H FOR GRAPHICS MODES)		:
;    (AH)= 03H	READ CURSOR POSITION						:
;		(BH) = PAGE NUMBER (MUST BE 00H FOR GRAPHICS MODES)		:
;		ON EXIT (DH,DL) = ROW,COLUMN OF CURRENT CURSOR			:
;		        (CH,CL) = CURSOR MODE CURRENTLY SET			:
;    (AH)= 04H	READ LIGHT PEN POSITION						:
;		ON EXIT:							:
;		(AH) = 00H -- LIGHT PEN SWITCH NOT DOWN/NOT TRIGGERED		:
;		(AH) = 01H -- VALID LIGHT PEN VALUE IN REGISTERS		:
;		        (DH,DL) = ROW,COLUMN OF CHARACTER LP POSITION		:
;		        (CH) = RASTER LINE (0-199)				:
;		        (BX) = PIXEL COLUMN (0-319,639)				:
;    (AH)= 05H	SELECT ACTIVE DISPLAY PAGE (VALID ONLY FOR ALPHA MODES)		:
;		(AL) = NEW PAGE VALUE (0-7 FOR MODES 0&1, 0-3 FOR MODES 2&3)	:
;    (AH)= 06H	SCROLL ACTIVE PAGE UP						:
;		(AL) = NUMBER OF LINES. ( LINES BLANKED AT BOTTOM OF WINDOW )	:
;		        (AL) = 00H MEANS BLANK ENTIRE WINDOW			:
;		(CH,CL) = ROW,COLUMN OF UPPER LEFT CORNER OF SCROLL		:
;		(DH,DL) = ROW,COLUMN OF LOWER RIGHT CORNER OF SCROLL		:
;		(BH) = ATTRIBUTE TO BE USED ON BLANK LINE			:
;    (AH)= 07H	SCROLL ACTIVE PAGE DOWN						:
;		(AL) = NUMBER OF LINES, INPUT LINES BLANKED AT TOP OF WINDOW	:
;		        (AL) = 00H MEANS BLANK ENTIRE WINDOW			:
;		(CH,CL) = ROW,COLUMN OF UPPER LEFT CORNER OF SCROLL		:
;		(DH,DL) = ROW,COLUMN OF LOWER RIGHT CORNER OF SCROLL		:
;		(BH) = ATTRIBUTE TO BE USED ON BLANK LINE			:
;										:
;   CHARACTER HANDLING ROUTINES							:
;										:
;    (AH)= 08H	READ ATTRIBUTE/CHARACTER AT CURRENT CURSOR POSITION		:
;		(BH) = DISPLAY PAGE (VALID FOR ALPHA MODES ONLY)		:
;		ON EXIT:							:
;		(AL) = CHAR READ						:
;		(AH) = ATTRIBUTE OF CHARACTER READ (ALPHA MODES ONLY)		:
;    (AH)= 09H	WRITE ATTRIBUTE/CHARACTER AT CURRENT CURSOR POSITION		:
;		(BH) = DISPLAY PAGE (VALID FOR ALPHA MODES ONLY)		:
;		(CX) = COUNT OF CHARACTERS TO WRITE				:
;		(AL) = CHAR TO WRITE						:
;		(BL) = ATTRIBUTE OF CHARACTER (ALPHA)/COLOR OF CHAR (GRAPHICS)	:
;		         SEE NOTE ON WRITE DOT FOR BIT 7 OF BL = 1.		:
;    (AH) = 0AH	WRITE CHARACTER ONLY AT CURRENT CURSOR POSITION			:
;		(BH) = DISPLAY PAGE (VALID FOR ALPHA MODES ONLY)		:
;		(CX) = COUNT OF CHARACTERS TO WRITE				:
;		(AL) = CHAR TO WRITE						:
;		       NOTE: USE FUNCTION (AH)= 09H IN GRAPHICS MODES		:
;	FOR READ/WRITE CHARACTER INTERFACE WHILE IN GRAPHICS MODE, THE		:
;		CHARACTERS ARE FORMED FROM A CHARACTER GENERATOR IMAGE		:
;		MAINTAINED IN THE SYSTEM ROM. ONLY THE 1ST 128 CHARS		:
;		ARE CONTAINED THERE. TO READ/WRITE THE SECOND 128 CHARS,	:
;		THE USER MUST INITIALIZE THE POINTER AT INTERRUPT 1FH		:
;		(LOCATION 0007CH) TO POINT TO THE 1K BYTE TABLE CONTAINING	:
;		THE CODE POINTS FOR THE SECOND 128 CHARS (128-255).		:
;	FOR WRITE CHARACTER INTERFACE IN GRAPHICS MODE, THE REPLICATION FACTOR	:
;		CONTAINED IN (CX) ON ENTRY WILL PRODUCE VALID RESULTS ONLY	:
;		FOR CHARACTERS CONTAINED ON THE SAME ROW. CONTINUATION TO	:
;		SUCCEEDING LINES WILL NOT PRODUCE CORRECTLY.			:
;										:
;    GRAPHICS INTERFACE								:
;    (AH)= 0BH	SET COLOR PALETTE						:
;		(BH) = PALETTE COLOR ID BEING SET (0-127)			:
;		(BL) = COLOR VALUE TO BE USED WITH THAT COLOR ID		:
;		       NOTE: FOR THE CURRENT COLOR CARD, THIS ENTRY POINT HAS	:
;		               MEANING ONLY FOR 320X200 GRAPHICS.		:
;		       COLOR ID = 0 SELECTS THE BACKGROUND COLOR (0-15)		:
;		       COLOR ID = 1 SELECTS THE PALETTE TO BE USED:		:
;		               0 = GREEN(1)/RED(2)/YELLOW(3)			:
;		               1 = CYAN(1)/MAGENTA(2)/WHITE(3)			:
;		       IN 40X25 OR 80X25 ALPHA MODES, THE VALUE SET FOR 	:
;		               PALETTE COLOR 0 INDICATES THE BORDER COLOR	:
;		               TO BE USED (VALUES 0-31, WHERE 16-31 SELECT	:
;		               THE HIGH INTENSITY BACKGROUND SET.		:
;    (AH)= 0CH	WRITE DOT							:
;		(DX) = ROW NUMBER						:
;		(CX) = COLUMN NUMBER						:
;		(AL) = COLOR VALUE						:
;		        IF BIT 7 OF AL = 1, THEN THE COLOR VALUE IS EXCLUSIVE	:
;		        ORed WITH THE CURRENT CONTENTS OF THE DOT		:
;    (AH)= ODH	READ DOT							:
;		(DX) = ROW NUMBER						:
;		(CX) = COLUMN NUMBER						:
;		(AL) = RETURNS THE DOT READ					:
;										:
;    ASCII TELETYPE ROUTINE FOR OUTPUT						:
;										:
;    (AH)= 0EH	WRITE TELETYPE TO ACTIVE PAGE					:
;		(AL) = CHAR TO WRITE						:
;		(BL) = FOREGROUND COLOR IN GRAPHICS MODE			:
;		NOTE -- SCREEN WIDTH IS CONTROLLED BY PREVIOUS MODE SET		:
;    (AH)= 0FH	CURRENT VIDEO STATE						:
;		RETURNS THE CURRENT VIDEO STATE					:
;		(AL) = MODE CURRENTLY SET ( SEE (AH)=00H FOR EXPLANATION)	:
;		(AH) = NUMBER OR CHARACTER COLUMNS ON SCREEN			:
;		(BH) = CURRENT ACTIVE DISPLAY PAGE				:
;    (AH)= 10H	RESERVED							:
;    (AH)= 11H	RESERVED							:
;    (AH)= 12H	RESERVED							:
;    (AH)= 13H	WRITE STRING							:
;			ES:BP  -  POINTER T0 STRING TO BE WRITTEN		:
;			CX     -  LENGTH OF CHARACTER STRING TO WRITTEN		:
;			DX     -  CURSOR POSITION FOR STRING TO BE WRITTEN	:
;			BH     -  PAGE NUMBER					:
;		(AL)= 00H	WRITE CHARACTER STRING				:
;			BL     -  ATTRIBUTE					:
;			STRING IS  <CHAR,CHAR, ... ,CHAR>			:
;			CURSOR NOT MOVED					:
;		(AL)= 01H	WRITE CHARACTER STRING AND MOVE CURSOR		:
;			BL     -  ATTRIBUTE					:
;			STRING IS  <CHAR,CHAR, ... ,CHAR>			:
;			CURSOR MOVED						:
;		(AL)= 02H	WRITE CHARACTER AND ATTRIBUTE STRING		:
;			       (VALID FOR ALPHA MODES ONLY)			:
;			STRING IS <CHAR,ATTR,CHAR,ATTR ..  ,CHAR,ATTR>		:
;			CURSOR IS NOT MOVED					:
;		(AL)= 03H WRITE CHARACTER AND ATTRIBUTE STRING AND MOVE CURSOR	:
;			       (VALID FOR ALPHA MODES ONLY)			:
;			STRING IS <CHAR,ATTR,CHAR,ATTR ..  ,CHAR,ATTR>		:
;			CURSOR IS MOVED						:
;		 NOTE:  CARRIAGE RETURN, LINE FEED, BACKSPACE, AND BELL ARE	:
;		        TREATED AS COMMANDS RATHER THAN PRINTABLE CHARACTERS.	:
;										:
;	BX,CX,DX,SI,DI,BP,SP,DS,ES,SS PRESERVED DURING CALLS EXCEPT FOR		:
;	BX,CX,DX RETURN VALUES ON FUNCTIONS 03H,04H,0DH AND 0FH. ON ALL CALLS	:
;	AX IS MODIFIED.								:
;--------------------------------------------------------------------------------

M1:	dd	SET_MODE	; TABLE OF ROUTINES WITHIN VIDEO I/O
	dd	SET_CTYPE
	dd	SET_CPOS
	dd	READ_CURSOR
	;dd	VIDEO_RETURN	; READ_LPEN
	dd	set_mode_ncm	; Set mode without clearing video memory
	dd	ACT_DISP_PAGE
	dd	SCROLL_UP
	dd	SCROLL_DOWN
	dd	READ_AC_CURRENT
	dd	WRITE_AC_CURRENT
	dd	WRITE_C_CURRENT
	dd	SET_COLOR
	dd	WRITE_DOT
	dd	READ_DOT
	dd	WRITE_TTY
	dd	VIDEO_STATE
	dd	vga_pal_funcs	; 10/08/2016 (TRDOS 386)
	dd	font_setup	; 10/07/2016 (TRDOS 386)
	dd	VIDEO_RETURN	; RESERVED
	dd	WRITE_STRING	; 23/06/2016 (TRDOS 386)
M1L	EQU	$ - M1

; 22/12/2025
; 21/12/2025 - TRDOS 386 v2.0.10
; 29/11/2023 - TRDOS 386 v2.0.7
;	[VESA VBE3-PMI functions]
;	(fixing page table problems for LFB/PMI for >2.5GB main memory)
;	((Addresses of kernel page tables and PMI videobios address
;	  must be < 4MB or cr3 register must contain kernel's page dir
;	  during video interrupt. Save-change-restore cr3 register
;	  content here is a bugfix method to prevent page faults 
;	  for -real- computers with >2.5GB main memory.))
;	 {INT 31h was not changing cr3 while it contains user's
;	  page directory addr and accessing beyond of the 1st 4MB was
;	  causing to page faults.. because, after the 1st 4MB, user's
;	  page tables contain non-linear/virtual memory pages.}
;
; 02/08/2022 - TRDOS 386 v2.0.5
; 06/12/2020
; 05/12/2020
; 03/12/2020
; 27/11/2020 - TRDOS 386 v2.0.3
; 14/01/2017
; 02/01/2017
; 04/07/2016
; 12/05/2016
; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
int31h:  ; Video BIOS

; BH = Video page number
; BL = Color/Attribute
; AH = Function number
; AL = Character

VIDEO_IO_1:
	;sti				; INTERRUPTS BACK ON
	cld				; SET DIRECTION FORWARD

	;cmp	ah, M1L/4		; TEST FOR WITHIN TABLE RANGE
	;jnb	short M4		; BRANCH TO EXIT IF NOT A VALID COMMAND

	; 26/11/2020
	cmp	ah, M1L/4
	jb	short VGA_func

	cmp	ah, 4Fh
	jne	short M4 ; invalid !

VGA_func: ; 26/11/2020
	push	es ; *
	push	ds ; **			; SAVE WORK AND PARAMETER REGISTERS

	; 26/11/2020
	push	eax ; -

	mov	ax, KDATA 		; POINT DS: TO DATA SEGMENT
	mov	ds, ax
	mov	es, ax

	; 26/11/2020
	pop	eax ; +
	;
	sti
	cmp	ah, 4Fh
	;je	short VBE_func
	; 21/12/2025
	je	VBE_func

	; 04/12/2020
	mov	[video_eax], eax

	; 21/12/2020
	cmp	byte [CRT_MODE], 0FFh	; Current mode is a VESA VBE mode ?
	jb	short VGA_func_std

	or	ah, ah			; set mode ?
	jnz	short VGA_func_std	; no

	cmp	byte [vbe3], 3		; (real) VESA VBE 3 video bios ?
	jne	short VGA_func_std	; no

	jmp	vesa_vbe3_pmi

	; 21/12/2020
M4:					; COMMAND NOT VALID
	iretd				; DO NOTHING IF NOT IN VALID RANGE

VGA_func_std:
	; 06/12/2020
	; 03/12/2020
	cmp	ah, 0Fh
	;ja	short VGA_funcs_0	; only CGA funcs will be handled by
	; 21/12/2025
	ja	VGA_funcs_0

	; 06/12/2020			; vga bios firmware
	; 03/12/2020
	;test	ah, 7Fh ; set mode ?
	;;or	ah, ah			; only 'set mode' will be handled by
	;jnz	short VGA_funcs_0	; vga bios firmware
	;jz	short vbe_pmi32_0

	; 28/11/2020
	cmp	byte [pmi32], 0		; 32 bit protected mode interface for
	jna	short VGA_funcs_0	; video hardware's vga bios firmware
					; ([pmi32] > 0 if it is activated)
					; note:
					; [vbe3] = 3 is required to activate
	; 07/12/2020
	and	ah, ah ; is this set mode ?
	jz	short vbe_pmi32_2 ; yes

	cmp	ah, 04h	 ; set mode ('no clear memory' option)
	je	short VGA_funcs_0

	; 07/12/2020
	cmp	byte [CRT_MODE], 7 ; current mode > 7 ?
	jna	short VGA_funcs_0  ; no

	; when mode 3 is active,
	; video bios functions are not redirected
	; to VESA VBE3 PMI except 'set mode' function

vbe_pmi32_0:
	; 06/12/2020
	;or	ah, ah
	;jnz	short vbe_pmi32_2
	; ah = 0 ; 'set mode'
	;cmp	al, 3 ; 80x25 text mode, 16 colors, default mode for MainProg
	;jne	short vbe_pmi32_1
	;cmp	byte [CRT_MODE], 3 ; If current video mode <> 3 and requested
	;			; video mode is 3, use internal 'set mode';
	;			; otherwise, use vesa vbe 3 bios's 'set mode'.
	;jne	short VGA_funcs_0
vbe_pmi32_1:
	jmp	vesa_vbe3_pmi

;vbe_pmi32_2:
	;cmp	ah, 04h ; set mode (no clear mem option)
	;jne	short vbe_pmi32_1

vbe_pmi32_2:

	; 22/12/2025
	; 21/12/2025 - TRDOS 386 v2.0.10
	; set character height (needed) for next system calls
	;;;;
vbe_pmi32_3:
	push	esi
	push	eax
	mov	ah, al
	and	ah, 7Fh
	mov	esi, vga_modes
vbe_pmi32_3_1:
	lodsb
	cmp	ah, al
	je	short vbe_pmi32_3_2
	cmp	esi, vga_modes+vga_mode_count
	jb	short vbe_pmi32_3_1
vbe_pmi32_3_2:
	sub	esi, vga_modes
	shl	esi, 2  ; dword
	add	esi, vga_mode_tbl_ptr
	mov	esi, [esi]
	lodsw
	mov	[CRT_COLS], al
	inc	ah
	mov	[VGA_ROWS], ah
	mov	al, [esi]
	;lodsb
	mov	[CHAR_HEIGHT], al
vbe_pmi32_4:
	pop	eax
	pop	esi
	;;;;

	; 07/12/2020
	cmp	byte [CRT_MODE], 7 ; current mode > 7 ?
	ja	short vbe_pmi32_1  ; yes

	cmp	al, 7	; requested mode > 7 ?
	jna	short VGA_funcs_0  ; no (CGA)

	cmp	al, 13h
	jna	short vbe_pmi32_1

	test	al, 80h
	jz	short VGA_funcs_0  ; unknown or special

	cmp	al, 87h	; requested mode > 7 ?
			; (with no clear mem ops)
	jna	short VGA_funcs_0  ; no (CGA)

	cmp	al, 93h
	jna	short vbe_pmi32_1

	; > 13h video modes are unknown or special
 	; they must be handled by kernel

	; CGA video modes will be handled by kernel

VGA_funcs_0:
	push	edx ; ***
	push	ecx ; ****
	push	ebx ; *****
	push	esi ; ******
	push	edi ; *******
	push	ebp ; ********

	;mov	[video_eax], eax ; 12/05/2016 
	mov	edi, 0B8000h		; GET offset FOR COLOR CARD

	; 23/03/2016
	shl	ah, 2  ; dword		; TIMES 2 FOR WORD TABLE LOOKUP
	movzx	esi, ah			; MOVE OFFSET INTO LOOK UP REGISTER (SI)
	;mov	ah, [CRT_MODE]		; MOVE CURRENT MODE INTO (AH) REGISTER

	;;15/01/2017
	; 14/01/2017
	; 02/01/2017
	;;mov	byte [intflg], 31h  ; video interrupt
	;sti ; 26/11/2020
	;

	jmp	dword [esi+M1]		; GO TO SELECTED FUNCTION

VBE_func:
	; 26/11/2020
	;sti
	push	ebp ; *** ; 27/11/2020
	push	esi ; **** 

	; Note:
	; ebx, ecx, edx, edi, ebp registers
	; must be saved by VBE functions and
	; eax register must be set
	; (according to VBE 3 standard)
	; before return from this interrupt
	; (every function must restore and set
	; registers except esp, esi, es, ds)

	; 29/11/2023 - TRDOS 386 v2.0.7
	mov	esi, cr3
	push	esi ; *****
	;cmp	esi, [k_page_dir]
	;je	short VBE_func_x
	mov	esi, [k_page_dir]
	mov	cr3, esi
;VBE_func_x:
	cmp	byte [vbe3], 2
	ja	short VESA_VBE3_PMI_CALL ; VBE3 video bios ('PMID')
	;je	short VBE_func_0  ; Bochs/Qemu/VirtualBox emulator
	jb	short VBE_unknown ; invalid ([vbe3] = 0)

	;jmp	VESA_VBE3_PMI_CALL

VBE_func_0:
	; Bochs/Plex86 VGAbios VBE extension
	; (TRDOS 386 v2.0.3 can use VBE graphics modes on emulators)
	; BOCHS/QEMU/VIRTUALBOX

	mov	ah, [vbe2bios]
	cmp	ah, 0C0h
	jb	short VBE_unknown
	cmp	ah, 0C5h
	ja	short VBE_unknown

	; TRDOS 386 is running on BOCHS or QEMU
	mov	ah, 4Fh
VBE_func_1:
	movzx	esi, al ; VESA VBE function number
	;shl	si, 2 ; dword
	; 02/08/2022
	shl	esi, 2
	cmp	si, N1L
	jnb	short VBE_unknown
	;sti

	call	dword [esi+N1] ; call VBE function

	;jmp	short VBE_bios_return

VBE_bios_return:
	cli
	; 29/11/2023 - TRDOS 386 v2.0.7
	pop	esi ; *****
	;cmp	esi, [k_page_dir]
	;je	short VBE_bios_return_x
	mov	cr3, esi
;VBE_bios_return_x:	
	pop	esi ; ****
	pop	ebp ; *** ; 27/11/2020
	pop	es  ; **
	pop	ds  ; *
	iretd

	; 26/11/2020
N1:
	dd	vbe_biosfn_return_ctrl_info
	dd	vbe_biosfn_return_mode_info
	dd	vbe_biosfn_set_mode
	dd	vbe_biosfn_return_current_mode
	dd	vbe_biosfn_save_restore_state
	;dd	vbe_biosfn_display_window_ctrl
	;dd	vbe_biosfn_set_get_log_scanline
	;dd	vbe_biosfn_set_get_disp_start
	;dd	vbe_biosfn_set_get_dac_pal_frm
	;dd	vbe_biosfn_set_get_palette_data

N1L	EQU	$ - N1

VESA_VBE3_PMI_CALL: ; VESA VBE video bios (firmware) functions
		    ; by using VESA VBE3 Protected Mode Interface

	; 02/08/2022 - TRDOS 386 v2.0.5
	; 29/11/2020
	; 26/11/2020 - TRDOS 386 v2.0.3 video.s

	; We are here because..
	; 'PMID' has been verified by TRDOS 386 v2.0.3 kernel.
	; (Otherwise bochs/plex86 compatible VBE functions and
	;  modes would be used on BOCHS/QEMU/VIRTUALBOX emulators
	;  or only standard/old VGA graphics modes would be used.)

	; (TRDOS 386 v2.0.3 can use VESA VBE graphics modes if
	;  the video bios is full compatible with VBE3 standard)

	; 29/11/2020

	movzx	esi, al ; VESA VBE 3 function number
	;shl	si, 2 ; dword
	; 02/08/2022
	shl	esi, 2
	cmp	si, P1L
	jnb	short VBE_unknown
	;sti

	push	edi ; *****

	call	dword [esi+P1] ; call VBE 3 function

	pop	edi ; *****

	jmp	short VBE_bios_return

P1:
	dd	vbe3_pmfn_return_ctrl_info
	dd	vbe3_pmfn_return_mode_info
	dd	vbe3_pmfn_set_mode
	dd	vbe3_pmfn_return_current_mode
	dd	vbe3_pmfn_save_restore_state
	;dd	vbe3_pmfn_display_window_ctrl
	;dd	vbe3_pmfn_set_get_log_scanline
	;dd	vbe3_pmfn_set_get_disp_start
	;dd	vbe3_pmfn_set_get_dac_pal_frm
	;dd	vbe3_pmfn_set_get_palette_data
	;dd	vbe3_pmfn_return_pmi ; invalid for TRDOS 386 v2
	;dd	vbe3_pmfn_set_get_pixel_clock
	
P1L	EQU	$ - P1

;	; 29/11/2020
;	mov	edi, VBE3MODEINFOBLOCK >> 4 ; / 16
;	
;	cmp	al, 04h
;	jb	short vbe3_pm_f ; function: 4F00h to 4F03h
;	ja	short vbe3_pmi_f5B ; function: 4F05h to 4F0Bh
;
;	; check buffer length (must be <= 2048 bytes)
;
;	and	dl, dl ; 0
;	jz	short vbe3_pm_f 
;		       ; Return Save/Restore State buffer size
;
;	push	ebx  ; buffer address
;	push	edx  ; function: save (01h) or restore (02h)
;	call
;
;vbe3_pm_f03:
;	cmp	al, 2
;	ja	short vbe3_pm_f ; function 4F03h
;	jb	short vbe3_pm_f1
;
;
;vbe3_pm_f1:
;
;
;vbe3_pmi_f5B:
;	cmp	al, 09h
;	jna	short vbe3_pm_f ; funcs 05h to 09h are usable
;	
;	cmp	al, 0Bh ; Get/Set pixel clock, last function
;	jne	short VBE_unknown 
;			; (do not use 'uncertain' functions
;			; because of system-user buff transfers)
;vbe3_pm_f:
;	mov	byte [vbe3_pm_fn], al	; set
;	; prepare 16 bit pm segments & registers for pmi call
;	call	VESA_VBE3_PM_FUNCTION
;
;

	; 26/11/2020
VBE_unknown:
	mov	ax, 0100h  ; ah = 1 : Function call failed
			   ; al = 0 : Function is not supported

	jmp	short VBE_bios_return

vbe3_pmfn_return_ctrl_info:
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 12/12/2020
	;
	; VBE function 4F00h - Return VBE Controller Information
	;
	; Input:
	;      EDI = Pointer to buffer in which to place
	;	     VbeInfoBlock structure
	;
	;	AX = 4F00h
	; Output:
	;	AX = VBE return status
	;	     AX = 004Fh -> succeeded
	;	     AX <> 004Fh -> failed
	;
	; Modified registers: eax (+ edi for kernel's own call)

	; NOTE: TRDOS 386 v2 (v2.0.3) kernel calls this function
	; during startup while cpu is in real mode
	; (by using int 10h, 4F02h) and saves VbeInfoBlock at
	; VBE3INFOBLOCK address (97E00h for TRDOS 386 v2.0.3).
	;
	; So...
	; This VBE function is adjusted to return/move same info
	; from VBE3INFOBLOCK to user's buffer in EDI.

	; int 31h (int 10h) entrance

	and	edi, edi
	jz	short vbe3_func_fail ; invalid buffer address !

;_vbe3_pmfn_return_ctrl_info:
	;or	edi, edi
	;jnz	short _vbe_biosfn_return_ctrl_info
	;
	;; this option may not be necessary - 12/12/2020
	;
	;; edi = 0, kernel forces to get ctrl info again
	;;	   by using VESA VBE3 video bios's pmi
	;
	;;push	edi
	;; far call to VESA VBE3 PMI
	;;mov	ax, 4F00h ; Return VBE Controller Info
	;mov	edi, VBE3INFOBLOCK-VBE3SAVERESTOREBLOCK
	;; ES selector base address = VBE3SAVERESTOREBLOCK
	;call	int10h_32bit_pmi
	;;pop	edi
	;mov	edi, VBE3INFOBLOCK ; retn to the kernel sub
	;cmp	ax, 004Fh
	;je	short vbe_ctrl_info_retn
	;stc
	;retn

_vbe_biosfn_return_ctrl_info:
	push	edi
	push	ecx
	mov	esi, VBE3INFOBLOCK
	;mov	ecx, 512
	; 02/08/2022
	sub	ecx, ecx
	mov	ch, 2
	; ecx = 512
	call	transfer_to_user_buffer
	pop	ecx
	pop	edi
	jc	short vbe3_func_fail

	xor	eax, eax
	mov	al, 4Fh ; successful
;vbe_ctrl_info_retn:
	retn

vbe_biosfn_return_ctrl_info:
	; 12/12/2020
	;
	; VBE function 4F00h - Return VBE Controller Information
	;
	; Input:
	;      EDI = Pointer to buffer in which to place
	;	     VbeInfoBlock structure
	;
	;	AX = 4F00h
	; Output:
	;	AX = VBE return status
	;	     AX = 004Fh -> succeeded
	;	     AX <> 004Fh -> failed
	;
	; Modified registers: eax

	and	edi, edi
	jz	short vbe3_func_fail ; invalid buffer addr !
	jmp	short _vbe_biosfn_return_ctrl_info

vbe3_pmfn_return_mode_info:
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 21/12/2020
	; 12/12/2020
	;
	; VBE function 4F01h - Return VBE Mode Information
	;
	; Input:
	;	CX = Mode number (VESA VBE mode number)
	;      EDI = Pointer to ModeInfoBlock structure
	;	     (256 bytes) -User's buffer address-
	;      EDI = 0 -> kernel call
	;		  (do not transfer ModeInfoBlock
	;		  to user's buffer address)
	;	AX = 4F01h
	; Output:
	;	AX = VBE return status
	;	     AX = 004Fh -> succeeded
	;	     AX <> 004Fh -> failed
	;
	; Modified registers: eax, esi, edi

	; int 31h (int 10h) entrance

	or	edi, edi
	jnz	short _vbe3_pmfn_return_mode_info

vbe3_func_fail:
	mov	eax, 014Fh ; ah = 1 : Function call failed
			   ; al = 4Fh : Function is supported
	retn

	; jump from '_vbe_biosfn_return_mode_info'
_vbe3_pmfn_return_mode_info:
	push	edi

	;; clear vbe3 'mode info block' buffer
	;push	ecx
	;xor	eax, eax
	;mov	ecx, 256/4
	;mov	edi, VBE3MODEINFOBLOCK
	;rep	stosd
	;pop	ecx

	; far call to VESA VBE3 PMI
	;mov	ax, 4F01h ; Return VBE Mode Information
	mov	edi, VBE3MODEINFOBLOCK-VBE3SAVERESTOREBLOCK
	; ES selector base address = VBE3SAVERESTOREBLOCK
	call	int10h_32bit_pmi

	pop	edi

	;cmp	ax, 004Fh
	;jne	short vbe3_func_retn  ; failed !

	and	edi, edi
	;jz	short vbe3_func_success
	; 21/12/2020
	jz	short vbe3_func_retn

	cmp	ax, 004Fh
	jne	short vbe3_func_retn  ; failed !

	push	ecx
	mov	esi, VBE3MODEINFOBLOCK
	;mov	ecx, 256
	; 02/08/2022
	sub	ecx, ecx
	inc	ch
	; ecx = 256
	call	transfer_to_user_buffer
	pop	ecx
	jc	short vbe3_func_fail

	xor	eax, eax
	mov	al, 4Fh ; successful
vbe3_func_success:
vbe3_func_retn:
	retn

vbe3_pmfn_return_current_mode:
	; 12/12/2020
	;
	; VBE function 4F03h - Return Current VBE Mode
	;
	; Input:
	;	none (AX = 4F03h)
	; Output:
	;	AX = VBE return status
	;	     AX = 004Fh -> succeeded
	;	     AX <> 004Fh -> failed
	;	BX = Current VBE mode
	;	  bit 0-13 = Mode number
	;	  bit 14 = 0 Windowed frame buffer model
	;	         = 1 Linear frame buffer model
	;	  bit 15 
	;	      = 0 Memory cleared at last mode set
	;	      = 1 Memory not cleared at last mode set
	;
	; Modified registers: eax, ebx

	; int 31h (int 10h) entrance

	; far call to VESA VBE3 PMI 

	;mov	eax, 4F03h ; Return Current VBE Mode
vbe3_pmfn_far_call:
	; ES selector base address = VBE3SAVERESTOREBLOCK 
	;call	int10h_32bit_pmi
	;retn
	jmp	int10h_32bit_pmi

vbe3_pmfn_set_mode:
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 22/12/2020
	; 21/12/2020
	; 12/12/2020
	;
	; VBE function 4F02h - Set VBE Mode
	;
	; Input:
	;	BX = Desired Mode to set
	;	  bit 0-13 = Mode number
	;	  bit 14 = 0 Windowed frame buffer model
	;	         = 1 Linear frame buffer model
	;	  bit 15
	;	      = 0 Memory cleared at last mode set
	;	      = 1 Memory not cleared at last mode set
	; Output:
	;	AX = VBE return status
	;	     AX = 004Fh -> succeeded
	;	     AX <> 004Fh -> failed
	;
	; Modified registers: eax, ebx, esi (21/12/2020)

	; int 31h (int 10h) entrance

	; 22/12/2020 (VESA VBE3 feature)
	; BX bit 11 is flag for
	;	user specified CRTC values for refresh rate
	; 'test bh, 8'
	; if bit 11 is set, EDI points to 'CRTCInfoBlock'

	; 22/12/2020
	;; test bx for VBE video mode
	;test	bh, 1
	;jnz	short vbe3_sm_0

	;; use internal VBE mode set procedure
	;; for non-vbe (std VGA/CGA) modes
	;
	;; (it is useful -as 4F02h function-
	;;  to jump 'vbe_biosfn_set_mode'
	;;  instead of direct jump to '_set_mode')
	;; ((eliminates additional push-pops and settings))

	;jmp	vbe_biosfn_set_mode

vbe3_sm_0: 
	;;push	ds  ; *
	;;push	es  ; **
	;;push	ebp ; ***
	;;push	esi ; ****

	; Fit bx to VESA VBE2 type mode setting
	; (bx bit 11 is used for custom CRTC values in VBE3)
	; clear bit 9 to 11 (clear bh bit 1 to bit 3)

	; 22/12/2020
	push	edi ; *****
	test	bh, 8	; Use user specified CRTC values
	jnz	short vbe3_sm_3  ; for refresh rate
vbe3_sm_4:
	and	bh, 0C1h ; use bit 15, 14, 8 only (for bh)
	;mov	[vbe_mode_x], bh

	cmp	byte [CRT_MODE], 3 ; is current mode 03h ?
	jne	short vbe3_sm_1    ; no

	; save mode 03h video pages and cursor positions
	push	edi ; **!***
	push	ecx ; ******
	;push	esi

	call	save_mode3_multiscreen

	;pop	esi
	pop	ecx ; ******
	pop	edi ; **!***
vbe3_sm_1:
	; ax = 4F02h
	; bx = video mode number (vbe2 type)
	call	int10h_32bit_pmi 
			; call to far call to VBE3 PMI

	cmp	ax, 004Fh ; succeeded ?
	jne	short vbe3_sm_2
	; set current mode byte/sign to extended (SVGA) mode
	mov	byte [CRT_MODE], 0FFh ; VESA VBE mode
	; set current VBE mode word to bx input
	mov	[video_mode], bx
vbe3_sm_2:
	; 22/12/2020
	pop	edi ; ***** 
	retn

vbe3_sm_3:
	; 22/12/2020
	; copy user's CRTCInfoBlock to the buffer
	push	ecx
	mov	esi, edi
	mov	edi, VBE3CRTCINFOBLOCK
	;mov	ecx, 64
	; 02/08/2022
	sub	ecx, ecx
	mov	cl, 64
	call	transfer_from_user_buffer
	pop	ecx
	; set offset (es base addr is VBE3SAVERESTOREBLOCK) 
	sub	edi, VBE3SAVERESTOREBLOCK
	jmp	short vbe3_sm_4

vesa_vbe3_pmi:
	; 29/11/2023 - TRDOS 386 v2.0.7
	; 12/12/2020
	; 08/12/2020
	; 07/12/2020
	; 05/12/2020, 06/12/2020
	; 03/12/2020, 04/12/2020
	; 28/11/2020 (TRDOS 386 v2.0.3)
	; VGA BIOS functions via
	; VESA VBE3 Protected Mode Inface
	; [vbe3] = 3 and [pmi32] > 0

	; 04/12/2020
	; Only 'set mode' will be redirected to vbe3 video bios
	; (by setting mode 3 multiscreen parameters before and after)

	; 29/11/2023 - TRDOS 386 v2.0.7
	push	eax
	mov	eax, cr3
	xchg	eax, [esp] ; **!**
	push	eax
	;cmp	esi, [k_page_dir]
	;je	short vesa_vbe3_pmi_x
	mov	eax, [k_page_dir]
	mov	cr3, eax
;vesa_vbe3_pmi_x:
	pop	eax

	; 06/12/2020
	and	ah, ah	; 0 = set mode function
	jz	short vbe3_pmi_0
	jmp	vbe3_pmi_9 

vbe3_pmi_0:
	; 07/12/2020
	mov	ah, al
	and	ah, 80h ; 0 or 80h
	xor	al, ah ; 8?h -> 0?h

	;cmp	al, 13h ; mode number above 13h is returned
	;jna	short vbe3_pmi_1
	;		; back to default code due to uncertainty
	; 		; (>13h is not std for all svga bioses)
	;jmp	VGA_funcs_0
vbe3_pmi_1:
	; 07/12/2020
	; Possible cases for VBE3 (PMI, ah=0) set mode:
	; current mode > 07h and requested mode: any 
	; current mode <= 07h and requested mode > 07h

	; 06/12/2020
	mov	byte [noclearmem], ah ; 0 or 80h
	; check current video mode if it is 03h
	cmp	byte [CRT_MODE], 3 ; current mode
	jne	short vbe3_pmi_3
	; 07/12/2020
	; check new video mode if it is 03h also
	;cmp	al, 3
	;jne	short vbe3_pmi_2
	;mov	byte [p_crt_mode], 80h 	; clear video memory
	;jmp	short vbe3_pmi_5
vbe3_pmi_2:
	; case 1:
	; Current mode is 03h and new mode is not 03h

	; save video pages and cursor positions
	push	esi
	push	edi
	push	ecx

	; 12/12/2020
	;mov	esi, 0B8000h ; mode 3 video memory
	;mov	edi, 98000h  ; backup location
	;mov	ecx, (0B8000h-0B0000h)/4
	;rep	movsd
	;
	;mov	byte [p_crt_mode], 3 ; previous mode, backup sign
	;xchg	cl, [ACTIVE_PAGE]
	;mov	[p_crt_page], cl  ; save as previous active page
	;
	;; save cursor positions
	;mov	esi, CURSOR_POSN
	;mov	edi, cursor_pposn ; cursor positions backup
	;mov	cl, 4
	;rep	movsd

	; 12/12/2020
	call	save_mode3_multiscreen

	pop	ecx
	pop	edi
	pop	esi
vbe3_pmi_3:
	; 08/12/2020
	; 07/12/2020
	; case 3 or case 4
	mov	[CRT_MODE], al
	cmp	al, 3
	je	short vbe3_pmi_4
	; case 4:
	; Current mode is not 03h and also new mode is not 03h
	or	byte [p_crt_mode], 80h  ; 83h (case 1 -> case 4)
	;jmp	short vbe3_pmi_5
vbe3_pmi_4:
	; case 3:
	;
	; Current mode is not 03h and new mode is 03h

;vbe3_pmi_5:
	;mov	[CRT_MODE], al

	call	int10h_32bit_pmi

	cmp	byte [CRT_MODE], 3  ; new video mode
	;jne	vbe3_pmi_8	; video mode <> 03h
	jne	short vbe3_pmi_8

	;push	eax ; 04/12/2020
	push	ebx
	push	ecx
	push	edx
	push	edi ; 03/12/2020

	; 12/12/2020
	push	esi
	call	restore_mode3_multiscreen
	pop	esi
	; AL = active video page

	; 12/12/2020
	;mov	al, [p_crt_page] ; previous mode 3 active page
	;
	;;test	byte [p_crt_mode], 7Fh ; 83h or 80h or 03h
	;;jz	short vbe3_pmi_6 ; do not restore video pages
	;			 ; clear current video page
	;; case 3
	;
	;; ([p_crt_mode] = 03h)
	;
	;; New video mode is 3 while current video mode is not 3
	;; (multi screen) video pages will be restored from 098000h
	;
	;; restore video pages and cursor positions
	;
	;mov	[ACTIVE_PAGE], al ; current mode 3 active page
	;
	;push	esi
	;
	;; restore video pages
	;mov	esi, 98000h
	;mov	edi, 0B8000h
	;;mov	ecx, 2000h
	;mov	cx, 2000h ; 8K dwords (32K)
	;rep	movsd
	;
	;mov	[p_crt_mode], cl ; reset ('case 3' end condition)
	;
	;; restore cursor positions
	;mov	esi, cursor_pposn
	;mov	edi, CURSOR_POSN
	;;mov	ecx, 4	; restore all cursor positions (16 bytes)
	;mov	cl, 4
	;rep 	movsd
	;
	;pop	esi
	;
	;; 07/12/2020
	;; restore CRT_START according to ACTIVE_PAGE
	;mov	[CRT_START], cx ; 0
	;
	;; check active page and set it again if it is not 0
	;or	al, al
	;jz	short vbe3_pmi_7
	;
	;mov	cl, al
;vbe3_pmi_5:
	;add	word [CRT_START], 4096
	;dec	cl
	;jnz	short vbe3_pmi_5

	mov	ah, 05h ; set current video page
	;al = video page
	call	int10h_32bit_pmi
	
	; check current cursor position & set it again if not 0,0
	;movzx	ebx, byte [ACTIVE_PAGE]
	movzx	ebx, al
	shl	bl, 1
	add	ebx, CURSOR_POSN
	mov	dx, [ebx]
	and	dx, dx
	jz	short vbe3_pmi_7

	;dx = cursor position (dl = column, dh = row)
	;mov	bh, [ACTIVE_PAGE] ; 06/12/2020
	mov	bh, al
	mov	ah, 02h ; set cursor position
	call	int10h_32bit_pmi

	;jmp	short vbe3_pmi_7

;vbe3_pmi_6:
;	; 07/12/2020
;	; case 1, previous mode is 03h, current mode is 03h
;	; 03/12/2020
;	cmp	byte [noclearmem], 0
;	jna	short vbe3_pmi_7 ; do not clear memory
;	; clear video page
;	mov	ecx, 1024 ; 4096/4
;	mov	eax, 07200720h
;	mov	edi, 0B8000h ; [crt_base]
;	add	di, [CRT_START]
;	rep	stosd	; FILL THE REGEN BUFFER WITH BLANKS

vbe3_pmi_7:
	pop	edi
	pop	edx
	pop	ecx
	pop	ebx
	;pop	eax ; 04/12/2020
vbe3_pmi_8:
	; 04/12/2020
	;(TRDOS 386 v2.0.3, INT 31h, ah=0 return)
	xor	eax, eax  ; eax = 0 -> succesful
vesa_vbe3_pmi_retn:
	; 29/11/2023 - TRDOS 386 v2.0.7
	xchg	eax, [esp] ; **!**
	;cmp	eax, [k_page_dir]
	;je	short vesa_vbe3_pmi_retn_x
	mov	cr3, eax
;vesa_vbe3_pmi_retn_x:
	pop	eax
	;
	pop	es  ; **
	pop	ds  ; *
	iretd

vbe3_pmi_9:
	; 06/12/2020 
	;cmp	ah, 10h ; Set/Get Palette Registers
	;jnb	short vbe3_pmi_10
	; 05/12/2020
	call	int10h_32bit_pmi
	jmp	short vesa_vbe3_pmi_retn

;vbe3_pmi_10:
	; 06/12/2020
	;jmp	VGA_funcs_0

int10h_32bit_pmi:
	; 03/12/2020
	; 28/11/2020
	; calling standard VGA Bios (INT 10h) functions
	; by using 32 bit protected mode interface of
	; VESA VBE3 Video Bios (with 'PMID' signature)

	; 03/12/2020
	; eax, ebx, ecx, edx, edi will be used by vbios pmi
	; (esi and ebp will not be used) 

	; 03/12/2020
	push	esi
	shl	eax, 16  ; move function number (ax) to hw
	mov	esi, [pmid_addr] ; linear address of
				 ; PMInfo.Entrypoint pointer
	;mov	ax, [esi+PMInfo.EntryPoint]
	mov	ax, [esi]	 
	rol	eax, 16 ; move PM entry address to hw
			; and move function number to lw (ax)
	pop	esi

	; top of stack: ; (*)
	; return (the caller) address of "int10h_32bit_pmi"

	jmp	_VBE3PMI_fcall ; will return to the caller (*)

_vbe3_pmfn_srs_8:
	; 17/01/2021
	xor 	ebx, ebx ; points to VBE3SAVERESTOREBLOCK
_vbe3_pmfn_srs_9: ; 24/01/2021
	mov	ax, 4F04h
	jmp	short int10h_32bit_pmi

vbe3_pmfn_save_restore_state:
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 24/01/2021
	; 23/01/2021
	; 16/01/2021 - 17/01/2021
	; 14/01/2021
	;
	; VBE function 4F04h - Save/Restore Video State
	;
	; Input:
	;	DL = sub function
	;	CL = requested state
	;      EBX = pointer to buffer (if DL<>00h)
	;	AX = 4F04h 
	; Output:
	;	AX = 004Fh (successful)
	;	AH > 0 -> error
	;	BX = Number of 64-byte blocks 
	;	     to hold the state buffer (if DL=00h)

	; Modified registers: eax, ebx, esi, edi

	and	ebx, ebx ; user's buffer address
	jnz	short _vbe3_pmfn_save_restore_state

	or	dl, dl
	jz	short _vbe3_pmfn_srs_0

	; function failed
	;;mov	eax, 0100h
	;sub	eax, eax
	;inc	ah  ; eax = 0100h
	;retn
	; 16/01/2021
_vbe3_pmfn_srs_fail:
	mov	eax, 014Fh ; ah = 1 : Function call failed
			   ; al = 4Fh : Function is supported
_vbe3_srs_retn:
	retn

_vbe3_pmfn_save_restore_state:
	and	dl, dl
	jnz	short _vbe3_pmfn_srs_2
_vbe3_pmfn_srs:
	xor	ebx, ebx
_vbe3_pmfn_srs_0:
	; 24/01/2021
	cmp	ecx, 0Fh
	;ja	short _vbe3_pmfn_srs_1
	ja	short _vbe3_pmfn_srs_fail 

	; !!! CLEAR CL BIT 2 !!! 
	; (when bit 2 is set, function causes cpu exception)
	; BIOS data will not be saved and restored
	; (to prevent protected mode page fault error)
	and	cl, ~2 ; and cl, not 2

	; 24/01/2021
	;mov	bl, 1
	inc	bl ; = 1
	;shl	bx, cl
	; 02/08/2022
	shl	ebx, cl
	and	bx, [vbe3stbsflags]
	jz	short _vbe3_pmfn_srs_1
	;mov	bx, cx
	mov	ebx, ecx ; <= 15
	shl	bl, 1 ; 0, 2, 8 .. 30
	mov	bx, [vbestatebufsize+ebx]
	mov	edi, ebx
	; edi = state buffer size in bytes
	;shr	bx, 6 ; / 64
	; 02/08/2022
	shr	ebx, 6
	;mov	ax, 4Fh
	sub	ah, ah
	mov	al, 4Fh
	retn
_vbe3_pmfn_srs_1:
	; ax = 4F04h
	;call	int10h_32bit_pmi
	; 24/01/2021
	;call	_vbe3_pmfn_srs_8
	; ebx = 0
	call	_vbe3_pmfn_srs_9
	cmp	ax, 004Fh
	jne	short _vbe3_srs_retn
	; 24/01/2021
	;cmp	ecx, 0Fh
	;ja	short _vbe3_srs_retn
	; 24/01/2021
	;mov	ax, 1
	mov	al, 1
	;shl	ax, cl
	; 02/08/2022
	shl	eax, cl
	or	[vbe3stbsflags], ax ; set flag for state option
	; 23/01/2021
	mov	edi, ebx
	mov	eax, ecx
	shl	al, 1
	;shl	di, 6 ; * 64
	; 02/08/2022
	shl	edi, 6
	mov	[vbestatebufsize+eax], di
				; save buf size for option 
	;xchg	edi, ebx
	; edi = state buffer size in bytes
	mov	al, 4Fh
	retn

_vbe3_pmfn_srs_2:
	; 24/01/2021
	; !!! CLEAR CL BIT 2 !!! 
	; (when bit 2 is set, function causes cpu exception)
	; BIOS data will not be saved and restored
	; (to prevent protected mode page fault error)

	test	cl, ~2 ; test cl, not 2
	jz	short _vbe3_pmfn_srs_fail

	cmp	dl, 2
	ja	short _vbe3_pmfn_srs_5

	;and	cl, ~2 ; and cl, not 2

	push	ebx ; * ; buffer address
	; save or restore state
	; (get required buffer size at first)
	push	edx ; **
	sub	dl, dl ; 0
	call	_vbe3_pmfn_srs
	pop	edx ; **
	; 24/01/2021
	pop	ebx ; *
	or	ah, ah
	jnz	short _vbe3_pmfn_srs_4 ; error

	; edi = buffer size in bytes
	cmp	edi, 2048
	ja	short _vbe3_pmfn_srs_3

	cmp	dl, 1
	jne	short _vbe3_pmfn_srs_6 ; restore state

	; save video state
	;xor 	ebx, ebx ; points to VBE3SAVERESTOREBLOCK
	;mov	ax, 4F04h
	;call	int10h_32bit_pmi

	; 24/01/2021
	call	_vbe3_pmfn_srs_7

	cmp	ax, 004Fh
	jne	short _vbe3_pmfn_srs_4

	or	ebx, ebx  ; kernel ('sysvideo') ?
	jz	short _vbe3_pmfn_srs_4 ; yes

	; the caller is user
	push	ecx ; *
	mov	ecx, edi ; state buffer size
	mov	esi, VBE3SAVERESTOREBLOCK ; source
					; (vbe3 pmi buff)
	mov	edi, ebx	; destination (user buff)
	call	transfer_to_user_buffer
	pop	ecx ; *
	jc	short _vbe3_pmfn_srs_3

	sub	eax, eax
	mov	al, 4Fh
	retn

	; 24/01/2021
_vbe3_pmfn_srs_3:
	mov	eax, 014Fh
_vbe3_pmfn_srs_4:
	retn
_vbe3_pmfn_srs_5:
	xor	eax, eax
	inc	ah
	; eax = 0100h, function is not supported
	retn

_vbe3_pmfn_srs_6:
	; restore video state
	; 24/01/2021
	;pop	ebx ; *
	; 23/01/2021
	or	ebx, ebx ; 0 ?	
	jz	short _vbe3_pmfn_srs_7 ; 'sysvideo' call
	; 24/01/2021
	;jz	_vbe3_pmfn_srs_8
	mov	esi, ebx
	; esi = user's video state buffer
	push	ecx ; *
	mov	ecx, edi ; state buffer size
	mov	edi, VBE3SAVERESTOREBLOCK ; destination
					; (vbe3 pmi buff)
	;mov	esi, ebx	; source (user buff)
	call	transfer_from_user_buffer
	pop	ecx ; *
	jc	short _vbe3_pmfn_srs_3
_vbe3_pmfn_srs_7:
	push	ebx ; *
	; restore video state
	;xor 	ebx, ebx ; points to VBE3SAVERESTOREBLOCK
	;mov	ax, 4F04h
	;call	int10h_32bit_pmi
	; 17/01/2021
	call	_vbe3_pmfn_srs_8
	pop	ebx ; *
	retn

VIDEO_STATE:
	; 26/06/2016
	; 12/05/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)

;---------------------------------------------------
; VIDEO STATE
;  RETURNS THE CURRENT VIDEO STATE IN AX
;  AH = NUMBER OF COLUMNS ON THE SCREEN
;  AL = CURRENT VIDEO MODE
;  BH = CURRENT ACTIVE PAGE
;---------------------------------------------------

	mov	ah, [CRT_COLS]	; GET NUMBER OF COLUMNS
	mov	al, [CRT_MODE]	; CURRENT MODE
	;movzx	esi, al
	;mov	ah, [esi+M6] 
	; BH = active page
	mov	bh, [ACTIVE_PAGE] ; GET CURRENT ACTIVE PAGE
	cli	; 02/01/2017
	pop	ebp		; RECOVER REGISTERS
	pop	edi
	pop	esi
	pop	ecx		; DISCARD SAVED BX
	jmp	short M15	; RETURN TO CALLER

set_mode_ncm:
	; 17/11/2020 (TRDOS 386 v2.0.3)
	; 04/07/2016 - TRDOS 386 (TRDOS v2.0)
	; set mode without clearing the video memory
	; (ony for graphics modes)

	;cmp	al, 7 ; IBM PC CGA modes
	;jna	short SET_MODE ; normal function (clear)
	;; do not clear memory
	;;mov	[noclearmem], al ; > 0
	;mov	byte [noclearmem], 80h ; 17/11/2020
	;call	_set_mode
	;mov	byte [noclearmem], 0
        ;jmp     short VIDEO_RETURN

	; 17/11/2020 (TRDOS v2.0.3)
	or	al, 80h ; not clear memory option

	; 05/12/2020
	; 27/11/2020
	; 17/11/2020
	; 08/08/2016, 10/08/2016
	; 29/07/2016, 30/07/2016
	; 25/07/2016, 26/07/2016, 27/07/2016
	; 02/07/2016, 18/07/2016, 23/07/2016
	; 24/06/2016, 26/06/2016
	; 29/05/2016 - TRDOS 386 (TRDOS v2.0)
SET_MODE:
	; For 32 bit TRDOS and Retro UNIX 386:
	;	valid video mode: 03h only!
	;	(VGA modes will be selected with another routine)
	;
	; set_txt_mode ; 80*25 (16 fore colors, 8 back colors)

	; 27/11/2020
	
	; Check if current mode is 
	; Bochs/Plex86 VBE graphics mode
	cmp	byte [CRT_MODE], 0FFh ; VESA VBE graphics mode
	jb	short _set_mode_      ; signature
				      ; VBE mode number is in
				      ; [video_mode] bit 0to8
	mov	bl, al ; save video mode
	call	dispi_get_enable
	push	eax ; save current VBE dispi status
	; Disable Bochs/Plex86 VBE dispi
	;mov	ax, 0 ; VBE_DISPI_DISABLED
	xor	eax, eax ; 0 
	call	dispi_set_enable
	mov	al, bl ; restore video mode
	call	_set_mode
	pop	eax ; restore current VBE dispi status
	jnc	short VIDEO_RETURN
	; ! unimplemented or invalid video mode number !
	; VBE dispi must be enabled again
	; (return to run on current VBE graphics mode)
	;;mov	al, [video_mode+1] ; bit 8 to 15
	;;and	al, 0C0h ; isolate bit 14 and bit 15
	;;or	al, 1 ; VBE_DISPI_ENABLED
	call	dispi_set_enable
	jmp	short _video_func_err

_set_mode_:
	; VGA bios (non-VBE) 'setmode' procedure

	; 26/11/2020 (TRDOS v2.0.3)

;------------------------------------------------------
; SET MODE					      :
;	THIS ROUTINE INITIALIZES THE ATTACHMENT TO    :
;	THE SELECTED MODE, THE SCREEN IS BLANKED.     :
; INPUT						      :
;	(AL) - MODE SELECTED (RANGE 0-7)	      :
; OUTPUT					      :
;	NONE					      :
;------------------------------------------------------

	call	_set_mode ; 24/06/2016 (set_txt_mode)
	; 26/11/2020
	jnc	short VIDEO_RETURN

	; 26/11/2020
_video_func_err:
	xor	eax, eax ; function call failed
	dec	eax  ; 0FFFFFFFFh ; - 1
	jmp	short _video_return

; 12/05/2016
; 16/01/2016 (TRDOS 386 = TRDOS v2.0)

;-----	NORMAL RETURN FROM ALL VIDEO RETURNS

VIDEO_RETURN:
	mov	eax, [video_eax] ; 12/05/2016
_video_return:
	cli ; 02/01/2017
	pop	ebp ; ******** ; 26/11/2020
	pop	edi ; *******
	pop	esi ; ******
	pop	ebx ; *****
M15:	; VIDEO_RETURN_C
	;;15/01/2017
	; 02/01/2017
	;;mov	byte [intflg], 0
	;
	pop	ecx ; **** ; 26/11/2020
	pop	edx ; ***
	pop	ds  ; **
	pop	es  ; *	; RECOVER SEGMENTS
	iretd		; ALL DONE

set_txt_mode:

	; 29/07/2016
	; 27/06/2016
	mov	al, 3 ; 26/11/2020 (bit 7 = 0)

	; 17/11/2020 (TRDOS v2.0.3)
	;mov	byte [noclearmem], 0

; 04/08/2022
; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
; 12/04/2021
; 10/08/2016
; 08/08/2016
; 30/07/2016
; 29/07/2016
; 25/07/2016 - 26/07/2016 - 27/07/2016
; 07/07/2016 - 18/07/2016 - 23/07/2016
; 02/07/2016 - 03/07/2016 - 04/07/2016
; 26/06/2016
; 24/06/2016 (set_txt_mode -> _set_mode)
; 17/06/2016
; 29/05/2016
; 16/01/2016 (TRDOS 386 = TRDOS v2.0)

_set_mode:
	; 12/12/2020
	; 26/11/2020
	; call from 'biosfn_set_video_mode'
	;	(bochs/plex86 video bios code)
	; call from 'SET_MODE'
	;	(TRDOS 386 v2 default, IBM PC/AT rom bios code)
	; continue from 'set_txt_mode'

	; INPUT:
	;	al = VGA video mode
	; RETURN:
	;	cf = 1 -> video mode not implemented
	;	cf = 0 -> OK
	;
	; Modified registers: eax, bx, ecx, esi, edi, (ebp)

	; 17/11/2020 (TRDOS v2.0.3)
	; no clear memory option 
	; (from mode number byte bit 7)
	mov	ah, al
	and	ah, 80h
	;mov	[noclearmem], al
	mov	[noclearmem], ah
	;and	al, 7Fh ; clear bit 7
	;;xor	[noclearmem], al ; clear bit 0 to 6
	; 26/11/2020
	xor	al, ah ; and al, 7Fh

	; 19/11/2020

	; Video mode 03h action principle:
	;
	; for case 1:
	; Current mode is 03h and next/requested mode is not 03h
	;	- save mode (set mode 03h flag)
	;	- save 8 video pages (which are will be restored)
	;	- save active page number (which will be reactivated)
	;	- set active page to 0 always (no multi screen)
	;	- save 8 cursor positions (which will be restored)
	; 	- use 'noclearmem' option
	;	[p_crt_mode] = 0 -> 03h 
	;
	; for case 2:
	; Current mode is 03h and next/requested mode is also 03h
	;	- clear active video page if 'noclearmem' is not set
	;	[p_crt_mode] = 0 -> 80h -> 0
	;
	; for case 3:
	; Current mode is not 03h and next/requested mode is 03h
	;	- restore video pages (8 video pages were saved)
	;	- restore active page number (which were saved)
	;	- restore 8 cursor positions (which were saved)
	;	- reset/clear mode 03h flag
	;	[p_crt_mode] = 03h -> 0
	;
	; for case 4:
	; Current mode is not 03h and next/requested mode is not 03h
	; 	- use 'noclearmem' option
	;	- set active page to 0 always
	;	[p_crt_mode] = 03h -> 83h -> 03h
	;
	; initial (boot time) values:
	;	[p_crt_mode] = 0 ("there isn't a page backup, yet")
	;	  [CRT_MODE] = 3 (kernel's starting mode)

	; 26/11/2020
	cmp	al, 03h	    ; mode 3, 80x25 text, 16 colors
	jne	short _sm_0 ; (default mode for TRDOS 386 mainprog)

	; case 2 or case 3

	; check current video mode if it is 03h
	or	ah, ah ; 80h or 0 ('noclearmem' option)
	jnz	short _sm_1 ; do not clear display page

	; 26/11/2020
	; Note:
	; [CRT_MODE] = 0FFh for VESA VBE video modes
	; [video_mode] = standard VGA and VESA VBE video modes

	cmp	[CRT_MODE], al	; 03h
	jne	short _sm_2	; case 3 ([p_crt_mode] = 03h)

	; case 2

	; [p_crt_mode] = 0

	; 19/11/2020
	; If '_set_mode' procedure is called for video mode 3
	;     while video mode is 3, video page will be cleared
	;     and cursor position of video page will be reset.

	; clear display page
	mov	byte [p_crt_mode], 80h ; clear page sign
	jmp	short _sm_3	; bypass save video page routine
_sm_0:
	; case 1 or case 4

	; 05/12/2020
	cmp	byte [CRT_MODE], 3 ; is current mode 03h?
	jne	short _sm_1	; case 4 ; [p_crt_mode] = 03h

	; case 1
	; [p_crt_mode] = 0

	; 19/11/2020
	; If '_set_mode' procedure is called for a video mode
	;     except video mode 3 while current video mode
	;     is 3, all video pages of mode 3 will be copied
	;     to 98000h address as backup, before mode change.
	
_sm_save_pm:
	; 12/12/2020
	;; 03/07/2016
	;; save video pages
	;mov	esi, 0B8000h
	;mov	edi, 98000h ; 30/07/2016
	;mov	ecx, (0B8000h-0B0000h)/4
	;rep	movsd

	;mov	byte [p_crt_mode], 3 ; previous mode, backup sign
	;; 26/11/2020
	;xchg	cl, [ACTIVE_PAGE]
	;mov	[p_crt_page], cl  ; save as previous active page
	;
	;; save cursor positions
	;mov	esi, CURSOR_POSN
	;mov	edi, cursor_pposn ; cursor positions backup
	;mov	cl, 4
	;rep	movsd

	; 12/12/2020
	call	save_mode3_multiscreen

	; 29/07/2016
	;;mov	[ACTIVE_PAGE], cl ; 0
	;xchg	cl, [ACTIVE_PAGE]
	;mov	[p_crt_page], cl  ; previous page (for mode 3)

	; [ACTIVE_PAGE] = 0 

	jmp	short _sm_2	; case 1 - 19/11/2020
_sm_1:
	; 26/11/2020
	; 19/11/2020
	
	; case 4
	or	byte [p_crt_mode], 80h 
				; here [p_crt_mode] must be 83h
				;	  (for case 4)
				; (because video mode 03h
				; was changed before as in case 1)

_sm_2:	; case 4 (jump to _sm_2) - 19/11/2020 

	; 19/11/2020
	; case 3
	; If '_set_mode' procedure is called for video mode 3
	;     while video mode is not 3 and if there is video
	;     page backup for video mode 3, all (of 8) mode 3
	;     video pages will be restored from 98000h.

	mov	[CRT_MODE], al  ; save mode in global variable
_sm_3:
	; 04/08/2022 (TRDOS 386 v2.0.5)
	; 30/07/2016
	; 26/07/2016
	; 25/07/2016
	; set_mode_vga:
	; 18/07/2016
	; 14/07/2016
	; 09/07/2016
	; 04/07/2016
	; 03/07/2016 (TRDOS 386 = TRDOS v2.0)
	; /// video mode 13h ///
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'vgatables.h'
	;
	; Oracle VirtualBox 5.0.24 VGABios Source Code
	; ('vgabios.c', 'vgatables.h', 'vgafonts.h', 'vgarom.asm')
	;
	mov	ah, al
	mov	ecx, vga_mode_count
	mov	esi, vga_modes
	xor	ebx, ebx
_sm_4:
	lodsb
	cmp	ah, al
	je	short _sm_5
	inc	bl
	loop	_sm_4

	; UNIMPLEMENTED VIDEO MODE !
	;xor	eax, eax
        ;mov	[video_eax], eax ; 0

	; 26/11/2020
	stc	; unimplemented video mode ! (cf=1)

	retn

;-----	EBX POINTS TO CORRECT ROW OF INITIALIZATION TABLE

_sm_5: 	; 25/07/2016
	;mov	esi, ebx
	;add	esi, vga_memmodel
	;mov	al, [esi]
	; 19/11/2020
	mov	al, [ebx+vga_memmodel]
	mov	[VGA_MTYPE], al

	mov	edi, ebx
	add	edi, vga_dac_s
	shl	bl, 2 ; byte -> dword
	add	ebx, vga_mode_tbl_ptr

	;mov	dword [VGA_BASE], 0B8000h
	;cmp	ah, 0Dh ; [CRT_MODE]
	;jb	short M9 
	;mov	dword [VGA_BASE], 0A0000h
;M9:
	mov	esi, [ebx]
	mov	ebx, esi
	add	esi, vga_p_cm_pos ; ebx + 20
	mov	ax, [esi]       ; get the cursor mode from the table
	mov	[CURSOR_MODE], ax ; save cursor mode (initial value)
	; al = 6, ah = 7
	; al = 0Dh, ah = 0Eh ; 25/07/2016
	call	cursor_shape_fix
	; al = 14, ah = 15 (If [CHAR_HEIGHT] = 16)
	mov	[esi], ax

	push	esi ; *

	; 17/04/2021
	mov	dh, 03h
	;
	mov	ah, [VGA_MODESET_CTL]
	and	ah, 8 ; default palette loading ?
	jnz	short _sm_6
	;mov	dx, 3C6h ; VGAREG_PEL_MASK (DAC mask register)
	; 17/04/2021
	mov	dl, 0C6h
	mov	al, 0FFh ; PEL mask
	out	dx, al
	mov	ah, [edi] ; DAC model (selection number)
	call	load_dac_palette
	; ecx = 0
	test	byte [VGA_MODESET_CTL], 2 ; gray scale summing
	jz	short _sm_6
	push	ebx
	sub	ebx, ebx ; sub bl, bl
	;mov	cx, 256
	; 02/08/2022
	;sub	ecx, ecx
	; ecx = 0
	inc	ch
	; ecx = 256
	call	gray_scale_summing
	pop	ebx
_sm_6:
	; Reset Attribute Ctl flip-flop
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
 	; 17/03/2021
	mov	dl, 0DAh ; dx = 3DAh
	in	al, dx
	; Set Attribute Ctl
	mov	esi, ebx ; addr of params tbl for selected mode
	add	esi, 35  ; actl regs
	xor	ah, ah ; 0
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 17/04/2021
	mov	dl, 0C0h
_sm_7:
	mov	al, ah
	out	dx, al ; index
	lodsb
	; DX = 3C0h = VGAREG_ACTL_WRITE_DATA
	out	dx, al ; value
	inc	ah
	cmp	ah, 20 ; number of actl registers
	jb	short _sm_7
	;
	mov	al, ah ; 20
	out	dx, al ; index
	sub	al, al ; 0
	out	dx, al ; value
	;
	; Set Sequencer Ctl
	mov	esi, ebx ; addr of params tbl for selected mode
	add	esi, 5 ; sequ regs
	;
	;mov	dx, 3C4h  ; VGAREG_SEQU_ADDRESS
	; 17/04/2021
	mov	dl, 0C4h
	out	dx, al ; 0
	;inc	dx ; 3C5h ; VGAREG_SEQU_DATA
	; 17/04/2021
	inc	dl ; dx = 3C5h
	mov	al, 3
	out	dx, al
	mov	ah, 1
_sm_8:
	mov	al, ah
	;mov	dx, 3C4h ; VGAREG_SEQU_ADDRESS
	;dec	dx
	; 17/04/2021
	dec	dl ; dx = 3C4h
	out	dx, al ; index
	lodsb
	;inc	dx ; 3C5h ; VGAREG_SEQU_DATA
	; 17/04/2021
	inc	dl
	out	dx, al
	cmp	ah, 4 ; number of sequ regs
	jnb	short _sm_9
	inc	ah 
	jmp	short _sm_8
_sm_9:
	; Set Grafx Ctl
	mov	esi, ebx ; addr of params tbl for selected mode
	add	esi, 55 ; grdc regs
	xor	ah, ah ; 0
_sm_10:
	mov	al, ah
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	; 17/04/2021
	mov	dl, 0CEh
	out	dx, al
	lodsb
	;inc	dx ; 3CFh ; VGAREG_GRDC_DATA
	; 17/04/2021
	inc	dl ; 3CFh
	out	dx, al
	inc	ah
	cmp	ah, 9 ; number of grdc regs
	jb	short _sm_10
	;
	; Disable CRTC write protection
	;mov	dx, 3D4h  ; VGAREG_VGA_CRTC_ADDRESS
	; 17/04/2021
	mov	dl, 0D4h
	;mov	al, 11h
	;out	dx, al
	;inc	dx
	;sub	al, al
	;out	dx, al
	mov	ax, 11h
	out	dx, ax
	mov	esi, ebx ; addr of params tbl for selected mode
	add	esi, 10 ; crtc regs
	; ah = 0
_sm_11:
	mov	al, ah
	; dx = 3D4h = VGAREG_VGA_CRTC_ADDRESS
	out	dx, al ; index
	lodsb
	;inc	dx  ; VGAREG_VGA_CRTC_ADDRESS + 1
	; 17/04/2021
	inc	dl
	out	dx, al ; value
	cmp	ah, 24 ; number of crtc registers - 1
	jnb	short _sm_12
	inc	ah
	;dec	dx ; 3D4h
	; 17/04/2021
	dec	dl
	jmp	short _sm_11
_sm_12:
	; Set the misc register
	;mov	dx, 3CCh ; VGAREG_READ_MISC_OUTPUT
	; 17/04/2021
	mov	dl, 0CCh
	mov	al, [ebx+9] ; misc reg
	out	dx, al
	;
	; Enable video
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 17/04/2021
	mov	dl, 0C0h
	mov	al, 20h
        out     dx, al   ; set bit 5 to 1
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 17/04/2021
	mov	dl, 0DAh
	in	al, dx
	;
	; 17/11/2020
	;cmp	byte [noclearmem], 0
        ;ja	short _sm_15

	test	byte [noclearmem], 80h
	jnz	short _sm_15

	; 29/07/2016
	xor	eax, eax
	;mov	ecx, 4000h ; 16K words (32K)
	; 02/08/2022
	sub	ecx, ecx
	mov	ch, 40h
	; ecx = 4000h 
	cmp     byte [VGA_MTYPE], 2  ; CTEXT, MTEXT, CGA
	ja	short _sm_14    ; no ? (0A0000h)
	mov	edi, 0B8000h
	je	short _sm_13	; CGA graphics mode
	; 08/08/2016
	mov	[VGA_INT43H], eax ; 0 ; default font
	mov	ax, 0720h	; CGA text mode
_sm_13:
	rep	stosw
	jmp	short _sm_15

_sm_14:
	mov	edi, 0A0000h
	; ecx = 16384 dwords (64K)
	;mov	dx, 3C4h ; VGAREG_SEQU_ADDRESS
	; 17/04/2021
	mov	dl, 0C4h
	mov	al, 2
	out	dx, al
	;mov	dx, 3C5h ; VGAREG_SEQU_DATA
	;inc	dx
	; 17/04/2021
	inc	dl ; 3C5h
	in	al, dx ; mmask
	;push	ax
	; 12/04/2021
	push	eax
	mov	al, 0Fh ; all planes
	out	dx, al
	xor	al, al ; 0
	rep	stosd	; ecx = 163684 (64K)
	;pop	ax
	; 12/04/2021
	pop	eax
	out	dx, al  ; mmask
_sm_15:
	; ebx = addr of params tbl for selected mode
	; 10/08/2016
	mov	ax, [ebx] ; num of columns, 'twidth'
	mov	[CRT_COLS], al
	;; 26/07/2016
	;; CRTC_ADDRESS = 3D4h (always)
	;mov	ah, [ebx+1] ; num of rows, 'theightm1'
	inc	ah ; 09/07/2016
	mov	[VGA_ROWS], ah
	; 10/08/2016
	mov	al, [ebx+2]
	mov	[CHAR_HEIGHT], al
	; 29/07/2016
	; length of regen buffer in bytes
	mov	cx, [ebx+3] ; 'slength_l'
	mov	[CRT_LEN], cx
	;
	; 27/07/2016
	xor	ah, ah
	mov	al, [ACTIVE_PAGE] ; may be > 0 for mode 3
	;mul	word [CRT_LEN] ; 4096 for mode 3
	mul	cx ; 29/07/2016
	mov	[CRT_START], ax
	;
	mov	al, 60h
	;cmp	byte [noclearmem], 0
	;jna	short _sm_16
	;add	al, 80h
	or	al, [noclearmem] ; 17/11/2020
_sm_16:
	mov	[VGA_VIDEO_CTL], al
	mov	byte [VGA_SWITCHES], 0F9h
	and	byte [VGA_MODESET_CTL], 7Fh

	pop	esi ; *

	; 26/07/2016
	; 07/07/2016
	mov	cx, [CURSOR_MODE] ; restore cursor mode (initial value)
	xchg	cx, [esi] ; cl = start line, ch = end line
			  ; reset to initial value
	xchg 	ch, cl  ; ch = start line, cl = end line
	mov	[CURSOR_MODE], cx ; save (fixed) cursor mode

	; 27/07/2016
	cmp	byte [VGA_MTYPE], 2 ; CTEXT, MTEXT
	jnb	short _sm_17

	; Set cursor shape
	;mov	cx, 0607h
	;call	_set_ctype

	; 29/07/2016
	mov	ah, 10	; 6845 register for cursor set
	call	m16	; output cx register
	
	; 25/07/2016
        cmp     byte [CRT_MODE], 03h
	jne	short _sm_17
	; 26/07/2016

	mov	al, [ACTIVE_PAGE]
	jmp	short _sm_18
_sm_17:
	; Set cursor pos for page 0..7
	;sub	ax, ax ; eax = 0
	sub	eax, eax ; 17/11/2020
	mov	edi, CURSOR_POSN
	stosd
	stosd
	stosd
	stosd
	;; Set active page 0
	;mov	[ACTIVE_PAGE], al ; 0
_sm_18:
	; 29/07/2016
	cmp	byte [VGA_MTYPE], 2 ; CTEXT, MTEXT
	;jnb	_sm_23
	; 04/08/2022
	jb	short _sm_24
	jmp	_set_active_page
_sm_24:
	;cmp	byte [CHAR_HEIGHT], 16
	;je	short _sm_19

 	;; copy and activate 8x16 font

	; 26/07/2016
	mov	al, 04h
	;sub	bl, bl
	; AX = 1104H ; Load ROM 8x16 Character Set
	; (BL = font block to load (EGA: 0-3; VGA: 0-7))
	call	load_text_8_16_pat

	; video_func_1103h:
	; biosfn_set_text_block_specifier:
	; BL = font block selector code	
	; NOTE: TRDOS 386 only uses and sets font block 0
	; (It is as BL = 0 for TRDOS 386)
	mov	dx, 3C4h ; VGAREG_SEQU_ADDRESS
	;;mov	ah, bl
	;sub	ah, ah ; 0
	;mov	al, 03h
	; 19/11/2020
	mov	ax, 03h
	out	dx, ax
_sm_19:
	; 29/07/2016
	; 26/07/2016
	; 24/06/2016
	;mov	edi, 0B8000h
	;mov	cx, 4000h ; 16K words (32K)
	;
	xor	al, al
        cmp     [p_crt_mode], al ; 0
        ja      short _sm_20 ; 03h, 80h or 83h

	; case 1 - 19/11/2020
	;
	; If [pc_crt_mode] = 0, that means, previous mode is 03h
	; and current mode is not 03h (case 1)

	; 30/07/2016
	; 24/06/2016
	; TRDOS 386 (TRDOS v2) 'set mode' modification
	; (for multiscreen feature):
	; If '_set_mode' procedure is called for video mode 3
	;     while video mode is 3, video page will be cleared
	;     and cursor position of video page will be reset.
	; If '_set_mode' procedure is called for a video mode
	;     except video mode 3, while current video mode
	;     is 3, all video pages of mode 3 will be copied 
	;     to 98000h address as backup, before mode change.
	; If '_set_mode' procedure is called for video mode 3
	;     while video mode is not 3 and if there is video
	;     page backup for video mode 3, all (of 8) mode 3
	;     video pages will be restored from 98000h.

	; 19/11/2020
	;mov	[ACTIVE_PAGE], al ; 0

	; Here,
	; video memory already cleared if [noclearmem] <> 80h

	;mov	ax, 0720h
	;mov	cx, 4000h ; 16K words (32K)
	;mov	edi, 0B8000h
	;rep	stosw
	;sub	al, al

	;jmp	short _sm_23

	; Set hardware side for the new active video page

	jmp	_set_active_page ; 19/11/2020

_sm_20:
	; 19/11/2020
	; case 2 or case 3 or case 4 - 19/11/2020
	
	; 19/11/2020
	cmp	byte [CRT_MODE], 3  ; new video mode
	jne	short _sm_22 ; al = 0 (& video mode <> 03h)
			     ; case 4 - 19/11/2020
			     ; ([p_crt_mode] = 83h)

	; case 2 or case 3 - 19/11/2020	

	;movzx	ebx, byte [ACTIVE_PAGE]
	; 19/11/2020
	mov	al, [p_crt_page] ; previous mode 3 active page
	;movzx	ebx, al
	;shl	bl, 1 ; * 2
	;add	ebx, CURSOR_POSN

	; 29/07/2016
	test    byte [p_crt_mode], 7Fh ; 83h or 80h or 03h
	jz	short _sm_21	; do not restore video pages
				; case 2 - 19/11/2020 
	; case 3 - 19/11/2020

	; ([p_crt_mode] = 03h)

	; New video mode is 3 while current video mode is not 3
	; (multi screen) video pages will be restored from 098000h

	; 19/11/2020
	mov	[ACTIVE_PAGE], al ; current mode 3 active page

	; 12/12/2020
	;; restore video pages
	;mov	esi, 98000h ; 30/07/2016
	;mov	edi, 0B8000h
	;mov	cx, 2000h ; 8K dwords (32K)
	;rep	movsd
	;
	;; 19/11/2020
	;mov	[p_crt_mode], cl ; reset ('case 3' end condition)
	;
	;; restore cursor positions
	;mov	esi, cursor_pposn
	;mov	edi, CURSOR_POSN
	;;mov	ecx, 4	; restore all cursor positions (16 bytes)
	;mov	cl, 4
	;rep 	movsd

	; 12/12/2020
	call	_restore_mode3_multiscreen

	;jmp	short _sm_23 ; do not clear current video pages

	; 19/11/2020
	jmp	_set_active_page

_sm_21:
	; 19/11/2020
	; case 2 
	;
	; ([p_crt_mode] = 80h)
	;
	; User has requested to set video mode 3 again while
	; current video mode is 3.. that means, set mode 03h
	; parameters again and clear video page.
	; ('noclearmem' option effects the result)

	; 19/11/2020
	test	byte [noclearmem], 80h
	jnz	short _sm_22	; 'do not clear video memory'
				; continue with current text
				; (user's option/choice) 
	; clear video page
	;mov	cx, [CRT_LEN] ; 4096
	;shr	cx, 1 ; 2048 ; 16/11/2020
	mov	ax, 0720h
	; 26/11/2020
	;mov	ecx, 2048 ; 4096/2
	; 02/08/2022
	sub	ecx, ecx
	mov	ch, 08h
	; ecx = 0800h
	mov	edi, 0B8000h ; [crt_base]
	add	di, [CRT_START]
	rep	stosw	; FILL THE REGEN BUFFER WITH BLANKS
	;
	; 19/11/2020
	mov	al, [ACTIVE_PAGE] ; 0 to 7 (for video mode 3)
	movzx	ebx, al
	shl	bl, 1
	mov	[ebx+CURSOR_POSN], cx ; reset cursor position
_sm_22:
	;mov	[p_crt_mode], al ; 0 ; reset
	; 19/11/2020
	;and	byte [p_crt_mode], 3 ; 83h -> 3, 80h -> 0
	and	byte [p_crt_mode], 7Fh ; 83h -> 3, 80h -> 0
_sm_23:
	; al = video page number
	; [CRT_LEN] = length of regen buffer in bytes
	;call	_set_active_page
	; 16/11/2020
	jmp	_set_active_page

;-----	NORMAL RETURN FROM ALL VIDEO RETURNS
	;retn

save_mode3_multiscreen:
	; 02/08/2022 (TRDOS v2.0.5)
	; 12/12/2020 (TRDOS v2.0.3)
	; save mode 03h video pages and cursor positions
	;
	; Modified registers: ecx (=0), esi, edi
	
	; 12/12/2020
	; moved here from '_set_mode'
	; 03/07/2016
	; save video pages
	mov	esi, 0B8000h
	mov	edi, 98000h ; 30/07/2016
	;mov	ecx, (0B8000h-0B0000h)/4
	; 02/08/2022
	sub	ecx, ecx
	mov	ch, 20h
	; ecx = 2000h 
	rep	movsd

	mov	byte [p_crt_mode], 3 ; previous mode, backup sign
	; 26/11/2020
	xchg	cl, [ACTIVE_PAGE]
	mov	[p_crt_page], cl  ; save as previous active page

	; save cursor positions
	mov	esi, CURSOR_POSN
	mov	edi, cursor_pposn ; cursor positions backup
	mov	cl, 4
	rep	movsd
	retn

restore_mode3_multiscreen:
	; 02/08/2022 (TRDOS v2.0.5)
	; 12/12/2020 (TRDOS v2.0.3)
	; restore mode 03h video pages and cursor positions
	;
	; Input:
	;    settings from the last 'save_mode3_multiscreen'
	;
	; Output: 
	;    AL = active video page = [ACTIVE_PAGE]
	;
	; Modified registers: al, ecx (=0), esi, edi

	mov	al, [p_crt_page] ; previous mode 3 active page
	mov	[ACTIVE_PAGE], al ; current mode 3 active page

	; 12/12/2020
	; moved here from 'vesa_vbe3_pmi'

	; 07/12/2020
	; restore CRT_START according to ACTIVE_PAGE
	;mov	[CRT_START], cx ; 0
	; 12/12/2020
	mov	word [CRT_START], 0

	; check active page and set it again if it is not 0
	or	al, al
	;;jz	short vbe3_pmi_7
	;jz	short _restore_mode3_multiscreen
	jz	short r_m3_ms_1
	mov	cl, al
;vbe3_pmi_5:
r_m3_ms_0:
	add	word [CRT_START], 4096
	dec	cl
	;jnz	short vbe3_pmi_5
	jnz	short r_m3_ms_0
r_m3_ms_1:
	; 12/12/2020
	; moved here from '_set_mode'
_restore_mode3_multiscreen:
	; Modified registers: ecx, esi, edi

	; restore video pages
	mov	esi, 98000h ; 30/07/2016 
	mov	edi, 0B8000h
	;mov	cx, 2000h ; 8K dwords (32K)
	;mov	ecx, 2000h
	; 02/08/2022
	sub	ecx, ecx
	mov	ch, 20h
	; ecx = 2000h 
	rep	movsd

	; 19/11/2020
	mov	[p_crt_mode], cl ; reset ('case 3' end condition)

	; restore cursor positions
	mov	esi, cursor_pposn
	mov	edi, CURSOR_POSN
	;mov	ecx, 4	; restore all cursor positions (16 bytes)
	mov	cl, 4
	rep 	movsd
	retn

cursor_shape_fix:
	; 12/04/2021
	; 07/07/2016
	; (Cursor start and cursor end line values -6,7-
	; will be fixed depending on character height)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', ' biosfn_set_cursor_shape (CH,CL)'
	;
	; INPUT ->
	;	AL = cursor start line (=6)
	;	AH = cursor end line (=7)
	; OUTPUT ->
	;	AL = cursor start line (=14)
	;	AH = cursor end line (=15)
	;
	;; if((modeset_ctl&0x01)&&(cheight>8)&&(CL<8)&&(CH<0x20))

	;test	byte [VGA_MODESET_CTL], 1 ; VGA active
	;jz	short csf_3
	cmp	byte [CHAR_HEIGHT], 8
	jna	short csf_3
	cmp	ah, 8
	jnb	short csf_3
	cmp	al, 20h
	jnb	short csf_3
	;
	;push	ax
	; 12/04/2021
	push	eax
	; {
   	; if(CL!=(CH+1))
	inc	al
	cmp	ah, al   ; ah != al + 1
        je      short csf_1
	; CH = ((CH+1) * cheight / 8) -1;
	mov	ah, [CHAR_HEIGHT]
	mul	ah
	shr	al, 3 ; / 8
	dec	al ; - 1
	jmp	short csf_2 
csf_1: 	
 	; }
   	; else		; ah = al + 1
    	; {
	inc	ah	; ah = ah + 1
	; CH = ((CL+1) * cheight / 8) - 2;
	mov	al, [CHAR_HEIGHT]
	mul	ah
	shr	al, 3 ; / 8
	sub	al, 2 ; - 2
	; al = 14 (if [CHAR_HEIGHT] = 16)
csf_2:
	mov	[esp], al
	mov	ah, [esp+1]
	; CL = ((CL+1) * cheight / 8) - 1;
	inc	ah 
	mov	al, [CHAR_HEIGHT]
	mul	ah
	shr	al, 3 ; / 8
	dec	al ; - 1
	mov	[esp+1], al
	; ah = 15 (if [CHAR_HEIGHT] = 16)
	;
	;pop	ax
	; 12/04/2021
	pop	eax
csf_3:
	retn

SET_CTYPE:
	; 04/08/2022 (TRDOS 386 v2.0.5)
	; 12/09/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	cmp	byte [CRT_MODE], 7
	;ja	VIDEO_RETURN ; 12/09/2016
	; 04/08/2022
	ja	short set_cpos_inv_vp
	call	_set_ctype
        jmp     VIDEO_RETURN

_set_ctype:
	; 02/09/2014 (Retro UNIX 386 v1)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS

	; (CH) = BITS 4-0 = START LINE FOR CURSOR
	;  ** HARDWARE WILL ALWAYS CAUSE BLINK
	;  ** SETTING BIT 5 OR 6 WILL CAUSE ERRATIC BLINKING
	;     OR NO CURSOR AT ALL
	; (CL) = BITS 4-0 = END LINE FOR CURSOR

;------------------------------------------------
; SET_CTYPE
;	THIS ROUTINE SETS THE CURSOR VALUE
; INPUT
;	(CX) HAS CURSOR VALUE CH-START LINE, CL-STOP LINE
; OUTPUT	
;	NONE
;------------------------------------------------

	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	;
	; 07/07/2016
	; Fixing cursor start and stop line depending on
	; current character height (=16)
	; (Note: Default/initial values are 6 and 7.
	; If set values are 6 (start) & 7 (stop) and
	; [CHAR_HEIGHT] = 16 :
	; After fixing, start line will be 14, stop line
	; will be 15.)

	;mov	ax, cx
	; 02/08/2022
	mov	eax, ecx

	xchg	al, ah
	; AL = start line, AH = stop line
	call	cursor_shape_fix
	; AL = start line (fixed), AH = stop line (fixed)
	;mov	cx, ax
	; 02/08/2022
	mov	ecx, eax
	xchg	ch, cl
	; CH = start line (fixed), CL = stop line (fixed)
	;
	mov	ah, 10	; 6845 register for cursor set
	mov	[CURSOR_MODE], cx ; save in data area
	;call	m16	; output cx register
	;retn
        jmp     m16

SET_CPOS:
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 12/09/2016
	; 07/07/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	cmp	bh, 7 ; video page > 7 ; 07/07/2016
	;ja	VIDEO_RETURN
	; 02/08/2022
	ja	short set_cpos_inv_vp
	;
	cmp	byte [CRT_MODE], 7
	ja	short vga_set_cpos ; 12/09/2016
	call	_set_cpos
set_cpos_inv_vp:   ; 02/08/2022
        jmp     VIDEO_RETURN

vga_set_cpos:
	; 12/09/2016
	; 09/07/2016
	; set cursor position
	; NOTE: Hardware cursor position will not be set
	;   in any VGA modes (>7)
	;   But, cursor position will be saved into
	;   [CURSOR_POSN].
	;   TRDOS 386 (TRDOS v2.0) uses only one page
	;   (page 0) for all graphics modes.

	mov	[CURSOR_POSN], dx ; save cursor pos for pg 0
	; 04/08/2016
	;mov	bh, [ACTIVE_PAGE] ; = 0
	;call	_set_cpos
	jmp     VIDEO_RETURN

READ_CURSOR:
	; 12/09/2016
	; 07/07/2016
	; 12/05/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS

;------------------------------------------------------
; READ_CURSOR
;	THIS ROUTINE READS THE CURRENT CURSOR VALUE FROM THE
;	845, FORMATS IT, AND SENDS IT BACK TO THE CALLER
; INPUT
;	BH - PAGE OF CURSOR
; OUTPUT
;	DX - ROW, COLUMN OF THE CURRENT CURSOR POSITION
;	CX - CURRENT CURSOR MODE
;------------------------------------------------------

	; BH = Video page number (0 to 7)

	; 07/07/2016
	cmp	bh, 7 ; video page > 7 (invalid)
	jna	short read_cursor_1
	; invalid video page (input) 
	xor	ecx, ecx ; 0
	xor	edx, edx ; 0
	jmp	short read_cursor_2
read_cursor_1:
	; 12/09/2016
	cmp	byte [CRT_MODE], 7 ; vga mode
	ja	short vga_get_cpos
	;
	call	get_cpos
	movzx	ecx, word [CURSOR_MODE]
read_cursor_2:
	pop	ebp
	pop	edi
	pop	esi
	pop	ebx
	pop	eax  ; DISCARD SAVED CX AND DX
	pop	eax
	mov	eax, [video_eax] ; 12/05/2016
	;;15/01/2017
	;;mov	byte [intflg], 0 ; 07/01/2017
	pop	ds
	pop	es
	iretd

get_cpos:
	; 12/05/2016
	; 16/01/2016
	; BH = Video page number (0 to 7)
	;
	shl	bh, 1 ; WORD OFFSET
	movzx	esi, bh
	movzx	edx, word [esi+CURSOR_POSN]
	retn

vga_get_cpos:
	; 12/09/2016
	; get cursor position (vga)
	movzx	edx, word [CURSOR_POSN] ; cursor pos for pg 0
	xor	ecx, ecx ; Cursor Mode = 0 (invalid)
	jmp     short read_cursor_2

ACT_DISP_PAGE:
	; 07/07/2016
	; 26/06/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
;-----------------------------------------------------
; ACT_DISP_PAGE
;	THIS ROUTINE SETS THE ACTIVE DISPLAY PAGE, ALLOWING
;	THE FULL USE OF THE MEMORY SET ASIDE FOR THE VIDEO ATTACHMENT
; INPUT
;	AL HAS THE NEW ACTIVE DISPLAY PAGE
; OUTPUT
;	THE 6845 IS RESET TO DISPLAY THAT PAGE
;-----------------------------------------------------
	; 07/07/2016
	cmp	al, 7	; > 7 = invalid video page number
	;ja	VIDEO_RETURN
        ja	short adp_2 ; 18/11/2020
	;cmp	byte [CRT_MODE], 3
	;je	short adp_1
	; 18/11/2020
	mov	ah, [CRT_MODE]
	cmp	ah, 3
	jna	short adp_1 ; mode 01h, 00h (01h), 02h (03h), 03h
	cmp	ah, 7	    ; mode 07h (03h)
	jne	short adp_2
	;and 	al, al
        ;jnz	VIDEO_RETURN
	;;sub	al, al ; 0 ; force to page 0
adp_1:
	call	set_active_page
adp_2:
        jmp     VIDEO_RETURN

set_active_page:   ; tty_sw
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 09/12/2017
	; 26/07/2016
	; 26/06/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 30/06/2015
	; 04/03/2014  (act_disp_page --> tty_sw)
	; 10/12/2013
	; 04/12/2013
	;
	mov	[ACTIVE_PAGE], al ; save active page value ; [ptty]
_set_active_page:
	; 27/06/2015
	;movzx	ebx, al
	; 02/08/2022
	movzx	eax, al
	mov	ebx, eax
	;
	;cbw	; 07/09/2014 (ah=0)
	; 02/08/2022
	;sub	ah, ah ; 09/12/2017
	mul	word [CRT_LEN]  ; get saved length of regen buffer
				; display page times regen length
	; 10/12/2013
	mov	[CRT_START], ax ; save start address for later
	;mov	cx, ax ; start address to cx
	; 02/08/2022
	mov	ecx, eax
_M16:
	;;sar	cx, 1
	;shr	cx, 1	; divide by 2 for 6845 handling
	; 02/08/2022
	shr	ecx, 1
	mov	ah, 12	; 6845 register for start address
	call	m16
	;sal	bx, 1
	; 01/09/2014
	shl	bl, 1	; *2 for word offset
	add	ebx, CURSOR_POSN
	mov	dx, [ebx] ; get cursor for this page
	; 16/01/2016
	;call	m18
	;retn
	jmp	m18

position:
	; 02/08/2022 - TRDOS 386 v2.0.5
	; 17/04/2021 - TRDOS 386 v2.0.4
	; 24/06/2016
	; 12/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 27/06/2015
	; 02/09/2014
	; 30/08/2014 (Retro UNIX 386 v1)
	; 04/12/2013 (Retro UNIX 8086 v1)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
;-----------------------------------------
; POSITION
;	THIS SERVICE ROUTINE CALCULATES THE REGEN BUFFER ADDRESS
;	OF A CHARACTER IN THE ALPHA MODE
; INPUT
;	AX = ROW, COLUMN POSITION
; OUTPUT
;	AX = OFFSET OF CHAR POSITION IN REGEN BUFFER
;-----------------------------------------

	; DX = ROW, COLUMN POSITION
	;movzx	eax, byte [CRT_COLS] ; 27/06/2015
	xor	eax, eax ; 02/09/2014
	mov	al, 80   ; determine bytes to row
	mul	dh	 ; row value
	;xor	dh, dh   ; 0
	;add	ax, dx	 ; add column value to the result
	; 16/04/2021
	add	al, dl
	adc	ah, 0
	; 02/08/2022
	shl	eax, 1
	;shl	ax, 1	; * 2 for attribute bytes
		; EAX = AX = OFFSET OF CHAR POSITION IN REGEN BUFFER
	retn

find_position:
	; 17/04/2021 - TRDOS 386 v2.0.4
	; 24/06/2016
	; 12/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 27/06/2015
	; 07/09/2014
	; 02/09/2014
	; 30/08/2014 (Retro UNIX 386 v1)
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS

	movzx	ecx, bh ; video page number
	; 17/04/2021
	;mov	esi, ecx
	;shl	si, 1
	;mov	dx, [esi+CURSOR_POSN]
	;jz	short p21
	;xor	si, si
	; 17/04/2021
	xor	esi, esi
	shl	cl, 1
	mov	dx, [ecx+CURSOR_POSN]
	jz	short p21
	shr	cl, 1
p20:
	add	si, [CRT_LEN] ; 24/06/2016
	;add	si, 80*25*2 ; add length of buffer for one page
	loop	p20
p21:
	and	dx, dx
	jz	short p22
	call 	position ; determine location in regen in page
	add	esi, eax ; add location to start of regen page
p22:	
	;mov	dx, [addr_6845] ; get base address of active display
	;mov	dx, 03D4h ; I/O address of color card
	;add	dx, 6	; point at status port
	mov	dx, 03DAh ; status port
	; cx = 0
	retn

SCROLL_UP:
	; 04/08/2022 (TRDOS 386 v2.0.5)
	; 07/07/2016
	; 26/06/2016
	; 12/05/2016
	; 30/01/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 07/09/2014
	; 02/09/2014
	; 01/09/2014 (Retro UNIX 386 v1 - beginning)
	; 04/04/2014
	; 04/12/2013
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
;----------------------------------------------
; SCROLL UP
;	THIS ROUTINE MOVES A BLOCK OF CHARACTERS UP
;	ON THE SCREEN
; INPUT
;	(AH) = CURRENT CRT MODE
;	(AL) = NUMBER OF ROWS TO SCROLL
;	(CX) = ROW/COLUMN OF UPPER LEFT CORNER
;	(DX) = ROW/COLUMN OF LOWER RIGHT CORNER
;	(BH) = ATTRIBUTE TO BE USED ON BLANKED LINE
;	(DS) = DATA SEGMENT
;	(ES) = REGEN BUFFER SEGMENT
; OUTPUT
;	NONE -- THE REGEN BUFFER IS MODIFIED
;--------------------------------------------

	; 07/07/2016
	cmp	ch, dh
	;ja	VIDEO_RETURN
	; 04/08/2022
	ja	short _s_u_retn

	cmp	cl, dl
	;ja	VIDEO_RETURN
	; 04/08/2022
	ja	short _s_u_retn
	;
	call	_scroll_up
_s_u_retn:
        jmp     VIDEO_RETURN

_scroll_up:  ; from 'write_tty'
	;
	; cl = left upper column
	; ch = left upper row
	; dl = right lower column
	; dh = right lower row
	;
	; al = line count 
	; bl = attribute to be used on blanked line
	; bh = video page number (0 to 7)

	call	test_line_count ; 16/01/2016

	mov	ah, [CRT_MODE] ; current video mode
	;;cmp	byte [CRT_MODE], 4
	;cmp	ah, 4 ; 07/07/2016
	;jnb	GRAPHICS_UP ; 26/06/2016
	; 18/11/2020
	cmp	ah, 4
 	jb	short n0
	cmp	ah, 7 ; TEST FOR BW CARD 
		      ;	(80x25 text, mono)
	je	short n0 ; same with mode 3 for TRDOS 386
	jmp	GRAPHICS_UP
n0:
	; 07/07/2016
	cmp	bh, 7 ; video page number
	jna	short n1
	mov	bh, [ACTIVE_PAGE]
n1:
	mov	ah, bl ; attribute
	;push	ax ; *
	; 12/04/2021
	push	eax ; *
	;mov 	esi, [CRT_BASE]
        mov     esi, 0B8000h  
        cmp     bh, [ACTIVE_PAGE]
	jne	short n2
	;
        mov     ax, [CRT_START]
        add     si, ax
        jmp     short n4
n2:
        and     bh, bh
	jz	short n4
	mov	al, bh
n3:
        add	si, [CRT_LEN]
        dec	al
	jnz	short n3
n4:
	call	scroll_position ; 16/01/2016
        jz      short n6 

        add     esi, ecx ; from address for scroll
	mov	ch, dh  ; #rows in block
	sub	ch, al	; #rows to be moved
n5:
	call	n10 ; 16/01/2016
	
        push	ecx
	movzx	ecx, byte [CRT_COLS] 
	add	cl, cl
        add	esi, ecx  ; next line
        add	edi, ecx
	pop	ecx

	dec	ch	 ; count of lines to move
	jnz	short n5 ; row loop
	; ch = 0
	mov	dh, al	 ; #rows
n6:
	; attribute in ah
	mov	al, ' '	 ; fill with blanks
n7:
	call	n11 ; 16/01/2016

	mov	cl, [CRT_COLS]
	add	cl, cl
        add	edi, ecx

	dec	dh
	jnz	short n7
n16:
	cmp	bh, [ACTIVE_PAGE]
	jne	short n8
	
	;cmp	byte [CRT_MODE], 7 ; is this the black and white card
	;je	short n8	   ; if so, skip the mode reset

	mov	al, [CRT_MODE_SET] ; get the value of mode set
	mov	dx, 03D8h ; always set color card port
	out	dx, al
n8:
	retn

test_line_count:
	; 12/04/2021
	; 12/05/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 07/09/2014 (scroll_up)
	or	al, al
	jz	short al_set2
	;push	dx
	; 12/04/2021
	push	edx
	sub	dh, ch  ; subtract upper row from lower row number
	inc	dh	; adjust difference by 1
	cmp	dh, al 	; line count = amount of rows in window?
	jne	short al_set1 ; if not the we're all set
	xor	al, al	; otherwise set al to zero
al_set1:
	;pop	dx
	; 12/04/2021
	pop	edx
al_set2:
	retn

scroll_position:
	; 12/04/2021
	; 26/06/2016
	; 30/01/2016
        ; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 07/09/2014 (scroll_up)

	; (*) [esp+4] = ax (al = line count, ah = attribute)

	;push	dx
	; 12/04/2021
	push	edx
	mov	dx, cx	; now, upper left position in DX
	call	position
	add	esi, eax
	mov	edi, esi
	;pop	dx	; lower right position in DX
	; 12/04/2021
	pop	edx
	sub	dx, cx
	inc	dh	; dh = #rows 
	inc	dl	; dl = #cols in block
	pop	ecx 	; return address
	;pop	ax	; * ; al = line count, ah = attribute
	; 12/04/2021
	pop	eax ; (*)
	push	ecx	; return address
	movzx	ecx, ax
	mov	ah, [CRT_COLS]
	mul	ah	; determine offset to from address
	;add	ax, ax  ; *2 for attribute byte
	; 02/08/2022
	;shl	eax, 1
	add	eax, eax
	;
	;push	ax	; offset
	;push	dx
	; 12/04/2021
	push	eax	; offset
	push	edx
	;
	; 04/04/2014
	mov	dx, 3DAh ; guaranteed to be color card here
n9:                      ; wait_display_enable
        in      al, dx   ; get port
	test	al, RVRT ; wait for vertical retrace
	jz	short n9 ; wait_display_enable
	mov	al, 25h
	mov	dl, 0D8h ; address control port
	out	dx, al	; turn off video during vertical retrace
	;pop	dx	; #rows, #cols
       	;pop	ax	; offset
	; 12/04/2021
	pop	edx	; #rows, #cols
       	pop	eax	; offset
	xchg	ax, cx	; 
	; ecx = offset, al = line count, ah = attribute
	;
	or	al, al
	retn
n10:
	; Move rows
	mov	cl, dl	; get # of cols to move
	push	esi
	push	edi	; save start address
n10r:
	movsw		; move that line on screen
	dec	cl
        jnz     short n10r
	pop	edi
	pop	esi	; recover addresses
	retn
n11:
	; Clear rows
                	; dh =  #rows
        mov	cl, dl	; get # of cols to clear
        push    edi     ; save address
n11r:
        stosw           ; store fill character
	dec	cl
        jnz     short n11r
        pop     edi     ; recover address
	retn

SCROLL_DOWN:
	; 12/04/2021
	; 07/07/2016
	; 27/06/2016
	; 26/06/2016
	; 12/05/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS

;------------------------------------------
; SCROLL DOWN
;	THIS ROUTINE MOVES THE CHARACTERS WITHIN A DEFINED
;	BLOCK DOWN ON THE SCREEN, FILLING THE TOP LINES
;	WITH A DEFINED CHARACTER
; INPUT
;	(AH) = CURRENT CRT MODE
;	(AL) = NUMBER OF LINES TO SCROLL
;	(CX) = UPPER LEFT CORNER OF RECION
;	(DX) = LOWER RIGHT CORNER OF REGION
;	(BH) = FILL CHARACTER
;	(DS) = DATA SEGMENT
;	(ES) = REGEN SEGMENT
; OUTPUT
;	NONE -- SCREEN IS SCROLLED
;------------------------------------------

	; 07/07/2016
	cmp	ch, dh
	;ja	VIDEO_RETURN
	ja	short _s_d_retn ; 18/11/2020
	cmp	cl, dl
	;ja	VIDEO_RETURN
	ja	short _s_d_retn ; 18/11/2020
	;
	call	_scroll_down
_s_d_retn:
        jmp     VIDEO_RETURN

_scroll_down: ; 27/06/2016

	; cl = left upper column
	; ch = left upper row
	; dl = right lower column
	; dh = right lower row
	;
	; al = line count 
	; bl = attribute to be used on blanked line
	; bh = video page number (0 to 7)

	; !!!!
	std		; DIRECTION FOR SCROLL DOWN
	; !!!!
	call	test_line_count ; 16/01/2016
	
	mov	ah, [CRT_MODE] ; current video mode
	;;cmp	byte [CRT_MODE], 4
	;cmp	ah, 4 ; 07/07/2016
	;jnb	GRAPHICS_DOWN ; 26/06/2016
	; 18/11/2020
	cmp	ah, 4
 	jb	short _n0
	cmp	ah, 7 ; TEST FOR BW CARD 
		      ;	(80x25 text, mono)
	je	short _n0 ; same with mode 3 for TRDOS 386
	jmp	GRAPHICS_DOWN
_n0:
	; 07/07/2016
	cmp	bh, 7 ; video page number
	jna	short n12
	mov	bh, [ACTIVE_PAGE]
	;
n12:			; CONTINUE_DOWN
	mov	ah, bl
	;push	ax	; * ; save attribute in ah
	; 12/04/2021
	push	eax
	mov	ax, dx	; LOWER RIGHT CORNER
	call	scroll_position	; GET REGEN LOCATION
	jz	short n14
	sub	esi, ecx  ; SI IS FROM ADDRESS
	mov	ch, dh  ; #rows in block
	sub	ch, al	; #rows to be moved
n13:
	call	n10	; MOVE ONE ROW

	push	ecx
	mov	cl, [CRT_COLS]
	add	cl, cl
        sub	esi, ecx  ; next line
        sub	edi, ecx
        pop	ecx

	dec	ch	 ; count of lines to move
	jnz	short n13 ; row loop
	; ch = 0
	mov	dh, al	 ; #rows
n14:
	; attribute in ah
	mov	al, ' '	 ; fill with blanks
n15:
	call	n11 ; 16/01/2016

	mov	cl, [CRT_COLS]
	add	cl, cl
        sub	edi, ecx

	dec	dh
	jnz	short n15
	;
	; 18/11/2020
	cld	; clear direction flag
	;
	jmp	n16 ; 27/06/2016

;	cmp	bh, [ACTIVE_PAGE]
;	jne	short n16
;
;	;cmp	byte [CRT_MODE], 7	; is this the black and white card
;	;je	short n16		; if so, skip the mode reset
;
;	mov	al, [CRT_MODE_SET] ; get the value of mode set
;	mov	dx, 03D8h ; always set color card port
;	out	dx, al
;n16:
;	; !!!!
;	cld		; Clear direction flag !
;	; !!!!
;	retn

READ_AC_CURRENT:
	; 08/07/2016
	; 26/06/2016
	; 12/05/2016
	; 18/01/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
	; 08/07/2016
        cmp     byte [CRT_MODE], 7 ; 6!?
	jna	short read_ac_c
	xor	eax, eax
        jmp     _video_return 
read_ac_c:
	call	_read_ac_current
	; 12/05/2016
        ;jmp     VIDEO_RETURN
	jmp	_video_return

;------------------------------------------------------------------------
; READ_AC_CURRENT							:
;	THIS ROUTINE READS THE ATTRIBUTE AND CHARACTER AT THE CURRENT	:
;	CURSOR POSITION AND RETURNS THEM TO THE CALLER			:
; INPUT									:
;	(AH) = CURRENT CRT MODE						:
;	(BH) = DISPLAY PAGE ( ALPHA MODES ONLY )			:
;	(DS) = DATA SEGMENT						:
;	(ES) = REGEN SEGMENT						:
; OUTPUT								:
;	(AL) = CHARACTER READ						:
;	(AH) = ATTRIBUTE READ						:
;------------------------------------------------------------------------

_read_ac_current:
	; 26/06/2016
	; 12/05/2016 
	; 18/01/2016

	mov	ah, [CRT_MODE] ; current video mode
	cmp	ah, 4
 	jb	short p10
	; 18/11/2020
	;cmp	byte [CRT_MODE], 4
	;jnb	GRAPHICS_READ ; 26/06/2016

	cmp	ah, 7 ; TEST FOR BW CARD (80x25 monochrome text)
	je	short p10	 ; same with mode 3 in TRDOS 386
	jmp	GRAPHICS_READ
p10:
	call	find_position	; GET REGEN LOCATION AND PORT ADDRESS
	;
	; esi = regen location
	; dx = status port
	;

	; 18/11/2020
	; convert display mode to a zero value
	; for 80 column color mode
	;mov	ah, [CRT_MODE]
	;sub	ah, 2
	;shr	ah, 1
	;jnz	short p13

	; 05/12/2020
	; 18/11/2020 (TRDOS 386 v2.0.3)
	;xor	bl, bl	; 0
	cmp	byte [CRT_MODE], 03h ; 80x25 color text
	;je	short p11    ; Note: Only mode 03h and mode 01h are
	;inc	bl	; 1  ;       in use by TRDOS 386 as text modes
	;jmp	short p14    ;       (07h, 00h and 02h are redirected)
	jne	short p13

	; 05/12/2020
	cmp     bh, [ACTIVE_PAGE]
	jne	short p13 

	; WAIT FOR HORIZONTAL RETRACE OR VERTICAL RETRACE IF COLOR 80
p11:
	sti		; enable interrupts first
	; 05/12/2020
	nop
	; 18/11/2020
	;or	bl, bl
	;jnz	short p13 ; mode 01h (and mode 00h) - 40x25 color text
	cli 		; block interrupts for single loop
	in	al, dx	; get status from the adapter
	test	al, RHRZ ; is horizontal retrace low
	jnz	short p11 ; wait until it is
p12:			;  wait for either retrace high
	in	al, dx ; get status again
	test	al, RVRT+RHRZ ; is horizontal or vertical retrace high
	jz	short p12 ; wait until either retrace active
;p14:
	sti
p13:
	add	esi, 0B8000h 
	mov	ax, [esi]

	retn	; 18/01/2016

WRITE_AC_CURRENT:
	; 08/07/2016
	; 26/06/2016
	; 24/06/2016
	; 12/05/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
;----------------------------------------------------------------
; WRITE_AC_CURRENT						:
;	THTS ROUTINE WRITES THE ATTRIBUTE AND CHARACTER		:
;	AT THE CURRENT CURSOR POSITION				:
; INPUT								:
;	(AH) = CURRENT CRT MODE					:
;	(BH) = DISPLAY PAGE					:
;	(CX) = COUNT OF CHARACTERS TO WRITE			:
;	(AL) = CHAR TO WRITE					:
;	(BL) = ATTRIBUTE OF CHAR TO WRITE			:
;	(DS) = DATA SEGMENT					:
;	(ES) = REGEN SEGMENT					:
; OUTPUT							:
;	DISPLAY REGEN BUFFER UPDATED				:
;----------------------------------------------------------------

	; 08/07/2016
	cmp	byte [CRT_MODE], 7 ; 6!?
	jna	short write_ac_c

	call	vga_write_char_attr
	jmp     VIDEO_RETURN

write_ac_c:
	call	_write_c_current

	movzx	esi, bh ; video page number (0 to 7)
	mov	[esi+chr_attrib], bl ; color/attribute

        jmp     VIDEO_RETURN

WRITE_C_CURRENT:
	; 08/07/2016
	; 26/06/2016
	; 12/05/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
;----------------------------------------------------------------
; WRITE_C_CURRENT						:
;	THIS ROUTINE WRITES THE CHARACTER AT			:
;	THE CURRENT CURSOR POSITION, ATTRIBUTE UNCHANGED	:
; INPUT								:
;	(AH) = CURRENT CRT MODE					:
;	(BH) = DISPLAY PAGE					:
;	(CX) = COUNT OF CHARACTERS TO WRITE			:
;	(AL) = CHAR TO WRITE					:
;	(DS) = DATA SEGMENT					:
;	(ES) = REGEN SEGMENT					:
; OUTPUT							:
;	DISPLAY REGEN BUFFER UPDATED				:
;----------------------------------------------------------------

	; 08/07/2016
	cmp	byte [CRT_MODE], 7 ; 6!?
	jna	short write_c_c

	call	vga_write_char_only
	jmp     VIDEO_RETURN	

write_c_c:
	;and	bh, 7 ; video page number (<= 7)
	movzx	esi, bh	
	mov	bl, [esi+chr_attrib]

	call	_write_c_current
        jmp     VIDEO_RETURN

_write_c_current:  ; from 'write_tty'
	; 12/04/2021
	; 18/11/2020
	; 26/06/2016
	; 24/06/2016
	; 12/05/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 30/08/2014 (Retro UNIX 386 v1)
	; 18/01/2014
	; 04/12/2013
	;
	; VIDEO.ASM - 06/10/85 VIDEO DISPLAY BIOS

	; 18/11/2020
	mov	ah, [CRT_MODE] ; current video mode
	cmp	ah, 4
 	jb	short p40

	;cmp	byte [CRT_MODE], 4
        ;jnb	GRAPHICS_WRITE ; 26/06/2016

	cmp	ah, 7 ; TEST FOR BW CARD
	je	short p40
	jmp	GRAPHICS_WRITE
p40:
	; al = character
	; bl = color/attribute
	; bh = video page
	; cx = count of characters to write
	;push	dx
	; 12/04/2021
	push	edx ; *
	mov	ah, bl  ; color/attribute (12/05/2016)
	;push	ax	; save character & attribute/color
	;push	cx
	; 12/04/2021
	push	eax ; ** ; save character & attribute/color
	push	ecx ; ***
	call 	find_position  ; get regen location and port address
	;pop	cx
	; 12/04/2021
	pop	ecx ; ***
	; esi = regen location
	; dx = status port
	;
	add	esi, 0B8000h ; 30/08/2014 (crt_base) 
	;
	; 18/11/2020
	; convert display mode to a zero value 
	; for 80 column color mode
	;mov	ah, [CRT_MODE]	
	;sub	ah, 2
	;shr	ah, 1
	;jnz	short p44    ; 26/06/2016

	; 05/12/2020
	; 18/11/2020 (TRDOS 386 v2.0.3)
	;xor	bl, bl	; 0
	cmp	byte [CRT_MODE], 03h ; 80x25 color text
	;je	short p41    ; Note: Only mode 03h and mode 01h are
	;inc	bl	; 1  ;       in use by TRDOS 386 as text modes
	;jmp	short p43    ;       (07h, 00h and 02h are redirected)
	; 05/12/2020
	jne	short p44
p46:
	; 05/12/2020
	cmp     bh, [ACTIVE_PAGE]
	jne	short p44

	; WAIT FOR HORIZONTAL RETRACE OR VERTICAL RETRACE IF COLOR 80
p41:
	; 05/12/2020
	sti		; enable interrupts first
	nop
	; 18/11/2020
	;or	bl, bl
	;jnz	short p44 ; mode 01h (and mode 00h) - 40x25 color text
	cli 		; block interrupts for single loop
	in	al, dx	; get status from the adapter
	test	al, RVRT ; check for vertical retrace first
	jnz	short p43 ; Do fast write now if vertical retrace
	test	al, RHRZ ; is horizontal retrace low
	jnz	short p41 ; wait until it is
p42:			;  wait for either retrace high
	in	al, dx ; get status again
	test	al, RVRT+RHRZ ; is horizontal or vertical retrace high
	jz	short p42 ; wait until either retrace active
p43:
	sti
p44:
	mov	ax, [esp] ; restore the character (al) & attribute (ah)
	mov	[esi], ax

	dec	cx
	jz	short p45

	inc	esi
	inc	esi

	; 05/12/2020
	cmp	byte [CRT_MODE], 03h
	jne	short p44
	;jmp	short p41
	jmp	short p46
p45:	
	;pop	ax
	;pop	dx
	; 12/04/2021
	pop	eax ; **
	pop	edx ; *
	retn

; 09/07/2016
; 26/06/2016
; 24/06/2016
; 12/05/2016
; 18/01/2016
; 16/01/2016 - TRDOS 386 (TRDOS v2.0)
; 30/06/2015
; 27/06/2015
; 11/03/2015
; 02/09/2014
; 30/08/2014
; VIDEO FUNCTIONS
; (write_tty - Retro UNIX 8086 v1 - U9.ASM, 01/02/2014)

WRITE_TTY:
	; 09/12/2017
	; 09/07/2016
	; 01/07/2016
	; 26/06/2016
	; 24/06/2016
	; 13/05/2016
	; 12/05/2016
	; 30/01/2016
	; 18/01/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 13/08/2015
	; 02/09/2014
	; 30/08/2014 (Retro UNIX 386 v1 - beginning)
	; 01/02/2014 (Retro UNIX 8086 v1 - last update)
	; 03/12/2013 (Retro UNIX 8086 v1 - beginning)	
	; (Modified registers: EAX, EBX, ECX, EDX, ESI, EDI)
	;
	; INPUT -> AL = Character to be written
	;	   BL = Color (Forecolor, Backcolor)
	;	   BH = Video Page (0 to 7)

	; 09/07/2016
        cmp     byte [CRT_MODE], 7
	jna 	short write_tty_cga

	call	vga_write_teletype
	jmp	VIDEO_RETURN

write_tty_cga:
	; 13/05/2016
	;call	_write_tty
	; 01/07/2016
	call	_write_tty_m3
	jmp	VIDEO_RETURN

RVRT	equ	00001000b	; VIDEO VERTICAL RETRACE BIT
RHRZ	equ	00000001b	; VIDEO HORIZONTAL RETRACE BIT

; Derived from "WRITE_TTY" procedure of IBM "pc-at" rombios source code
; (06/10/1985), 'video.asm', INT 10H, VIDEO_IO
;
; 06/10/85  VIDEO DISPLAY BIOS
;
;--- WRITE_TTY ------------------------------------------------------------------
;										:
;   THIS INTERFACE PROVIDES A TELETYPE LIKE INTERFACE TO THE			:
;   VIDEO CARDS. THE INPUT CHARACTER IS WRITTEN TO THE CURRENT			:
;   CURSOR POSITION, AND THE CURSOR IS MOVED TO THE NEXT POSITION.		:
;   IF THE CURSOR LEAVES THE LAST COLUMN OF THE FIELD, THE COLUMN		:
;   IS SET TO ZERO, AND THE ROW VALUE IS INCREMENTED. IF THE ROW		:
;   ROW VALUE LEAVES THE FIELD, THE CURSOR IS PLACED ON THE LAST ROW,		:
;   FIRST COLUMN, AND THE ENTIRE SCREEN IS SCROLLED UP ONE LINE.		:
;   WHEN THE SCREEN IS SCROLLED UP, THE ATTRIBUTE FOR FILLING THE		:
;   NEWLY BLANKED LINE IS READ FROM THE CURSOR POSITION ON THE PREVIOUS		:
;   LINE BEFORE THE SCROLL, IN CHARACTER MODE. IN GRAPHICS MODE,		:
;   THE 0 COLOR IS USED.							:
;   ENTRY --									:
;     (AH) = CURRENT CRT MODE							:
;     (AL) = CHARACTER TO BE WRITTEN						:
;	    NOTE THAT BACK SPACE, CARRIAGE RETURN, BELL AND LINE FEED ARE	:
;	    HANDLED AS COMMANDS RATHER THAN AS DISPLAY GRAPHICS CHARACTERS	:
;     (BL) = FOREGROUND COLOR FOR CHAR WRITE IF CURRENTLY IN A GRAPHICS MODE	:
;   EXIT -- 									:
;     ALL REGISTERS SAVED							:
;--------------------------------------------------------------------------------

; 02/08/2022 (TRDOS 386 v2.0.5)
; 18/11/2020 (TRDOS 386 v2.0.3)
; 09/12/2017
; 08/07/2016
; 26/06/2016
; 24/06/2016
_write_tty:
	; 13/05/2016
	; --- 18/11/2020 ---
	; NOTE:
	; Only kernel calls "_write_tty" procedure...
	; TRDOS 386 v2 kernel uses video mode 3 for displaying
	; (some error) messages and also mainprog command interpreter
	; (in kernel) uses "_write_tty".
	; So, here video mode must be set to 3 if it is not 3.

	cli	; disable interrupts
	;
	; 01/09/2014
	cmp	byte [CRT_MODE], 3
	je	short _write_tty_m3
	;
set_mode_3:
	push	ebx
	push	eax
	;call	_set_mode
	; 18/11/2020 
	call	set_txt_mode  ; set video mode to 03h
	pop	eax
	pop	ebx
	;
_write_tty_m3: ; 24/06/2016 (m3 -> _write_tty_m3)
	movzx 	esi, bh ; 12/05/2016
	;shl	si, 1
	; 02/08/2022
	shl	esi, 1
	add	esi, CURSOR_POSN
	mov	dx, [esi]
	;
	; dx now has the current cursor position
	;
	cmp	al, 0Dh	; CR	; is it carriage return or control character
	;jbe	short u8
	; 17/04/2021
	jb	short u8
	je	short u9
	;
	; write the char to the screen
u0:
	; al = character
	; bl = attribute/color
	; bh = video page number (0 to 7)
	;
	;mov	cx, 1  ; 24/06/2016
	; 02/08/2022
	sub	ecx, ecx
	inc	cl ; ecx = 1
	; cx = count of characters to write
	;
	call	_write_c_current ; 16/01/2015
	;
	; position the cursor for next char
	inc	dl		; next column
	cmp	dl, [CRT_COLS]  ; test for column overflow 
	;jne	_set_cpos
	; 02/08/2022
	je	short u13
	jmp	_set_cpos
u13:
	mov	dl, 0		; column = 0
u10:				; (line feed found)
	cmp	dh, 25-1 	; check for last row
	jb 	short u6
	;
	; scroll required
u1:	
	; SET CURSOR POSITION (04/12/2013)
	call	_set_cpos
	;
	; determine value to fill with during scroll
u2:
	; bh = video page number
	;
	call	_read_ac_current ; 18/01/2016
	;
	; al = character, ah = attribute
	; bh = video page number
	; 18/11/2020
	mov	bl, ah ; color/attribute
u3:
	;;mov	ax, 0601h 	; scroll one line
	;;sub	cx, cx		; upper left corner
	;;mov	dh, 25-1 	; lower right row
	;;;mov	dl, [CRT_COLS]
	;mov	dl, 80		; lower right column
	;;dec	dl
	;;mov	dl, 79

	;;call	scroll_up	; 04/12/2013
	;;; 11/03/2015
	; 02/09/2014
	;;;mov	cx, [crt_ulc] ; Upper left corner  (0000h)
	;;;mov	dx, [crt_lrc] ; Lower right corner (184Fh)
	; 11/03/2015
	;sub	cx, cx
	; 17/04/2021
	sub	ecx, ecx
	;mov	dx, 184Fh ; dl = 79 (column), dh = 24 (row)
	; 18/11/2020
	mov	dh, 25-1
	mov	dl, [CRT_COLS]
	dec	dl
	;
	mov	al, 1		; scroll 1 line up
		; ah = attribute
	;mov	bl, al ; 12/05/2016
	jmp	_scroll_up	; 16/01/2016
;u4:
	;;int	10h		; video-call return
				; scroll up the screen
				; tty return
;u5:
	;retn			; return to the caller

u6:				; set-cursor-inc
	inc	dh		; next row
				; set cursor
;u7:
	;;mov	ah, 02h
	;;jmp	short u4 	; establish the new cursor
	;call	_set_cpos
	;jmp 	short u5
	jmp     _set_cpos

	; check for control characters
u8:
	;je	short u9 ; 17/04/2021
	cmp	al, 0Ah		; is it a line feed (0Ah)
	je	short u10
	cmp	al, 07h 	; is it a bell
	je	short u11
	cmp	al, 08h		; is it a backspace
	;jne	short u0
	je	short bs	; 12/12/2013
	; 12/12/2013 (tab stop)
	cmp	al, 09h		; is it a tab stop
	jne	short u0
	mov	al, dl
	;cbw
	xor	ah, ah ; 09/12/2017
	mov	cl, 8
	div	cl
	sub	cl, ah
ts:
	; 02/09/2014
	; 01/09/2014
	;mov	al, 20h
tsloop:
	;push	cx
	; 12/04/2021
	push	ecx ; *
	; 18/11/2020
	;push	ax
	;;mov	bh, [ACTIVE_PAGE]
	; 05/12/2020
	;push	bx
	mov	al, 20h ; al = space (blank) 
			; bl = color/attribute
	call	_write_tty_m3 ; 24/06/2016 (m3 -> _write_tty_m3)
	; 05/12/2020
	; bx is preserved in '_write_tty_m3'
	; 18/11/2020
	;pop	bx  ; BL = color/attribute, Bh = video page
	;pop	ax  ; AL = character
	; 12/04/2021
	;pop	cx
	pop	ecx ; *
	dec	cl
	jnz	short tsloop
	retn
bs:
	; back space found

	or	dl, dl 		; is it already at start of line
	;je	short u7 	; set_cursor
	jz	short _set_cpos
	;dec	dx     		; no -- just move it back
	; 17/04/2021
	dec	dl		; move to 1 column back
	;jmp	short u7
	jmp	short _set_cpos

	; carriage return found
u9:
	mov	dl, 0 		; move to first column
	;jmp	short u7
	;jmp	short _set_cpos ; 30/01/2016

	; line feed found
;u10:
;	cmp	dh, 25-1 	; bottom of screen
;	jne	short u6 	; no, just set the cursor
;       jmp     u1              ; yes, scroll the screen

_set_cpos:
	; 02/08/2022 - TRDOS 386 v2.0.5
	; 17/04/2021 - TRDOS 386 v2.0.4
	; 12/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 27/06/2015
	; 01/09/2014
	; 30/08/2014 (Retro UNIX 386 v1)
	;
	; 04/12/2013 - 12/12/2013 (Retro UNIX 8086 v1)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
;----------------------------------------------
; SET_CPOS
;	THIS ROUTINE SETS THE CURRENT CURSOR POSITION TO THE
;	NEW X-Y VALUES PASSED
; INPUT
;	DX - ROW,COLUMN OF NEW CURSOR
;	BH - DISPLAY PAGE OF CURSOR
; OUTPUT
;	CURSOR ID SET AT 6845 IF DISPLAY PAGE IS CURRENT DISPLAY
;----------------------------------------------
	;
	mov	esi, CURSOR_POSN
        movzx   eax, bh	; BH = video page number
;	or	al, al
;	jz	short _set_cpos_0
        shl     al, 1   ; word offset
        add     esi, eax
;_set_cpos_0:
	mov	[esi], dx ; save the pointer
	cmp	[ACTIVE_PAGE], bh
	jne	short m17
	;call	m18	; CURSOR SET
;m17:			; SET_CPOS_RETURN
	; 01/09/2014
;	retn
		; DX = row/column
m18:
	call	position ; determine location in regen buffer
	mov	cx, [CRT_START]
	add	cx, ax  ; add char position in regen buffer
			; to the start address (offset) for this page
	shr	cx, 1	; divide by 2 for char only count
	mov	ah, 14	; register number for cursor
	;call	m16	; output value to the 6845
	;retn

	;-----	THIS ROUTINE OUTPUTS THE CX REGISTER
	;	TO THE 6845 REGISTERS NAMED IN (AH)
m16:
	cli
	;mov	dx, [addr_6845] ; address register
	mov 	dx, 03D4h ; I/O address of color card
	mov	al, ah	; get value
	out	dx, al	; register set
	;inc	dx	; data register
	; 17/04/2021
	inc	dl
	jmp	$+2	; i/o delay
	mov	al, ch	; data
	out	dx, al
	;dec	dx
	; 17/04/2021
	dec	dl
	mov	al, ah
	inc	al	; point to other data register
	out	dx, al	; set for second register
	;inc	dx
	; 17/04/2021
	inc	dl
	jmp	$+2	; i/o delay
	mov	al, cl	; second data value
	out	dx, al
	sti
m17:
	retn

_beep:
	; 12/02/2021 (TRDOS v2.0.3)
	cli
	call	beep
	sti
	retn

beeper: 
	; 04/08/2016
	; 12/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 30/08/2014 (Retro UNIX 386 v1)
	; 18/01/2014
	; 03/12/2013
	; bell found
u11:
	sti
	cmp	bh, [ACTIVE_PAGE]
	jne	short u12	; Do not sound the beep 
				; if it is not written on the active page
beeper_gfx: ; 04/08/2016
	mov	cx, 1331 	; divisor for 896 hz tone
	mov	bl, 31		; set count for 31/64 second for beep
	;call	beep		; sound the pod bell
	;jmp	short u5 	; tty_return
	;retn
	
TIMER	equ 	040h   		; 8254 TIMER - BASE ADDRESS
PORT_B	equ	061h		; PORT B READ/WRITE DIAGNOSTIC REGISTER
GATE2	equ	00000001b	; TIMER 2 INPUT CATE CLOCK BIT
SPK2	equ	00000010b	; SPEAKER OUTPUT DATA ENABLE BIT

beep:
	; 12/02/2021
	; 07/02/2015
	; 30/08/2014 (Retro UNIX 386 v1)
	; 18/01/2014
	; 03/12/2013
	;
	; TEST4.ASM - 06/10/85  POST AND BIOS UTILITY ROUTINES
	;
	; ROUTINE TO SOUND THE BEEPER USING TIMER 2 FOR TONE
	;
	; ENTRY:
	;    (BL) = DURATION COUNTER ( 1 FOR 1/64 SECOND )
	;    (CX) = FREQUENCY DIVISOR (1193180/FREQUENCY) (1331 FOR 886 HZ)
	; EXIT:				:
	;    (AX),(BL),(CX) MODIFIED.

	pushfd  ; 18/01/2014	; save interrupt status
	cli			; block interrupts during update
	mov	al, 10110110b	; select timer 2, lsb, msb binary
	out	TIMER+3, al 	; write timer mode register
	jmp	$+2		; I/O delay
	mov	al, cl		; divisor for hz (low)
	out	TIMER+2,AL	; write timer 2 count - lsb
	jmp	$+2		; I/O delay
	mov	al, ch		; divisor for hz (high)
	out	TIMER+2, al	; write timer 2 count - msb
	in	al, PORT_B	; get current setting of port
	mov	ah, al		; save that setting
	or	al, GATE2+SPK2	; gate timer 2 and turn speaker on
	out	PORT_B, al	; and restore interrupt status
	; 12/02/2021
	popfd	; 18/01/2014
	;sti
g7:				; 1/64 second per count (bl)
	mov	ecx, 1035	; delay count for 1/64 of a second
	call	waitf		; go to beep delay 1/64 count
	dec	bl		; (bl) length count expired?
	jnz	short g7	; no - continue beeping speaker
	;
	pushfd	; 12/02/2021	; save interrupt status
	cli  	; 18/01/2014	; block interrupts during update
	in	al, PORT_B	; get current port value
        ;or	al, not (GATE2+SPK2) ; isolate current speaker bits in case
        or      al, ~(GATE2+SPK2)
        and	ah, al		; someone turned them off during beep
	mov	al, ah		; recover value of port
        ;or	al, not (GATE2+SPK2) ; force speaker data off
	or 	al, ~(GATE2+SPK2) ; isolate current speaker bits in case
	out	PORT_B, al	; and stop speaker timer
	popfd	; 12/02/2021		; restore interrupt flag state
	;sti
	;mov	ecx, 1035	; force 1/64 second delay (short)
	; 17/04/2021
	mov	cx, 1035
	call	waitf		; minimum delay between all beeps
	pushfd			; save interrupt status
	cli			; block interrupts during update
	in	al, PORT_B	; get current port value in case
	and	al, GATE2+SPK2	; someone turned them on
	or	al, ah		; recover value of port_b
	out	PORT_B, al	; restore speaker status
	popfd			; restore interrupt flag state
u12:
	retn

REFRESH_BIT equ	00010000b 	; REFRESH TEST BIT

WAITF:
waitf:
	; 30/08/2014 (Retro UNIX 386 v1)
	; 03/12/2013
	;
;	push	ax		; save work register (ah)
;waitf1:
				; use timer 1 output bits
;	in	al, PORT_B	; read current counter output status
;	and	al, REFRESH_BIT	; mask for refresh determine bit
;	cmp	al, ah		; did it just change
;	je	short waitf1	; wait for a change in output line
;	;
;	mov	ah, al		; save new lflag state
;	loop	waitf1		; decrement half cycles till count end
;	;
;	pop	ax		; restore (ah)
;	retn			; return (cx)=0

; 06/02/2015 (unix386.s <-- dsectrm2.s)
; 17/12/2014 (dsectrm2.s)
; WAITF
; /// IBM PC-XT Model 286 System BIOS Source Code - Test 4 - 06/10/85 ///
;
;---WAITF-----------------------------------------------------------------------
;	FIXED TIME WAIT ROUTINE (HARDWARE CONTROLLED - NOT PROCESSOR)
; ENTRY:
;	(CX) =	COUNT OF 15.085737 MICROSECOND INTERVALS TO WAIT
;	      	MEMORY REFRESH TIMER 1 OUTPUT USED AS REFERENCE
; EXIT:
;	       	AFTER (CX) TIME COUNT (PLUS OR MINUS 16 MICROSECONDS)
;	(CX) = 0
;-------------------------------------------------------------------------------

; Refresh period: 30 micro seconds (15-80 us)
; (16/12/2014 - AWARDBIOS 1999 - ATORGS.ASM, WAIT_REFRESH)

;WAITF:					; DELAY FOR (CX)*15.085737 US
	;push	AX			; SAVE WORK REGISTER (AH)
	; 11/04/2021
	push	eax
	; 16/12/2014
	;shr	cx, 1			; convert to count of 30 micro seconds
	shr	ecx, 1	; 21/02/2015
;17/12/2014	
;WAITF1:
;	in	al, PORT_B   ;061h	; READ CURRENT COUNTER OUTPUT STATUS
;	and	al, REFRESH_BIT	;00010000b ; MASK FOR REFRESH DETERMINE BIT
;	cmp	al, ah			; DID IT JUST CHANGE
;	je	short WAITF1		; WAIT FOR A CHANGE IN OUTPUT LINE
;	mov	ah, al			; SAVE NEW FLAG STATE
;	loop	WAITF1			; DECREMENT HALF CYCLES TILL COUNT END
	;
	; 17/12/2014
	;
	; Modification from 'WAIT_REFRESH' procedure of AWARD BIOS - 1999
	;
;WAIT_REFRESH:  Uses port 61, bit 4 to have CPU speed independent waiting.
;   	INPUT:  CX = number of refresh periods to wait
;     	       (refresh periods = 1 per 30 microseconds on most machines)
WR_STATE_0:
	in	al, PORT_B		; IN AL,SYS1
	test	al, 010h
	JZ	short WR_STATE_0
WR_STATE_1:
	in	al, PORT_B		; IN AL,SYS1
	test	al, 010h
	jnz	short WR_STATE_1
        loop    WR_STATE_0
	;
	;pop	ax			; RESTORE (AH)
	; 11/04/2021
	pop	eax
	retn				; (CX) = 0

; 03/08/2022
; 02/08/2022 - TRDOS 386 Kernel v2.0.5
; 09/07/2016
; 01/07/2016
; 24/06/2016
; 23/06/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;-------------------------------------------------------------------------------
; WRITE_STRING								       :
;	THIS ROUTINE WRITES A STRING OF CHARACTERS TO THE CRT.		       :
; INPUT 								       :
;	(AL) = WRITE STRING COMMAND  0 - 3				       :
;	(BH) = DISPLAY PAGE (ACTIVE PAGE)				       :
;	(CX) = COUNT OF CHARACTERS TO WRITE, IF (CX) = 0 THEN RETURN	       :
;	(DX) = CURSOR POSITION FOR START OF STRING WRITE		       :
;	(BL) = ATTRIBUTE OF CHARACTER TO WRITE IF (AL) = 0  OR	(AL) = 1       :
;	(EBP) = SOURCE STRING OFFSET					       :
; OUTPUT								       :
;	NONE								       :
;-------------------------------------------------------------------------------

; AL = 00h: Assign all characters the attribute in BL; do not update cursor
; AL = 01h: Assign all characters the attribute in BL; update cursor
; AL = 02h: Use attributes in string; do not update cursor
; AL = 03h: Use attributes in string; update cursor

WRITE_STRING:
	; 03/08/2022
	; 02/08/2022
	; 12/09/2016
	; 09/07/2016
	;cmp	byte [CRT_MODE], 7 ; 6?!
	;ja	VIDEO_RETURN		; not a valid function for VGA modes
	;
	mov	[w_str_cmd], al		; save (AL) command
	cmp	al, 4			; TEST FOR INVALID WRITE STRING OPTION
	;jnb	VIDEO_RETURN		; IF OPTION INVALID THEN RETURN
	; 02/08/2022
	jnb	short P55

        ;jcxz	VIDEO_RETURN		; IF ZERO LENGTH STRING THEN RETURN

        jcxz    P55			; 01/07/2016

	; 01/07/2016
	;and	ecx, 0FFFFh
	; ecx = byte count
	;push	ecx
	mov	esi, ebp ; user buffer
	mov	edi, Cluster_Buffer  ; system buffer
	call	transfer_from_user_buffer
	;pop	ecx
	;jc	VIDEO_RETURN
	; 02/08/2022
	jc	short P55
	; ecx = transfer (byte) count = character count
	mov	ebp, Cluster_Buffer
	; 12/09/2016
	cmp	byte [CRT_MODE], 7 ; 6?!
	;ja	vga_write_string
	; 02/08/2022
	jna	short P57
	jmp	vga_write_string
P57:
	movzx	esi, bh			; GET CURRENT CURSOR PAGE
	;sal	si, 1			; CONVERT TO PAGE OFFSET (SI = PAGE)
	; 02/08/2022
	sal	esi, 1
	; *****
	push	word [esi+CURSOR_POSN]	; SAVE CURRENT CURSOR POSITION IN STACK

	;mov	ax, 0200h		; SET NEW CURSOR POSITION
	;int	10h
P50next:
	push	ecx ; ****
	push	ebx ; *** ; 18/11/2020
	push	esi ; **
	push	edx ; *
	call	_set_cpos
P50:
	mov	al, [ebp]		; GET CHARACTER FROM INPUT STRING
	inc	ebp			; BUMP POINTER TO CHARACTER

;-----	TEST FOR SPECIAL CHARACTER'S

	cmp	al, 08h			; IS IT A BACKSPACE
	je	short P51		; BACK_SPACE
	cmp	al, 0Dh ; CR		; IS IT CARRIAGE RETURN
	je	short P51		; CAR_RET
	cmp	al, 0Ah ; LF		; IS IT A LINE FEED
	je	short P51		; LINE_FEED
	; 18/11/2020
	cmp	al, 09h			; is it a tab stop
	je	short P51
	;
	cmp	al, 07h			; IS IT A BELL
	jne	short P52		; IF NOT THEN DO WRITE CHARACTER
P51:
	;mov	ah, 0Eh			; TTY_CHARACTER_WRITE
	;int	10h			; WRITE TTY CHARACTER TO THE CRT
	
	call	_write_tty_m3

	pop	edx ; *
	pop	esi ; **

	mov	dx, [esi+CURSOR_POSN]	; GET CURRENT CURSOR POSITION
	jmp	short P54		; SET CURSOR POSITION AND CONTINUE
P55:
	jmp	VIDEO_RETURN
P52:
	;mov	cx, 1			; SET CHARACTER WRITE AMOUNT TO ONE
	; 02/08/2022
	sub	ecx, ecx
	inc	cl
	; ecx = 1
	cmp	byte [w_str_cmd], 2	; IS THE ATTRIBUTE IN THE STRING
	jb	short P53		; IF NOT THEN SKIP
	mov	bl, [ebp]		; ELSE GET NEW ATTRIBUTE
	inc	ebp			; BUMP STRING POINTER
P53:
	;mov	ah, 09h			; GOT_CHARACTER
	;int	10h			; WRITE CHARACTER TO THE CRT

	call	_write_c_current
	
	pop	edx ; *	

	; 05/12/2020
	; bx is preserved in '_write_c_current'
	; 18/11/2020
	;mov	ebx, [esp+4]	; ***

	movzx	esi, bh ; video page number (0 to 7)
	mov	[esi+chr_attrib], bl ; color/attribute

	inc	dl			; INCREMENT COLUMN COUNTER
	cmp	dl, [CRT_COLS]		; IF COLS ARE WITHIN RANGE FOR THIS MODE
	;jb	short P54		;    THEN GO TO COLUMNS SET
	jb	short P56 ; 05/12/2020
	inc	dh			; BUMP ROW COUNTER BY ONE
	sub	dl, dl			; SET COLUMN COUNTER TO ZERO
	cmp	dh, 25			; IF ROWS ARE LESS THAN 25 THEN
	;jb	short P54		; GO TO ROWS_COLUMNS_SET
	jb	short P56 ; 05/12/2020

	; 18/11/2020
	;mov	ax, 0E0Ah		; ELSE SCROLL SCREEN
	;int	10h			; RESET ROW COUNTER TO 24

	; 18/11/2020
	mov	al, 0Ah	; line feed

	call	_write_tty_m3
	
	mov	dx, 1800h		; Column = 0, Row = 24
P56:	
	; 05/12/2020
	; 18/11/2020
	pop	esi ; **
P54:					; ROW_COLUMNS_SET
	;mov	ax, 0200h		; SET NEW CURSOR POSITION COMMAND
	;int	10h			; ESTABLISH NEW CURSOR POSITION

	; 18/11/2020
	pop	ebx ; ***
	pop	ecx ; ****

	;loop	P50			; DO IT ONCE MORE UNTIL (CX) = ZERO
	dec	cx
	jnz	short P50next

	pop	dx  ; *****		; RESTORE OLD CURSOR COORDINATES
	
	test	byte [w_str_cmd], 1	; IF CURSOR WAS NOT TO BE MOVED
	;jnz	VIDEO_RETURN		; THEN EXIT WITHOUT RESETTING OLD VALUE
	; 03/08/2022
	jnz	short P58
	
	;mov	ax, 0200h		; ELSE RESTORE OLD CURSOR POSITION
	;int	10h
					; DONE - EXIT WRITE STRING
	call	_set_cpos
P58:
	jmp	VIDEO_RETURN		; RETURN TO CALLER

vga_write_string:
	; 04/08/2022 - TRDOS 386 Kernel v2.0.5
	; 12/09/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', ' biosfn_write_string'

	; INPUT 								  :
	;	(AL) = WRITE STRING COMMAND  0 - 3				  :
	;	(BH) = DISPLAY PAGE (ACTIVE PAGE)				  :
	;	(CX) = COUNT OF CHARACTERS TO WRITE, IF (CX) = 0 THEN RETURN	  :
	;	(DX) = CURSOR POSITION FOR START OF STRING WRITE		  :
	;	(BL) = ATTRIBUTE OF CHARACTER TO WRITE IF (AL) = 0  OR	(AL) = 1  :
	;	(EBP) = SOURCE STRING OFFSET					  :
	; OUTPUT								  :
	;	NONE								  :
	;-------------------------------------------------------------------------;

	; AL = 00h: Assign all characters the attribute in BL; do not update cursor
	; AL = 01h: Assign all characters the attribute in BL; update cursor
	; AL = 02h: Use attributes in string; do not update cursor
	; AL = 03h: Use attributes in string; update cursor
	
	; biosfn_write_string(GET_AL(),GET_BH(),GET_BL(),CX,GET_DH(),GET_DL(),ES,BP);
	; static void biosfn_write_string (flag,page,attr,count,row,col,seg,offset)

	; // Read curs info for the page
 	; biosfn_get_cursor_pos(page,&dummy,&oldcurs);
	; bh = video page = 0
	;movzx	esi, word [CURSOR_POSN] ; current cursor position for video page 0
	
	; // if row=0xff special case : use current cursor position
	; if(row==0xff)
	;  {col=oldcurs&0x00ff;
	;   row=(oldcurs&0xff00)>>8;
	;  }

	;mov	al, [w_str_cmd]

	cmp	dh, 0FFh
	je	short vga_wstr_1 ; user current cursor position
vga_wstr_0:
	; set cursor position
	mov	[CURSOR_POSN], dx ; save cursor pos for pg 0
vga_wstr_1:
	push	word [CURSOR_POSN] ; *

	; ebp = string offset in system buffer (user buffer was copied to)
	
	; while(count--!=0)
	;  {
	;   car=read_byte(seg,offset++);
	;   if((flag&0x02)!=0)
	;    attr=read_byte(seg,offset++);
	;    biosfn_write_teletype(car,page,attr,WITH_ATTR);
	;  }

	;push	eax ; **
	;test	al, 2
	test	byte [w_str_cmd], 2
	jnz	short vga_wstr_3
	mov	[ccolor], bl
vga_wstr_2:
	push	ecx
	mov	al, [ebp]
	call	vga_write_teletype
	pop	ecx
	dec	cx
	jz	short vga_wstr_4
	inc	ebp
	mov	bl, [ccolor]
	jmp	short vga_wstr_2
vga_wstr_3:
	push	ecx
	mov	al, [ebp]
	inc	ebp
	mov	bl, [ebp]
	call	vga_write_teletype
	pop	ecx
	dec	cx
	jz	short vga_wstr_4
	inc	ebp
	jmp	short vga_wstr_3
vga_wstr_4:
	; // Set back curs pos 
	; if((flag&0x01)==0)
	;  biosfn_set_cursor_pos(page,oldcurs);
	; }
	;pop	eax ; **
	pop	dx ; word [CURSOR_POSN] ; *
	;test	al, 1
	test	byte [w_str_cmd], 1
	;jnz	VIDEO_RETURN
	; 04/08/2022
	jnz	short vga_wstr_5
	mov 	[CURSOR_POSN], dx
vga_wstr_5:
	jmp	VIDEO_RETURN

; 03/08/2022 - TRDOS 386 Kernel v2.0.5
; 07/07/2016
; 27/06/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;------------------------------------------------------
;  SCROLL UP
;   THIS ROUTINE SCROLLS UP THE INFORMATION ON THE CRT
; ENTRY ---
;  CH,CL = UPPER LEFT CORNER OF REGION TO SCROLL
;  DH,DL = LOWER RIGHT CORNER OF REGION TO SCROLL
;   BOTH OF THE ABOVE ARE IN CHARACTER POSITIONS
;  BH = FILL VALUE FOR BLANKED LINES
;  AL = # LINES TO SCROLL (AL=0 MEANS BLANK THE ENTIRE FIELD)
;  DS = DATA SEGMENT
;  ES = REGEN SEGMENT
; EXIT --
;  NOTHING, THE SCREEN IS SCROLLED
;--------------------------------------------------------

	; cl = upper left column
	; ch = upper left row
	; dl = lower rigth column
	; dh = lower right row
	;
	; al = line count (AL=0 means blank entire fields)
	; bl = fill value for blanked lines
	; bh = unused

GRAPHICS_UP:
	; 07/07/2016
	; AH = Current video mode, [CRT_MODE]
	cmp	ah, 7
	ja	short vga_graphics_up
	;je	n0

	mov	bh, al			; save line count in BH
	;mov	ax, cx			; GET UPPER LEFT POSITION INTO AX REG
	; 03/08/2022
	mov	eax, ecx

;-----	USE CHARACTER SUBROUTINE FOR POSITIONING
;-----	ADDRESS RETURNED IS MULTIPLIED BY 2 FROM CORRECT VALUE

	call	GRAPH_POSN
	;movzx	edi, ax			; SAVE RESULT AS DESTINATION ADDRESS
	; 03/08/2022
	mov	edi, eax

;-----	DETERMINE SIZE OF WINDOW

	sub	dx, cx
        add     dx, 101h                ; ADJUST VALUES
	sal	dh, 2			; MULTIPLY ROWS BY 4 AT 8 VERT DOTS/CHAR
					; AND EVEN/ODD ROWS
;-----	DETERMINE CRT MODE

	cmp	byte [CRT_MODE], 6	; TEST FOR MEDIUM RES
        jnc	short _R7_              ; FIND_SOURCE

;-----	MEDIUM RES UP
	sal	dl, 1			; # COLUMNS * 2, SINCE 2 BYTES/CHAR
	;sal	di, 1			; OFFSET *2 SINCE 2 BYTES/CHAR
	; 03/08/2022
	sal	edi, 1

;-----	DETERMINE THE SOURCE ADDRESS IN THE BUFFER
_R7_:                                   ; FIND_SOURCE
	add	edi, 0B8000h
	sal	bh, 2			; multiply number of lines by 4
        jz	short _R11              ; IF ZERO, THEN BLANK ENTIRE FIELD
	mov	al, 80			; 80 BYTES/ROW
	mul	bh			; determine offset to source
	;movzx	esi, ax			; offset to source
	; 03/08/2022
	mov	esi, eax
	add	esi, edi		; SET UP SOURCE
	mov	ah, dh			; NUMBER OF ROWS IN FIELD
	sub	ah, bh			; determine number to move

;-----	LOOP THROUGH, MOVING ONE ROW AT A TIME, BOTH EVEN AND ODD FIELDS
_R8:                                    ; ROW_LOOP
        call    _R17                    ; MOVE ONE ROW
	sub	si, 2000h-80		; MOVE TO NEXT ROW
	sub	di, 2000h-80
	dec	ah			; NUMBER OF ROWS TO MOVE
        jnz     short _R8               ; CONTINUE TILL ALL MOVED

;-----	FILL IN THE VACATED LINE(S)
_R9:                                    ; CLEAR ENTRY
	mov	al, bl			; attribute to fill with
_R10_:
        call    _R18                    ; CLEAR THAT ROW
	sub	di, 2000h-80		; POINT TO NEXT LINE
	dec	bh			; number of lines to fill
	jnz	short _R10_             ; CLEAR LOOP
	retn				; EVERYYHING DONE

_R11:                                   ; BLANK_FIELD
	mov	bh, dh			; set blank count to everything in field
        jmp     short _R9               ; CLEAR THE FIELD

vga_graphics_up:
	; 03/08/2022 - TRDOS 386 Kertnel v2.0.5
	; 12/04/2021
	; 08/08/2016
	; 07/08/2016
	; 04/08/2016
	; 01/08/2016
	; 31/07/2016
	; 07/07/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_scroll'
	;

	; cl = upper left column
	; ch = upper left row
	; dl = lower rigth column
	; dh = lower right row
	;
	; al = line count (AL=0 means blank entire fields)
	; bl = fill value for blanked lines
	; bh = unused
	;
	; ah = [CRT_MODE], current video mode

	mov	bh, al ; 31/07/2016
	mov	esi, vga_g_modes
	mov	edi, esi
	add	edi, vga_g_mode_count
vga_g_up_0:
	lodsb
	cmp	al, ah ; [CRT_MODE]
	je	short vga_g_up_1
	cmp	esi, edi
	jb	short vga_g_up_0
	;xor	bh, bh ; 31/07/2016)
	retn	; nothing to do
vga_g_up_1:
	mov	al, bh ; 31/07/2016
	add	esi, vga_g_memmodel - (vga_g_modes + 1)
	; [ESI] = VGA memory model number (LINEAR8, PLANAR4, PLANAR1)

	; if(rlr>=nbrows)rlr=nbrows-1;
 	; if(clr>=nbcols)clr=nbcols-1;
 	; if(nblines>nbrows)nblines=0;
 	; cols=clr-cul+1;

	cmp	dh, [VGA_ROWS]
	jb	short vga_g_up_2
	mov	dh, [VGA_ROWS]
	dec	dh
vga_g_up_2:
	cmp	dl, [CRT_COLS]  ; = [VGA_COLS]
	jb	short vga_g_up_3
	mov	dl, [CRT_COLS]
	dec	dl
vga_g_up_3:
	cmp	al, [VGA_ROWS]
	jna	short vga_g_up_4
	sub	al, al ; 0
vga_g_up_4:
	mov	bh, dl ; clr
	sub	bh, cl ; cul
	inc	bh ; cols = clr-cul+1

	and	al, al ; nblines = 0
	jnz	short vga_g_up_6
	and	ch, ch ; rul = 0
	jnz	short vga_g_up_6
	and	cl, cl ; cul = 0
	jnz	short vga_g_up_6

	;push	ax
	; 12/04/2021
	push	eax
	mov	al, [VGA_ROWS]
	dec	al
	cmp	dh, al ; rlr = nbrows-1
	jne	short vga_g_up_5
        mov     al, [CRT_COLS]  ; = VGA_COLS
	dec	al
 	cmp	dl, al ; clr = nbcols-1
	;jne	short vga_g_up_5
	;;pop	ax
	; 12/04/2021
	pop	eax
	jne	short vga_g_up_5

	mov	ax, 0205h
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	out	dx, ax
	mov	al, [VGA_ROWS]
	mov	ah, [CRT_COLS] ; = [VGA_COLS]
	mul	ah
	movzx	edx, ax
	; 08/08/2016
	movzx	eax, byte [CHAR_HEIGHT]
	mul	edx
	; eax = byte count
	mov	ecx, eax
	;; 07/08/2016
	;shl	dx, 3 ; * 8 ; * [CHAR_HEIGHT]
	;mov	ecx, edx
	mov	al, bl ; fill value for blanked lines
	mov	edi, 0A0000h
	rep	stosb

	;mov	ax, 5
	; 03/08/2022
	xor	ah, ah
	mov	al, 5

	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	out	dx, ax ; 0005h

	retn

vga_g_up_5:
	;;pop	ax
	; 12/04/2021
	;pop	eax

vga_g_up_6:
	; [ESI] = VGA memory model number for current video mode
        ;
        ; LINEAR8 equ 5
        ; PLANAR4 equ 4
        ; PLANAR1 equ 3

	cmp	byte [esi], PLANAR4
	je	short vga_g_up_planar
	cmp	byte [esi], PLANAR1
	je	short vga_g_up_planar
vga_g_up_linear8:
	; 07/07/2016 (TEMPORARY)
	;
	; cl = upper left column ; cul
	; ch = upper left row ; rul
	; dl = lower rigth column ; clr
	; dh = lower right row ; rlr

vga_g_up_l0:
 	;{for(i=rul;i<=rlr;i++)
  	; if((i+nblines>rlr)||(nblines==0))
	or	al, al
	jz	short vga_g_up_l2
	mov	ah, al
	add	ah, ch ; i+nblines
	;jc	short vga_g_up_l2
	cmp	ah, dh
	ja	short vga_g_up_l2
	; else
	;  vgamem_copy_pl4(cul,i+nblines,i,cols,nbcols,cheight);
	call	vgamem_copy_l8
vga_g_up_l1:
	inc	ch
	cmp	ch, dh
	jna	short vga_g_up_l0
	retn
vga_g_up_l2:
	; vgamem_fill_pl4(cul,i,cols,nbcols,cheight,attr);
	call	vgamem_fill_l8
	jmp	short vga_g_up_l1

vga_g_up_planar:
	; cl = upper left column ; cul
	; ch = upper left row ; rul
	; dl = lower rigth column ; clr
	; dh = lower right row ; rlr
vga_g_up_pl0:
 	;{for(i=rul;i<=rlr;i++)
  	; if((i+nblines>rlr)||(nblines==0))
	and	al, al
	jz	short vga_g_up_pl2
	mov	ah, al
	add	ah, ch ; i+nblines
	;jc	short vga_g_up_pl2
	cmp	ah, dh
	ja	short vga_g_up_pl2
	; else
	;  vgamem_copy_pl4(cul,i+nblines,i,cols,nbcols,cheight);
	call	vgamem_copy_pl4
vga_g_up_pl1:
	inc	ch 
	cmp	ch, dh
	jna	short vga_g_up_pl0
	retn
vga_g_up_pl2:
	; vgamem_fill_pl4(cul,i,cols,nbcols,cheight,attr);
	call	vgamem_fill_pl4
	jmp	short vga_g_up_pl1

vgamem_copy_pl4:
	; 08/08/2016
	; 07/08/2016
	; 07/07/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'vgamem_copy_pl4'
	;
	; vgamem_copy_pl4(xstart,ysrc,ydest,cols,nbcols,cheight)
	; cl = xstart, ah = ysrc (i+nblines), ch = ydest (i),
	; bh = cols, [CRT_COLS] = nbcols, [CHAR_HEIGHT] = cheight

	; src=ysrc*cheight*nbcols+xstart;
	; dest=ydest*cheight*nbcols+xstart;

	push	edx
	push	eax

	; outw(VGAREG_GRDC_ADDRESS, 0x0105)
	mov	ax, 0105h
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	out	dx, ax

	; 07/08/2016
 	;mov     ah, [esp+1]
	;movzx	edx, ah ; ysrc
	movzx	edx, byte [esp+1]
	; 08/08/2016
	movzx	eax, byte [CHAR_HEIGHT]
	mov	ah, [CRT_COLS] ; nbcols
	mul	ah 
	;; 07/08/2016
	;movzx	eax, byte [CRT_COLS]
	;shl	ax, 3 ; * 8 ; * [CHAR_HEIGHT]
	push	eax ; cheight * nbcols
	mul	edx ; * ysrc
	; eax = ysrc * cheight * nbcols
	; edx = 0
	mov	dl, cl ; edx = xstart
 	add	eax, edx
	mov	esi, eax ; src
	mov	dl, ch ; ydest
	pop	eax ; cheight * nbcols
	mul	edx
	; eax = ydest * cheight * nbcols
	mov	dl, cl ; edx = xstart
 	add	eax, edx
	mov	edi, eax ; dest
	; esi = src
	; edi = dest
	; for(i=0;i<cheight;i++)
  	; {
   	;  memcpyb(0xa000,dest+i*nbcols,0xa000,src+i*nbcols,cols);
  	; }
	push	ecx
	mov	ecx, 0A0000h
	add	esi, ecx
	add	edi, ecx
	; 08/08/2016
	mov	dh, [CHAR_HEIGHT]
	;; 07/08/2016
	;mov	dh, 8 ; 07/08/2016
	sub	dl, dl ; i
vgamem_copy_pl4_0:
	push	esi
	push	edi
	movzx	eax, byte [CRT_COLS]
	mul	dl
	; eax = i * nbcols
	add	edi, eax ; dest+i*nbcols
	add	esi, eax
	movzx	ecx, bh ; cols
	rep	movsb
	pop	edi
	pop	esi
	dec	dh
	jnz	short vgamem_copy_pl4_0
vgamem_copy_pl4_1:
	pop	ecx

	; outw(VGAREG_GRDC_ADDRESS, 0x0005);
	mov	ax, 0005h
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	out	dx, ax

	pop	eax
	pop	edx

	retn

vgamem_fill_pl4:
	; 08/08/2016
	; 07/08/2016
	; 04/08/2016
	; 07/07/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'vgamem_fill_pl4'
	;
	; vgamem_fill_pl4(xstart,ystart,cols,nbcols,cheight,attr)
	; cl = xstart, edi = ch = ystart, bh = cols,
	; [CRT_COLS] = nbcols, [CHAR_HEIGHT] = cheight, attr = 0

	; dest=ystart*cheight*nbcols+xstart;
	push	edx
	push	eax

	; outw(VGAREG_GRDC_ADDRESS, 0x0205)
	mov	ax, 0205h
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	out	dx, ax

      	; 08/08/2016
	movzx   eax, byte [CHAR_HEIGHT]
	mul	ch
	;; 07/08/2016
	;movzx	eax, ch
	;shl	ax, 3 ; * 8 ; * [CHAR_HEIGHT]
	movzx	edx, byte [CRT_COLS] ; = [VGA_COLS]
	mul	edx
	; edx  = 0
	mov	dl, cl 
	add	eax, edx
	mov	edi, eax
	; edi = dest
	; for(i=0;i<cheight;i++)
  	; {
   	;  memsetb(0xa000,dest+i*nbcols,attr,cols);
  	; }
	add	edi, 0A0000h
	push	ecx
	; 08/08/2016
	mov	dh, [CHAR_HEIGHT]
	;; 07/08/2016
	;mov	dh, 8 ; 07/08/2016
	sub	dl, dl ; i
vgamem_fill_pl4_0:
	push	edi
	movzx	eax, byte [CRT_COLS]
	mul	dl
	; eax = i * nbcols
	add	edi, eax ; dest+i*nbcols
	mov	al, bl ; attr ; 04/08/2016
	movzx	ecx, bh ; cols
 	rep	stosb
	pop	edi
	jnz	short vgamem_fill_pl4_0
vgamem_fill_pl4_1:
	pop	ecx

	; outw(VGAREG_GRDC_ADDRESS, 0x0005);
	mov	ax, 0005h
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	out	dx, ax

	pop	eax
	pop	edx

	retn

vgamem_copy_l8:
	; 02/08/2022 - TRDOS 386 Kernel v2.0.5
	; 08/08/2016
	; 07/08/2016
	; 06/08/2016
	; 07/07/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; TEMPORARY
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'vgamem_copy_pl4'
	;
	; vgamem_copy_pl4(xstart,ysrc,ydest,cols,nbcols,cheight)
	; cl = xstart, ah = ysrc (i+nblines), ch = ydest (i),
	; bh = cols, [CRT_COLS] = nbcols, [CHAR_HEIGHT] = cheight

	; src=ysrc*cheight*nbcols+xstart;
	; dest=ydest*cheight*nbcols+xstart;

	push	edx
	push	eax

	; outw(VGAREG_GRDC_ADDRESS, 0x0105)
	;mov	ax, 0105h
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	;out	dx, ax

	;mov	ah, [esp+1]

	movzx	edx, ah ; ysrc
	; 08/08/2016
	movzx	eax, byte [CHAR_HEIGHT]
	mov	ah, [CRT_COLS] ; nbcols
	mul	ah 
	;; 07/08/2016
	;movzx	eax, byte [CRT_COLS]
	;shl	ax, 3 ; * 8 ; * [CHAR_HEIGHT]
	push	eax ; cheight * nbcols
	mul	edx ; * ysrc
	; eax = ysrc * cheight * nbcols
	; edx = 0
	mov	dl, cl ; edx = xstart
 	add	eax, edx
	mov	esi, eax ; src
	;shl	si, 3 ; * 8 ; 06/08/2016
	; 02/08/2022
	shl	esi, 3
	mov	dl, ch ; ydest
	pop	eax ; cheight * nbcols
	mul	edx
	; eax = ydest * cheight * nbcols
	mov	dl, cl ; edx = xstart
 	add	eax, edx
	mov	edi, eax ; dest
	;shl	di, 3 ; * 8 ; 06/08/2016
	; 02/08/2022
	shl	edi, 3
	; esi = src
	; edi = dest
	; for(i=0;i<cheight;i++)
  	; {
   	;  memcpyb(0xa000,dest+i*nbcols,0xa000,src+i*nbcols,cols);
  	; }
	push	ecx
	mov	ecx, 0A0000h
	add	esi, ecx
	add	edi, ecx
	; 08/08/2016
	mov	dh, [CHAR_HEIGHT]
	;; 07/08/2016
	;mov	dh, 8 ; 07/08/2016
	sub	dl, dl ; i
vgamem_copy_l8_0:
	push	esi
	push	edi
	movzx	eax, byte [CRT_COLS]
	mul	dl
	; eax = i * nbcols
	;shl	ax, 3 ; * 8 ; 06/08/2016
	; 02/08/2022
	shl	eax, 3
	add	edi, eax ; dest+i*nbcols
	add	esi, eax
	movzx	ecx, bh ; cols
	;shl	cx, 3 ; * 8 ; 06/08/2016
	; 02/08/2022
	shl	ecx, 3
	rep	movsb
	pop	edi
	pop	esi
	inc	dl ; 06/08/2016
	dec	dh
	jnz	short vgamem_copy_l8_0
vgamem_copy_l8_1:
	pop	ecx

	;; outw(VGAREG_GRDC_ADDRESS, 0x0005);
	;mov	ax, 0005h
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	;out	dx, ax

	pop	eax
	pop	edx

	retn

vgamem_fill_l8:
	; 02/08/2022 - TRDOS 386 Kernel v2.0.5
	; 08/08/2016
	; 07/08/2016
	; 06/08/2016
	; 04/08/2016
	; 07/07/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; TEMPORARY
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'vgamem_fill_pl4'
	;
	; vgamem_fill_pl4(xstart,ystart,cols,nbcols,cheight,attr)
	; cl = xstart, edi = ch = ystart, bh = cols,
	; [CRT_COLS] = nbcols, [CHAR_HEIGHT] = cheight, attr = 0

	; dest=ystart*cheight*nbcols+xstart;
	push	edx
	push	eax

	;; outw(VGAREG_GRDC_ADDRESS, 0x0205)
	;mov	ax, 0205h
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	;out	dx, ax

        ; 08/08/2016
	movzx   eax, byte [CHAR_HEIGHT]
	mul	ch
	;; 07/08/2016
	;movzx	eax, ch
	;shl	ax, 3 ; * 8 ; * [CHAR_HEIGHT]
	movzx	edx, byte [CRT_COLS] ; = [VGA_COLS]
	mul	edx
	; edx  = 0
	mov	dl, cl 
	add	eax, edx
	mov	edi, eax
	;shl	di, 3 ; * 8 ; 06/08/2016
	; 02/08/2022
	shl	edi, 3
	; edi = dest
	; for(i=0;i<cheight;i++)
  	; {
   	;  memsetb(0xa000,dest+i*nbcols,attr,cols);
  	; }
	add	edi, 0A0000h
	push	ecx
	; 08/08/2016
	mov	dh, [CHAR_HEIGHT]
	;; 07/08/2016
	;mov	dh, 8 ; 07/08/2016
	sub	dl, dl ; i
vgamem_fill_l8_0:
	push	edi
	movzx	eax, byte [CRT_COLS]
	mul	dl
	; eax = i * nbcols
	;shl	ax, 3 ; * 8 ; 06/08/2016
	; 02/08/2022
	shl	eax, 3
	add	edi, eax ; dest+i*nbcols
	mov	al, bl ; attr ; 04/08/2016
	movzx	ecx, bh ; cols
	;shl	cx, 3 ; * 8 ; 06/08/2016
 	; 02/08/2022
	shl	ecx, 3
	rep	stosb
	pop	edi
	inc	dl ; 06/08/2016
	dec	dh
	jnz	short vgamem_fill_l8_0
vgamem_fill_l8_1:
	pop	ecx

	;; outw(VGAREG_GRDC_ADDRESS, 0x0005);
	;mov	ax, 0005h
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	;out	dx, ax

	pop	eax
	pop	edx

	retn

; 03/08/2022 - TRDOS 386 Kernel V2.0.5
; 07/07/2016
; 27/06/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;------------------------------------------------------
; SCROLL DOWN
;  THIS ROUTINE SCROLLS DOWN THE INFORMATION ON THE CRT
; ENTRY --
;  CH,CL = UPPER LEFT CORNER OF REGION TO SCROLL
;  DH,DL = LOWER RIGHT CORNER OF REGION TO SCROLL
;   BOTH OF THE ABOVE ARE IN CHARACTER POSITIONS
;  BH = FILL VALUE FOR BLANKED LINES
;  AL = # LINES TO SCROLL (AL=0 MEANS BLANK THE ENTIRE FIELD)
;  DS = DATA SEGMENT
;  ES = REGEN SEGMENT
; EXIT --
;  NOTHING, THE SCREEN IS SCROLLED
;--------------------------------------------------------

	; cl = upper left column
	; ch = upper left row
	; dl = lower rigth column
	; dh = lower right row
	;
	; al = line count (AL=0 means blank entire fields)
	; bl = fill value for blanked lines
	; bh = unused

GRAPHICS_DOWN:
	; 07/07/2016
	; ah = Current video mode, [CRT_MODE]
	; std				; SET DIRECTION
	cmp	ah, 7
        ja	short vga_graphics_down ; 03/08/2022
	;je	_n0

	mov	bh, al			; save line count in BH
	;mov	ax, dx			; GET LOWER RIGHT POSITION INTO AX REG
	; 03/08/2022
	mov	eax, edx

;-----	USE CHARACTER SUBROUTINE FOR POSITIONING
;-----	ADDRESS RETURNED IS MULTIPLIED BY 2 FROM CORRECT VALUE

	call	GRAPH_POSN
	;movzx	edi, ax			; SAVE RESULT AS DESTINATION ADDRESS
	; 03/08/2022
	mov	edi, eax

;-----	DETERMINE SIZE OF WINDOW

	sub	dx, cx
        add     dx, 101h                ; ADJUST VALUES
	sal	dh, 2			; MULTIPLY ROWS BY 4 AT 8 VERT DOTS/CHAR
					; AND EVEN/ODD ROWS
;-----	DETERMINE CRT MODE

	cmp	byte [CRT_MODE], 6	; TEST FOR MEDIUM RES
	jnc	short _R12              ; FIND_SOURCE_DOWN

;-----	MEDIUM RES DOWN
	sal	dl, 1			; # COLUMNS * 2, SINCE 2 BYTES/CHAR
	;sal	di, 1			; OFFSET *2 SINCE 2 BYTES/CHAR
	; 03/08/2022
	sal	edi, 1
	;inc	di			; POINT TO LAST BYTE
	; 03/08/2022
	inc	edi

;-----	DETERMINE THE SOURCE ADDRESS IN THE BUFFER

_R12:                                   ; FIND_SOURCE_DOWN
	add	edi, 0B8000h		
	add	di, 240			; POINT TO LAST ROW OF PIXELS
	sal	bh, 2			; multiply number of lines by 4
	jz	short 6			; IF ZERO, THEN BLANK ENTIRE FIELD
	mov	al, 80			; 80 BYTES/ROW
	mul	bh			; determine offset to source
	mov	esi, edi		; SET UP SOURCE
	;sub	si, ax			; SUBTRACT THE OFFSET
	; 03/08/2022
	sub	esi, eax
	mov	ah, dh			; NUMBER OF ROWS IN FIELD
	sub	ah, bh			; determine number to move

;-----	LOOP THROUGH, MOVING ONE ROW AT A TIME, BOTH EVEN AND ODD FIELDS

_R13:                                   ; ROW_LOOP_DOWN
        call    _R17                    ; MOVE ONE ROW
	sub	si, 2000h+80		; MOVE TO NEXT ROW
	sub	di, 2000h+80
	dec	ah			; NUMBER OF ROWS TO MOVE
	jnz	short _R13              ; CONTINUE TILL ALL MOVED

;-----	FILL IN THE VACATED LINE(S)
_R14:                                   ; CLEAR_ENTRY_DOWN
	mov	al, bl			; attribute to fill with
_R15_:                                  ; CLEAR_LOOP_DOWN
	call	_R18			; CLEAR A ROW
	sub	di, 2000h+80		; POINT TO NEXT LINE
	dec	bh			; number of lines to fill
        jnz	short _R15_             ; CLEAR_LOOP_DOWN
	; 18/11/2020
	cld				; RESET THE DIRECTION FLAG
	
	retn				; EVERYTHING DONE

_R16:                                   ; BLANK_FIELD_DOWN
	mov	bh, dh			; set blank count to everything in field
        jmp     short _R14              ; CLEAR THE FIELD

vga_graphics_down:
	; 03/08/2022 - TRDOS 386 Kertnel v2.0.5
	; 12/04/2021
	; 08/08/2016
	; 07/08/2016
	; 31/07/2016
	; 07/07/2016 - TRDOS 386 (TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_scroll'
	;

	; cl = upper left column
	; ch = upper left row
	; dl = lower rigth column
	; dh = lower right row
	;
	; al = line count (AL=0 means blank entire fields)
	; bl = fill value for blanked lines
	; bh = unused
	;
	; ah = [CRT_MODE], current video mode

        cld     ; !!! Clear direction flag !!!

	mov	bh, al ; 31/07/2016

	mov	esi, vga_modes
	mov	edi, esi
	add	edi, vga_mode_count
vga_g_down_0:
	lodsb
	cmp	al, ah ; [CRT_MODE]
	je	short vga_g_down_1
	cmp	esi, edi
	jb	short vga_g_down_0
	; xor 	bh, bh	; 31/07/2016
	retn	; nothing to do
vga_g_down_1:
	mov	al, bh ; 31/07/2016
	add	esi, vga_memmodel - (vga_modes + 1)
	; [ESI] = VGA memory model number (LINEAR8, PLANAR4, PLANAR1)

	; if(rlr>=nbrows)rlr=nbrows-1;
 	; if(clr>=nbcols)clr=nbcols-1;
 	; if(nblines>nbrows)nblines=0;
 	; cols=clr-cul+1;

	cmp	dh, [VGA_ROWS]
	jb	short vga_g_down_2
	mov	dh, [VGA_ROWS]
	dec	dh
vga_g_down_2:
	cmp	dl, [CRT_COLS]  ; = [VGA_COLS]
	jb	short vga_g_down_3
	mov	dl, [CRT_COLS]
	dec	dl
vga_g_down_3:
	cmp	al, [VGA_ROWS]
	jna	short vga_g_down_4
	sub	al, al ; 0
vga_g_down_4:
	mov	bh, dh ; clr
	sub	bh, cl ; cul
	inc	bh ; cols = clr-cul+1

	and	al, al ; nblines = 0
	jnz	short vga_g_down_6
	and	ch, ch ; rul = 0
	jnz	short vga_g_down_6
	and	cl, cl ; cul = 0
	jnz	short vga_g_down_6

	push	eax ; push ax ; 12/04/2021
	mov	al, [VGA_ROWS]
	dec	al
	cmp	dh, al ; rlr = nbrows-1
	jne	short vga_g_down_5
        mov     al, [CRT_COLS]  ; = VGA_COLS
	dec	al
 	cmp	dl, al ; clr = nbcols-1
	;jne	short vga_g_down_5
 	; 12/04/2021
	pop	eax ; pop ax
	jne	short vga_g_down_5

	mov	ax, 0205h
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	out	dx, ax
	mov	al, [VGA_ROWS]
	mov	ah, [CRT_COLS] ; = [VGA_COLS]
	mul	ah
	movzx	edx, ax
	; 08/08/2016
	movzx	eax, byte [CHAR_HEIGHT]
	mul	edx
	; eax = byte count
	mov	ecx, eax
	;; 07/08/2016
	;shl	dx, 3 ; * 8 ; * [CHAR_HEIGHT]
	;mov	ecx, edx
	mov	al, bl ; fill value for blanked lines
	mov	edi, 0A0000h
	rep	stosb

	; 03/08/2022
	xor	ah, ah ; 0

	mov	al, 5
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	out	dx, ax ; 0005h

	retn

vga_g_down_5:
	; 12/04/2021
	;pop	eax  ; pop ax

vga_g_down_6:
	; [ESI] = VGA memory model number for current video mode
        ;
        ; LINEAR8 equ 5
        ; PLANAR4 equ 4
        ; PLANAR1 equ 3

	cmp	byte [esi], PLANAR4
	je	short vga_g_down_planar
	cmp	byte [esi], PLANAR1
	je	short vga_g_down_planar
vga_g_down_linear8:
	; 07/07/2016 (TEMPORARY)
	;
	; cl = upper left column ; cul
	; ch = upper left row ; rul
	; dl = lower rigth column ; clr
	; dh = lower right row ; rlr

vga_g_down_l0:
	;{for(i=rlr;i>=rul;i--)
        ; if((i<rul+nblines)||(nblines==0))
	or	al, al
	jz	short vga_g_down_l2
	mov	ah, al
	add	ah, ch
	;jc	short vga_g_down_l2
	xchg	ch, dh
	cmp	ch, ah
	jb	short vga_g_down_l2
	mov	ah, ch
	sub	ah, al ; ah = i - nblines
	; else
	; vgamem_copy_pl4(cul,i,i-nblines,cols,nbcols,cheight);
	call	vgamem_copy_l8
vga_g_down_l1:
	xchg	dh, ch
	dec	dh
	cmp	dh, ch
	jnb	short vga_g_down_l0
	retn

vga_g_down_l2:
	; vgamem_fill_pl4(cul,i,cols,nbcols,cheight,attr);
	call	vgamem_fill_l8
	jmp	short vga_g_down_l1

vga_g_down_planar:
	; cl = upper left column ; cul
	; ch = upper left row ; rul
	; dl = lower rigth column ; clr
	; dh = lower right row ; rlr
vga_g_down_pl0:
 	;{for(i=rlr;i>=rul;i--)
        ; if((i<rul+nblines)||(nblines==0))
	or	al, al
	jz	short vga_g_down_pl2
	mov	ah, al
	add	ah, ch
	;jc	short vga_g_down_pl2
	xchg	ch, dh
	cmp	ch, ah
	jb	short vga_g_down_pl2
	mov	ah, ch
	sub	ah, al ; ah = i - nblines
	; else
	; vgamem_copy_pl4(cul,i,i-nblines,cols,nbcols,cheight);
	call	vgamem_copy_pl4
vga_g_down_pl1:
	xchg	dh, ch
	dec	dh 
	cmp	dh, ch
	jnb	short vga_g_down_pl0
	retn

vga_g_down_pl2:
	; vgamem_fill_pl4(cul,i,cols,nbcols,cheight,attr);
	call	vgamem_fill_pl4
	jmp	short vga_g_down_pl1



; 27/06/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)

;-----	ROUTINE TO MOVE ONE ROW OF INFORMATION

_R17:
	movzx	ecx, dl			; NUMBER OF BYTES IN THE ROW
	push	esi
	push	edi			; SAVE POINTERS
	rep	movsb			; MOVE THE EVEN FIELD
	pop	edi
	pop	esi
	add	si, 2000h
	add	di, 2000h		; POINT TO THE ODD FIELD
	push	esi
	push	edi			; SAVE THE POINTERS
	mov	cl, dl			; COUNT BACK
	rep	movsb			; MOVE THE ODD FIELD
	pop	edi
	pop	esi			; POINTERS BACK
	retn				; RETURN TO CALLER

;-----	CLEAR A SINGLE ROW

_R18:
	movzx	ecx, dl			; NUMBER OF BYTES IN FIELD
	push	edi			; SAVE POINTER
	rep	stosb			; STORE THE NEW VALUE
	pop	edi			; POINTER BACK
	add	di, 2000h		; POINT TO ODD FIELD
	push	edi
	mov	cl, dl
	rep	stosb			; FILL THE ODD FIELD
	pop	edi
	retn				; RETURN TO CALLER

; 03/08/2022 - TRDOS 386 Kernel v2.0.5
; 04/07/2016
; 01/07/2016
; 30/06/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;--------------------------------------------------
; GRAPHICS WRITE
;  THIS ROUTINE WRITES THE ASCII CHARACTER TO THE CURRENT
;  POSITION ON THE SCREEN.
; ENTRY --
;  AL = CHARACTER TO WRITE
;  BL = COLOR ATTRIBUTE TO BE USED FOR FOREGROUND COLOR
;	IF BIT 7 IS SET, THE CHAR IS XOR'D INTO THE REGEN BUFFER
;	(0 IS USED FOR THE BACKGROUND COLOR)
;  CX = NUMBER OF CHARS TO WRITE
;  DS = DATA SEGMENT
;  ES = REGEN SEGMENT
; EXIT --
;  NOTHING IS RETURNED
;
; GRAPHICS READ
;  THIS ROUTINE READS THE ASCII CHARACTER AT THE CURRENT CURSOR
;  POSITION ON THE SCREEN BY MATCHING THE DOTS ON THE SCREEN TO THE
;  CHARACTER GENERATOR CODE POINTS
; ENTRY --
;  NONE (0 IS ASSUMED AS THE BACKGROUND COLOR)
; EXIT --
;  AL = CHARACTER READ AT THAT POSITION (0 RETURNED IF NONE FOUND)
;
; FOR BOTH ROUTINES, THE IMAGES USED TO FORM CHARS ARE CONTAINED IN ROM
;  FOR THE 1ST 128 CHARS.  TO ACCESS CHARS IN THE SECOND HALF, THE USER
;  MUST INITIALIZE THE VECTOR AT INTERRUPT 1FH (LOCATION 0007CH) TO
;  POINT TO THE USER SUPPLIED TABLE OF GRAPHIC IMAGES (8X8 BOXES).
;  FAILURE TO DO SO WILL CAUSE IN STRANGE RESULTS
;-----------------------------------------------------

GRAPHICS_WRITE:
	and 	eax, 0FFh		; ZERO TO HIGH OF CODE POINT
	push	eax			; SAVE CODE POINT VALUE

;-----	DETERMINE POSITION IN REGEN BUFFER TO PUT CODE POINTS

	call	S26			; FIND LOCATION IN REGEN BUFFER
	mov	edi, eax		; REGEN POINTER IN DI

;-----	DETERMINE REGION TO GET CODE POINTS FROM

	pop	eax			; RECOVER CODE POINT

	mov	esi, CRT_CHAR_GEN	; OFFSET OF IMAGES

;-----	DETERMINE GRAPHICS MODE IN OPERATION
					; DETERMINE_MODE
	;sal	ax, 3			; MULTIPLY CODE POINT VALUE BY 8
	; 03/08/2022
	sal	eax, 3
	add	esi, eax		; SI HAS OFFSET OF DESIRED CODES
	
	cmp	byte [CRT_MODE], 6
	jc	short S6		; TEST FOR MEDIUM RESOLUTION MODE

;-----	HIGH RESOLUTION MODE

	add	edi, 0B8000h
S1:					; HIGH_CHAR
	push	edi			; SAVE REGEN POINTER
	push	esi			; SAVE CODE POINTER
	mov	dh, 4			; NUMBER OF TIMES THROUGH LOOP
S2:
	lodsb				; GET BYTE FROM CODE POINTS
	test	bl, 80h			; SHOULD WE USE THE FUNCTION
	jnz	short S5		; TO PUT CHAR IN
	stosb				; STORE IN REGEN BUFFER
	lodsb
S4:
	mov	[edi+2000h-1], al	; STORE IN SECOND HALF
	add	edi, 79			; MOVE TO NEXT ROW IN REGEN
	dec	dh			; DONE WITH LOOP
	jnz	short S2
	pop	esi
	pop	edi			; RECOVER REGEN POINTER
	inc	edi			; POINT TO NEXT CHAR POSITION
	loop	S1			; MORE CHARS TO WRITE
	retn

S5:
	xor	al, [edi]		; EXCLUSIVE OR WITH CURRENT
	stosb				; STORE THE CODE POINT
	lodsb				; AGAIN FOR ODD FIELD
	xor	al, [edi+2000h-1]
	jmp	short S4		; BACK TO MAINSTREAM

;-----	MEDIUM RESOLUTION WRITE
S6:					; MED_RES_WRITE
	mov	dl, bl			; SAVE HIGH COLOR BIT
	; 03/08/2022
	sal	edi, 1
	;sal	di, 1			; OFFSET*2 SINCE 2 BYTES/CHAR
					; EXPAND BL TO FULL WORD OF COLOR
	and	bl, 3			; ISOLATE THE COLOR BITS ( LOW 2 BITS )
	mov	al, 055h		; GET BIT CONVERSION MULTIPLIER
	mul	bl			; EXPAND 2 COLOR BITS TO 4 REPLICATIONS
	mov	bl, al			; PLACE BACK IN WORK REGISTER
	mov	bh, al			; EXPAND TO 8 REPLICATIONS OF COLOR BITS
	add	edi, 0B8000h
S7:                                     ; MED_CHAR
	push	edi			; SAVE REGEN POINTER
	push	esi			; SAVE THE CODE POINTER
	mov	dh, 4			; NUMBER OF LOOPS
S8:
	lodsb				; GET CODE POINT
	call	S21			; DOUBLE UP ALL THE BITS
	and	ax, bx			; CONVERT TO FOREGROUND COLOR ( 0 BACK )
	xchg	ah, al			; SWAP HIGH/LOW BYTES FOR WORD MOVE
	test	dl, 80h			; IS THIS XOR FUNCTION
	jz	short S9		; NO, STORE IT IN AS IS
	xor	ax, [edi]		; DO FUNCTION WITH LOW/HIGH
S9:
	mov	[edi], ax		; STORE FIRST BYTE HIGH, SECOND LOW
	lodsb				; GET CODE POINT
	call	S21
	and	ax, bx			; CONVERT TO COLOR
	xchg	ah, al			; SWAP HIGH/LOW BYTES FOR WORD MOVE
	test	dl, 80h			; AGAIN, IS THIS XOR FUNCTION
	jz	short _S10		; NO, JUST STORE THE VALUES
	xor	ax, [edi+2000h]		; FUNCTION WITH FIRST HALF LOW
_S10:
	mov	[edi+2000h], ax		; STORE SECOND PORTION HIGH
	add	di, 80			; POINT TO NEXT LOCATION
	dec	dh
	jnz	short S8		; KEEP GOING
	pop	esi			; RECOVER CODE POINTER
	pop	edi			; RECOVER REGEN POINTER
	inc	edi			; POINT TO NEXT CHAR POSITION
	inc	edi
	loop	S7			; MORE TO WRITE
	retn

; 03/08/2022 - TRDOS 386 Kernel v2.0.5
; 04/07/2016
; 01/07/2016
; 30/06/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;----------------------------------------
; GRAPHICS READ
;----------------------------------------
GRAPHICS_READ:
	call	S26			; CONVERTED TO OFFSET IN REGEN
	mov	esi, eax		; SAVE IN SI
	add	esi, 0B8000h		; 01/07/2016
	sub	esp, 8			; ALLOCATE SPACE FOR THE READ CODE POINT
	mov	ebp, esp		; POINTER TO SAVE AREA

;-----	DETERMINE GRAPHICS MODES
	mov	dh, 4			; number of passes ; 01/07/2016
	cmp	byte [CRT_MODE], 6
	jc	short S12		; MEDIUM RESOLUTION

;-----	HIGH RESOLUTION READ
;-----	GET VALUES FROM REGEN BUFFER AND CONVERT TO CODE POINT
	;mov	dh,4			; NUMBER OF PASSES
S11:
	mov	al, [esi] 		; GET FIRST BYTE
	mov	[ebp], al 		; SAVE IN STORAGE AREA
	inc	ebp			; NEXT LOCATION
	mov	al, [esi+2000h]		; GET LOWER REGION BYTE
	mov	[ebp], al 		; ADJUST AND STORE
	inc	ebp
	add	esi, 80			; POINTER INTO REGEN
	dec	dh			; LOOP CONTROL
	jnz	short S11		; DO IT SOME MORE
	jmp	short S14		; GO MATCH THE SAVED CODE POINTS

;-----	MEDIUM RESOLUTION READ
S12:
	;sal	si, 1			; OFFSET*2 SINCE 2 BYTES/CHAR
	; 03/08/2022
	sal	esi, 1
	;mov	dh, 4			; NUMBER OF PASSES
S13:
	call	S23			; GET BYTES FROM REGEN INTO SINGLE SAVE
	add	esi, 2000h-2		; GO TO LOWER REGION
	call	S23			; GET THIS PAIR INTO SAVE
	sub	esi, 2000h-80+2		; ADJUST POINTER BACK INTO UPPER
	dec	dh
	jnz	short S13		; KEEP GOING UNTIL ALL 8 DONE

;-----	SAVE AREA HAS CHARACTER IN IT, MATCH IT
S14:					; FIND_CHAR
	mov	edi, CRT_CHAR_GEN	; ESTABLISH ADDRESSING
	sub	ebp, 8			; ADJUST POINTER TO START OF SAVE AREA
	mov	esi, ebp
S15:
	;mov	ax, 256			; NUMBER TO TEST AGAINST
	; 03/08/2022
	sub	eax, eax
	inc	ah
	; eax = 256
S16:
	push	esi			; SAVE SAVE AREA POINTER
	push	edi			; SAVE CODE POINTER
	;mov	ecx, 4			; NUMBER OF WORDS TO MATCH
	;repe	cmpsw			; COMPARE THE 8 BYTES AS WORDS
	cmpsd				; compare first 4 bytes 
	jne	short S17		; 
	cmpsd				; compare last 4 bytes
S17:
	pop	edi			; RECOVER THE POINTERS
	pop	esi
	;jz	short S18		; IF ZERO FLAG SET, THEN MATCH OCCURRED
	je	short S18
	;				; NO MATCH, MOVE ON TO NEXT
	add	edi, 8			; NEXT CODE POINT
	;dec	ax			; LOOP CONTROL
	; 03/08/2022
	dec	eax
	jnz	short S16		; DO ALL OF THEM

;-----	CHARACTER IS FOUND ( AL=0 IF NOT FOUND )
S18:
	add	esp, 8			; READJUST THE STACK, THROW AWAY SAVE
	retn				; ALL DONE

; 12/04/2021
; 30/06/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;--------------------------------------------
; EXPAND BYTE
;  THIS ROUTINE TAKES THE BYTE IN AL AND DOUBLES ALL
;  OF THE BITS, TURNING THE 8 BITS INTO 16 BITS.
;  THE RESULT IS LEFT IN AX
;--------------------------------------------
S21:
	; 03/08/2022
	;push	cx			; SAVE REGISTER
	; 12/04/2021
	push	ecx
	;;mov	cx, 8			; SHIFT COUNT REGISTER FOR ONE BYTE
	;mov	cl, 8
	mov	ah, 8
S22:
	ror	al, 1			; SHIFT BITS, LOW BIT INTO CARRY FLAG
	;rcr	bp, 1			; MOVE CARRY FLAG (LOW BIT INTO RESULTS
	;sar	bp, 1			; SIGN EXTEND HIGH BIT (DOUBLE IT)
	; 03/08/2022
	rcr	cx, 1
	sar	cx, 1

	;;loop	S22			; REPEAT FOR ALL 8 BITS
	;dec	cl
	;jnz	short S22
	;xchg	ax, bp			; MOVE RESULTS TO PARAMETER REGISTER
	; 03/08/5022
	dec	ah
	jnz	short S22
	mov	ax, cx
	;pop	cx			; RECOVER REGISTER
	; 12/04/2021
	pop	ecx
	retn				; ALL DONE

; 01/07/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;--------------------------------------------------
; MED_READ_BYTE
; THIS ROUTINE WILL TAKE 2 BYTES FROM THE REGEN BUFFER,
;  COMPARE AGAINST THE CURRENT FOREGROUND COLOR, AND PLACE
;  THE CORRESPONDING ON/OFF BIT PATTERN INTO THE CURRENT
;  POSITION IN THE SAVE AREA
; ENTRY --
;  SI,DS = POINTER TO REGEN AREA OF INTEREST
;  BX = EXPANDED FOREGROUND COLOR
;  BP = POINTER TO SAVE AREA
; EXIT --
;  SI AND BP ARE INCREMENTED
;----------------------------------------------------
S23:
	lodsw				; GET FIRST BYTE AND SECOND BYTES
	xchg	al, ah			; SWAP FOR COMPARE
	;mov	cx, 0C000h		; 2 BIT MASK TO TEST THE ENTRIES
	; 02/08/2022
	sub	ecx, ecx
	mov	ch, 0C0h
	; ecx = 0C000h
	;mov	dl, 0			; RESULT REGISTER
	; 03/08/2022
	sub	dl, dl
S24:
	;test	ax, cx			; IS THIS SECTION BACKCROUND?
        ; 03/08/2022
	test	eax, ecx
	jz	short S25               ; IF ZERO, IT IS BACKGROUND (CARRY=0)
	stc				; WASN'T, SO SET CARRY
S25:
	rcl	dl, 1			; MOVE THAT BIT INTO THE RESULT
	;shr	cx, 2			; MOVE THE MASK TO THE RIGHT BY 2 BITS
	; 02/08/2022
	shr	ecx, 2
	jnc	short S24		; DO IT AGAIN IF MASK DIDN'T FALL OUT
	mov	[ebp], dl 		; STORE RESULT IN SAVE AREA
	inc	ebp			; ADJUST POINTER
	retn				; ALL DONE

; 02/08/2022 - TRDOS 386 Kernel v2.0.5
; 30/06/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;-----------------------------------------
; V4_POSITION
;  THIS ROUTINE TAKES THE CURSOR POSITION CONTAINED IN
;  THE MEMORY LOCATION, AND CONVERTS IT INTO AN OFFSET
;  INTO THE REGEN BUFFER, ASSUMING ONE BYTE/CHAR.
;  FOR MEDIUM RESOLUTION GRAPHICS, THE NUMBER MUST
;  BE DOUBLED.
; ENTRY -- NO REGISTERS,MEMORY LOCATION @CURSOR_POSN IS USED
; EXIT--
;  AX CONTAINS OFFSET INTO REGEN BUFFER
;-----------------------------------------
S26:
	movzx	eax, word [CURSOR_POSN]	; GET CURRENT CURSOR
GRAPH_POSN:
	push	ebx			; SAVE REGISTER
	movzx	ebx, al			; SAVE A COPY OF CURRENT CURSOR
	mov	al, [CRT_COLS]		; GET BYTES PER COLUMN
	mul	ah			; MULTIPLY BY ROWS
	;shl	ax, 2			; MULTIPLY * 4 SINCE 4 ROWS/BYTE
	; 02/08/2022
	shl	eax, 2
	add	eax, ebx		; DETERMINE OFFSET
	pop	ebx			; RECOVER POINTER
	retn				; ALL DONE

; 03/08/2022 - TRDOS 386 Kernel v2.0.5
; 09/07/2016
; 01/07/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;---------------------------------------------
; SET_COLOR
;	THIS ROUTINE WILL ESTABLISH THE BACKGROUND COLOR, THE OVERSCAN COLOR,
;	AND THE FOREGROUND COLOR SET FOR MEDIUM RESOLUTION GRAPHICS
; INPUT
;	(BH) HAS COLOR ID
;		IF BH=0, THE BACKGROUND COLOR VALUE IS SET
;			FROM THE LOW BITS OF BL (0-31)
;		IF BH=1, THE PALETTE SELECTION IS MADE
;			BASED ON THE LOW BIT OF BL:
;				0 = GREEN, RED, YELLOW FOR COLORS 1,2,3
;				1 = BLUE, CYAN, MAGENTA FOR COLORS 1,2,3
;	(BL) HAS THE COLOR VALUE TO BE USED
; OUTPUT
;	THE COLOR SELECTION IS UPDATED
;----------------------------------------------
SET_COLOR:
        cmp     byte [CRT_MODE], 7      ; 09/07/2016
	;ja	VIDEO_RETURN		; nothing to do for VGA modes
	; 03/08/2022
	jna	short M21
	jmp	VIDEO_RETURN
M21:
	;mov	dx, [ADDR_6845]		; I/O PORT FOR PALETTE
	;mov	dx, 3D4h
	;add	dx, 5			; OVERSCAN PORT
	mov	dx, 3D9h
	mov	al, [CRT_PALETTE] 	; GET THE CURRENT PALETTE VALUE
	or	bh, bh			; IS THIS COLOR 0?
	jnz	short M20		; OUTPUT COLOR 1

;-----	HANDLE COLOR 0 BY SETTING THE BACKGROUND COLOR

	and	al, 0E0h 		; TURN OFF LOW 5 BITS OF CURRENT
	and	bl, 01Fh 		; TURN OFF HIGH 3 BITS OF INPUT VALUE
	or	al, bl			; PUT VALUE INTO REGISTER
M19:					; OUTPUT THE PALETTE
	out	dx, al			; OUTPUT COLOR SELECTION TO 3D9 PORT
	mov	[CRT_PALETTE], al 	; SAVE THE COLOR VALUE
	jmp	VIDEO_RETURN

;-----	HANDLE COLOR 1 BY SELECTING THE PALETTE TO BE USED

M20:
	and	al, 0DFH 		; TURN OFF PALETTE SELECT BIT
	shr	bl, 1			; TEST THE LOW ORDER BIT OF BL
	jnc	short M19		; ALREADY DONE
	or	al, 20H			; TURN ON PALETTE SELECT BIT
	jmp	short M19		; GO DO IT

; 03/08/2022 - TRDOS 386 Kernel v2.0.5
; 09/07/2016
; 01/07/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)
;--------------------------------------------
; READ DOT -- WRITE DOT
; THESE ROUTINES WILL WRITE A DOT, OR READ THE
;  DOT AT THE INDICATED LOCATION
; ENTRY --
;   DX = ROW (0-199)	(THE ACTUAL VALUE DEPENDS ON THE MODE)
;   CX = COLUMN ( 0-639) ( THE VALUES ARE NOT RANGE CHECKED )
;   AL = DOT VALUE TO WRITE (1,2 OR 4 BITS DEPENDING ON MODE,
;	REQUIRED FOR WRITE DOT ONLY, RIGHT JUSTIFIED)
;	BIT 7 OF AL = 1 INDICATES XOR THE VALUE INTO THE LOCATION
;   DS = DATA SEGMENT
;   ES = REGEN SEGMENT
;
; EXIT
;	AL = DOT VALUE READ, RIGHT JUSTIFIED, READ ONLY
;----------------------------------------------

READ_DOT:
	; 09/07/2016
	mov	ah,  [CRT_MODE]
	cmp	ah, 7 ; 6!?
	jna	short read_dot_cga

	call	vga_read_pixel
	; al = pixel value
read_dot_retn:	; 03/08/2022
	jmp	_video_return

read_dot_cga:
	;je	VIDEO_RETURN ; 7
	cmp	ah, 4 ; graphics ?
	;jb	VIDEO_RETURN ; no, text mode, nothing to do
	; 03/08/2022
	jb	short read_dot_retn

	call	R3			; DETERMINE BYTE POSITION OF DOT
	mov	al, [esi]		; GET THE BYTE
	and	al, ah			; MASK OFF THE OTHER BITS IN THE BYTE
	shl	al, cl			; LEFT JUSTIFY THE VALUE
	mov	cl, dh			; GET NUMBER OF BITS IN RESULT
	rol	al, cl			; RIGHT JUSTIFY THE RESULT
	;jmp	VIDEO_RETURN		; RETURN FROM VIDEO I/O
	movzx	eax, al
	jmp	_video_return

; 03/08/2022
; 02/08/2022 - TRDOS 386 Kernel v2.0.5
; 12/04/2021
; 09/07/2016
; 01/07/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)

WRITE_DOT:
	; 09/07/2016
	mov	ah, [CRT_MODE]
	cmp	ah, 7 ; 6!?
	jna	short write_dot_cga
	
	call	vga_write_pixel
write_dot_retn:	; 03/08/2022
	jmp	VIDEO_RETURN

write_dot_cga:
	;je	VIDEO_RETURN ; 7
	cmp	ah, 4 ; graphics ?
	;jb	VIDEO_RETURN ; no, text mode, nothing to do
	; 03/08/2022
	jb	short write_dot_retn

	;;push	ax			; SAVE DOT VALUE
	;push	ax			; TWICE
	; 12/04/2021
	push	eax
	call	R3			; DETERMINE BYTE POSITION OF THE DOT
	shr	al, cl			; SHIFT TO SET UP THE BITS FOR OUTPUT
	and	al, ah			; STRIP OFF THE OTHER BITS
	mov	cl, [esi]		; GET THE CURRENT BYTE
	;pop	bx			; RECOVER XOR FLAG
	; 12/04/2021
	pop	ebx
	test	bl, 80h			; IS IT ON
	jnz	short R2		; YES, XOR THE DOT
	not	ah			; SET MASK TO REMOVE THE INDICATED BITS
	and	cl, ah
	or	al, cl			; OR IN THE NEW VALUE OF THOSE BITS
R1:					; FINISH_DOT
	mov	[esi], al		; RESTORE THE BYTE IN MEMORY
	;;pop	AX
	jmp	VIDEO_RETURN		; RETURN FROM VIDEO I/O
R2:					; XOR_DOT
	xor	al, cl			; EXCLUSIVE OR THE DOTS
	jmp	short R1		; FINISH UP THE WRITING

; 02/08/2022 - TRDOS 386 Kernel v2.0.5
; 01/07/2016 - TRDOS 386 (TRDOS v2.0)
; VIDEO1.ASM - 24/03/1985 (IBM PC-AT BIOS source code)

;----------------------------------------------
; THIS SUBROUTINE DETERMINES THE REGEN BYTE LOCATION OF THE
; INDICATED ROW COLUMN VALUE IN GRAPHICS MODE.
; ENTRY --
;  DX = ROW VALUE (0-199)
;  CX = COLUMN VALUE (0-639)
; EXIT --
;  SI = OFFSET INTO REGEN BUFFER FOR BYTE OF INTEREST
;  AH = MASK TO STRIP OFF THE BITS OF INTEREST
;  CL = BITS TO SHIFT TO RIGHT JUSTIFY THE MASK IN AH
;  DH = # BITS IN RESULT
;  BX = MODIFIED
;-----------------------------------------------
R3:

;-----	DETERMINE 1ST BYTE IN INDICATED ROW BY MULTIPLYING ROW VALUE BY 40
;-----	 ( LOW BIT OF ROW DETERMINES EVEN/ODD, 80 BYTES/ROW )

	movzx	esi, ax			; WILL SAVE AL AND AH DURING OPERATION
	mov	al, 40
	mul	dl			; AX= ADDRESS OF START OF INDICATED ROW
	test	al, 08H 		; TEST FOR EVEN/ODD ROW CALCULATED
	JZ	short R4		; JUMP IF EVEN ROW
	add	ax, 2000h-40		; OFFSET TO LOCATION OF ODD ROWS ADJUST
R4:					; EVEN_ROW
	;xchg	si, ax			; MOVE POINTER TO (SI) AND RECOVER (AX)
	; 02/08/2022
	xchg	esi, eax
	add	esi, 0B8000h
	;mov	dx, cx			; COLUMN VALUE TO DX
	movzx	edx, cx

;-----	DETERMINE GRAPHICS MODE CURRENTLY IN EFFECT

; SET UP THE REGISTERS ACCORDING TO THE MODE
; CH = MASK FOR LOW OF COLUMN ADDRESS ( 7/3 FOR HIGH/MED RES )
; CL = # OF ADDRESS BITS IN COLUMN VALUE ( 3/2 FOR H/M )
; BL = MASK TO SELECT BITS FROM POINTED BYTE ( 80H/C0H FOR H/M )
; BH = NUMBER OF VALID BITS IN POINTED BYTE ( 1/2 FOR H/M )

	mov	bx, 2C0h
	mov	cx, 302h 		; SET PARMS FOR MED RES
	cmp	byte [CRT_MODE], 6
	jc	short R5		; HANDLE IF MED RES
	mov	bx, 180h
	mov	cx, 703h 		; SET PARMS FOR HIGH RES

;-----	DETERMINE BIT OFFSET IN BYTE FROM COLUMN MASK
R5:
	and	ch, dl			; ADDRESS OF PEL WITHIN BYTE TO CH

;-----	DETERMINE BYTE OFFSET FOR THIS LOCATION IN COLUMN

	;shr	dx, cl			; SHIFT BY CORRECT AMOUNT
	;add	si, dx			; INCREMENT THE POINTER
	; 02/08/2022
	shr	edx, cl
	add	esi, edx
	mov	dh, bh			; GET THE # OF BITS IN RESULT TO DH

;-----	MULTIPLY BH (VALID BITS IN BYTE) BY CH (BIT OFFSET)

	sub	cl, cl			; ZERO INTO STORAGE LOCATION
R6:
	ror	al, 1			; LEFT JUSTIFY VALUE IN AL (FOR WRITE)
	add	cl, ch			; ADD IN THE BIT OFFSET VALUE
	dec	bh			; LOOP CONTROL
	jnz	short R6		; ON EXIT, CL HAS COUNT TO RESTORE BITS
	mov	ah, bl			;  GET MASK TO AH
	shr	ah, cl			;  MOVE THE MASK TO CORRECT LOCATION
	retn				;  RETURN WITH EVERYTHING SET UP

load_dac_palette:
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 29/07/2016
	; 23/07/2016
	; 03/07/2016 (TRDOS 386 = TRDOS v2.0)
	; (set_mode_vga)
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'load_dac_palette'
	;
	; Oracle VirtualBox 5.0.24 VGABios Source Code
	; ('vgabios.c', 'vgatables.h', 'vgafonts.h', 'vgarom.asm')
	;
	; INPUT -> AH = DAC selection number (3, 2 or 1)
	; OUTPUT -> ECX = 0, AX = 0
	; (Modifed registers: EAX, ECX, EDX, ESI)
	;
	mov	dx, 3C8h  ; VGAREG_DAC_WRITE_ADDRESS
	sub	al, al ; 0
	out	dx, al ; 0 ; color index, always 0 at the beginning
	;inc	dx   ; 3C9h ; VGAREG_DAC_DATA
	; 02/08/2022
	inc	dl ; dx = 3C9h
	;mov	ecx, 256   ; always 256*3 values
	; 02/08/2022
	xor	ecx, ecx
	inc	ch
	; ecx = 256

	;push	esi
	mov	al, ah
	mov	ah, 3Fh	; 3Fh except DAC selection number 3
	cmp 	al, 2
	je	short l_dac_p_2
	ja	short l_dac_p_3
	and	al, al
	jnz	short l_dac_p_1
l_dac_p_0:
	mov	esi, palette0
	jmp	short l_dac_p_4
l_dac_p_1:
	mov	esi, palette1
	jmp	short l_dac_p_4
l_dac_p_2:
	mov	esi, palette2
	jmp	short l_dac_p_4
l_dac_p_3:
	mov	ah, 0FFh ; dac registers
	mov	esi, palette3
l_dac_p_4:
	lodsb
	out	dx, al  ; Red
	lodsb
	out	dx, al	; Green
	lodsb
	out	dx, al	; Blue
	and	ah, ah
	jz	short l_dac_p_5
	dec	ah
	loop	l_dac_p_4
	;pop	esi
	retn
l_dac_p_5:
	; 29/07/2016
	dec	cl
	jz	short l_dac_p_7
	;
	sub	al, al ; 0
l_dac_p_6:
	out	dx, al ; outb(VGAREG_DAC_DATA,0);
	out	dx, al
	out	dx, al
	loop	l_dac_p_6
l_dac_p_7:
	;pop	esi
	retn

gray_scale_summing:
	; 03/08/2022 (TRDOS 386 v2.0.5)
	; 12/04/2021
	; 03/07/2016 (TRDOS 386 = TRDOS v2.0)
	; (set_mode_vga)
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_perform_gray_scale_summing'
	;
	; Oracle VirtualBox 5.0.24 VGABios Source Code
	; ('vgabios.c', 'vgatables.h', 'vgafonts.h', 'vgarom.asm')
	;

	; INPUT -> EBX = Start address (color index <= 255)
	;	   ECX = Count (<= 256)
	; OUTPUT -> (E)CX = 0
	; (Modifed registers: EAX, ECX, EDX, EBX)

	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	xor	al, al ; 0
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
	out	dx, al	; clear bit 5
			; (while loading palette registers)
	; set read address and switch to read mode
g_s_s_1:
	;mov	dx, 3C7h ; VGAREG_DAC_READ_ADDRESS
	; 03/08/2022
	mov	dl, 0C7h
	mov	al, bl
	out	dx, al
	; get 6-bit wide RGB data values
 	; intensity = (0.3*Red)+(0.59*Green)+(0.11*Blue)
	; i = ( ( 77*r + 151*g + 28*b ) + 0x80 ) >> 8;
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	; 03/08/2022
	mov	dl, 0C9h
	in	al, dx ; red
	mov	ah, 77 ; 0.3* Red
	mul	ah
	;push	ax
	; 12/04/2021
	push	eax
	in	al, dx ; green
	mov	ah, 151  ; 0.59 * Green
	mul	ah
	;push	ax
	; 12/04/2021
	push	eax
	in	al, dx ; blue
	mov	ah, 28 ; 0.11 * Blue
	mul	ah
	;pop	dx
	; 12/04/2021
	pop	edx
	add	ax, dx
	;pop	dx
	; 12/04/2021
	pop	edx
	add	ax, dx
	add	ax, 80h
	mov	al, 3Fh
	cmp	ah, al	; if(i>0x3f)i=0x3f
	jna	short g_s_s_2
	mov	ah, al
g_s_s_2:
	;mov	dx, 3C8h  ; VGAREG_DAC_WRITE_ADDRESS
	; 03/08/2022
	mov	dl, 0C8h
	mov	al, bl ; color index
	out	dx, al
	mov	al, ah ; intensity
	;inc	dx ; 3C9h ; VGAREG_DAC_DATA
	; 03/08/2022
	inc	dl
	out	dx, al ; R (R=G=B)
	mov	al, ah ; intensity
	out	dx, al ; G (R=G=B)
 	mov	al, ah ; intensity
	out	dx, al ; B (R=G=B)
	;dec	cx
	; 03/08/2022
	dec	ecx
	jz	short g_s_s_3
	inc	bl    ; next color index value
	jmp	short g_s_s_1
g_s_s_3:
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	mov	al, 20h
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h 
	out	dx, al ; 20h -> set bit 5
		        ; (after loading palette regs)
	retn

vga_write_char_attr:
vga_write_char_only:
	; 08/07/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_write_char_attr'
	; 'biosfn_write_char_only'

	; INPUT ->
	; [CRT_MODE] = current video mode (>7)
	; CX = Count of characters to write
	; AL = Character to write
	; BL = Color of character
	; OUTPUT ->
	; Regen buffer updated

	mov 	ah, [CRT_MODE]
	mov	dx, [CURSOR_POSN] ; cursor pos for page 0

	mov	esi, vga_modes
	mov	edi, esi
	add	edi, vga_mode_count
vga_wca_0:
	lodsb
	cmp	al, ah ; [CRT_MODE]
	je	short vga_wca_2
	cmp	esi, edi
	jb	short vga_wca_0
vga_wca_1:
	retn	; nothing to do
vga_wca_2:
	add	esi, vga_memmodel - (vga_modes + 1)
	; [ESI] = VGA memory model number (LINEAR8, PLANAR4, PLANAR1)

	; biosfn_write_char_attr (car,page,attr,count)
	; AL = car, page = 0, BL = attr, CX = count
	cmp	byte [esi], PLANAR4
	je	short vga_wca_planar
	cmp	byte [esi], PLANAR1
	je	short vga_wca_planar
vga_wca_linear8:
	; while((count-->0) && (xcurs<nbcols))
	; CX = count
	and	cx, cx
	jz	short vga_wca_1
	cmp	dl, [CRT_COLS]
	jnb	short vga_wca_1
	; write_gfx_char_lin(car,attr,xcurs,ycurs,nbcols);
	; AL = car, BL = attr, DL = xcurs, DH = ycurs,
	; [CRT_COLS] = nbcols
	call	write_gfx_char_lin
	dec	cx ; count
	inc	dl ; xcurs
	jmp	short vga_wca_linear8
vga_wca_planar:
	; while((count-->0) && (xcurs<nbcols))
	; CX = count
	and	cx, cx
	jz	short vga_wca_1
	cmp	dl, [CRT_COLS]
	jnb	short vga_wca_1
	; write_gfx_char_pl4(car,attr,xcurs,ycurs,nbcols,cheight);
	; AL = car, BL = attr, DL = xcurs, DH = ycurs, 
	; [CRT_COLS] = nbcols, [CHAR_HEIGHT] = cheight
	call	write_gfx_char_pl4
	dec	cx ; count
	inc	dl ; xcurs
	jmp	short vga_wca_planar

write_gfx_char_lin:
	; 02/08/2022 (TRDOS 386 v2.0.5)
	; 08/01/2021
	; 05/01/2021 (TRDOS 386 v2.0.3)
	; 08/08/2016
	; 31/07/2016
	; 08/07/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'write_gfx_char_lin'

	; write_gfx_char_lin(car,attr,xcurs,ycurs,nbcols)
	; INPUT ->
	; AL = car, BL = attr, DL = xcurs, DH = ycurs, 
	; [CRT_COLS] = nbcols
	; OUTPUT ->
	; Regen buffer updated

	push	ecx
	push	ebx
	push	edx
	push	eax
	; addr=xcurs*8+ycurs*nbcols*64;
	; 08/08/2016
	movzx	esi, al ; car
	; 08/01/2021
	;movzx	eax, dh ; ycurs
	;mov	ah, [CRT_COLS] ; nbcols
	;mul	ah
	mov	al, [CRT_COLS]
	mul	dh
	;shl	ax, 6 ; * 64
	;shl	ax, 3 ; 8 * ycurs * [CRT_COLS]
	; 02/08/2022
	shl	eax, 3
	;sub	dh, dh
	;shl	dx, 3 ; xcurs * 8
	;movzx	edi, dx
	mov	edi, 0A0000h
	xor	dh, dh
	mov	di, dx
	;movzx	edi, dl
	shl	di, 3 ; xcurs * 8
	;xor	dh, dh
	mov	dl, [CHAR_HEIGHT]
	mul	dx
	; eax = ycurs*nbcols*8*[CHAR_HEIGHT]
	;add	edi, eax ; addr
	;add	edi, 0A0000h
	add	di, ax
	;shl	si, 3 ; car * 8
	xor	ah, ah
	mov	al, [CHAR_HEIGHT]
	mul	si
	mov	si, ax
	;; esi = src = car * 8
	; esi = src = car * [CHAR_HEIGHT]
	; i = 0
	;add	esi, vgafont8 ; fdata [src+i]
	; 08/08/2016
	mov	eax, [VGA_INT43H]
	or	eax, eax ; 0 ?
	jz	short wfxl_7 ; yes, default font
	;cmp	eax, vgafont16
        ;je	short wgfxl_0
	;cmp	eax, vgafont14
	;je	short wgfxl_0
	;cmp	eax, vgafont8
	;je	short wgfxl_0
	;; 05/01/2021 (TRDOS 386 v2.0.3)
	;; user font  
	;mov	eax, VGAFONTUSR ; 8x16 or 8x8 or 8x14 font
	;			; (256 characters)
wgfxl_0:
	add	esi, eax
wgfxl_1:
	sub	bh, bh ; i = 0
wgfxl_2:
	; for(i=0;i<8;i++)
	push	edi ; addr
	movzx	eax, byte [CRT_COLS] ; nbcols
	mul	bh ; nbcols*i
	;shl	ax, 3 ; i*nbcols*8
 	; 02/08/2022
	shl	eax, 3
	; dest=addr+i*nbcols*8;
	add	edi, eax ; dest + j ; j = 0
	mov	cl, 80h ; mask = 0x80;
	; esi = fdata + src + i
	; for(j=0;j<8;j++)
	sub	edx, edx ; j = 0
wgfxl_3:
	mov	al, [esi] ; al = fdata[src+i]
	and	al, cl ; if (fdata[src+i] & mask)
	jz	short wgfxl_4  ; data = 0, zf = 1
	mov	al, bl ; data = attr;
wgfxl_4:
	; write_byte(0xa000,dest+j,data);
	stosb  ; dest + j (+ 0A0000h)
	;inc	dl ; j++
	;cmp	dl, 8
	cmp	dl, 7
	jb	short wgfxl_5
	pop	edi
	; 08/08/2016
	;cmp	bh, 7
	;jnb	short wgfxl_6
	inc	bh ; i++
	cmp	bh, [CHAR_HEIGHT]
	jnb	short wgfxl_6
	inc	esi
	jmp	short wgfxl_2
wgfxl_5:
	shr	cl, 1 ; mask >>= 1;
	inc	dl ; j++
        jmp     short wgfxl_3
wgfxl_6:
	pop	eax
	pop	edx
	pop	ebx
	pop	ecx
	retn
wfxl_7:
	; 08/01/2021
	; 05/01/2021
	; Default font (8x8 or 8x14 or 8x16)
	mov	al, [CHAR_HEIGHT]
	cmp	al, 8
	jne	short wfxl_8
	mov	eax, vgafont8
	jmp	short wgfxl_0
wfxl_8:
	cmp	al, 14
	jne	short wfxl_9
	mov	eax, vgafont14
	jmp	short wgfxl_0
wfxl_9:
	mov	eax, vgafont16
	jmp	short wgfxl_0

write_gfx_char_pl4:
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 08/08/2016
	; 08/07/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'write_gfx_char_pl4'

	; write_gfx_char_pl4(car,attr,xcurs,ycurs,nbcols,cheight)
	; INPUT ->
	; AL = car, BL = attr, DL = xcurs, DH = ycurs,
	; [CRT_COLS] = nbcols, [CHAR_HEIGHT] = cheight
	; OUTPUT ->
	; Regen buffer updated

	push	ecx
	push	ebx
	push	edx
	push	eax
wgfxpl_f0:
	; switch(cheight)
	mov	ah, [CHAR_HEIGHT]
	cmp	ah, 16 ; case 16:
	jne	short wgfxpl_f1
	; fdata = &vgafont16;
	mov	esi, vgafont16
	jmp	short wgfxpl_f3
wgfxpl_f1:
	cmp	ah, 14 ; case 14:
	jne	short wgfxpl_f2
	mov	esi, vgafont14
	jmp	short wgfxpl_f3
wgfxpl_f2:
	; default:
	;  fdata = &vgafont8;
	mov	esi, vgafont8
	mov	ah, 8
wgfxpl_f3:
	; al = car
	mul	ah ; ah = cheight
	and	eax, 0FFFFh ; car * cheight
	; src = car * cheight;
	add	esi, eax ; esi = fdata[src+i]
	; addr=xcurs*8+ycurs*nbcols*64;
	mov	al, dh ; ycurs
	mov	ah, [CRT_COLS] ; nbcols
	mul	ah
	; 08/08/2016
	;shl	ax, 6 ; * 64
	;shl	ax, 3 ; * 8
	; 02/08/2022
	shl	eax, 3
	;sub	dh, dh ; 0
	;shl	dx, 3 ; xcurs * 8
	;movzx	edi, dx
	movzx	edi, dl
	;shl	di, 3 ; xcurs * 8
	; 02/08/2022
	shl	edi, 3
	xor	dh, dh
	mov	dl, [CHAR_HEIGHT]
	mul	dx
	; eax = ycurs*nbcols*8*[CHAR_HEIGHT]
	add	edi, eax ; addr
	add	edi, 0A0000h
	;
	; outw(VGAREG_SEQU_ADDRESS, 0x0f02);
	; outw(VGAREG_GRDC_ADDRESS, 0x0205);
	mov	dx, 3C4h ; VGAREG_SEQU_ADDRESS
	mov	ax, 0F02h
	out	dx, ax
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	mov	ax, 0205h
	out	dx, ax
	;
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	test	bl, 80h ; if(attr&0x80)
	jz	short wgfxpl_f4 ; else
	; outw(VGAREG_GRDC_ADDRESS, 0x1803);
	mov	ax, 1803h
	jmp	short wgfxpl_f5
wgfxpl_f4:
	; outw(VGAREG_GRDC_ADDRESS, 0x0003);
	mov	ax, 0003h
wgfxpl_f5:
	out	dx, ax
	;
	sub	bh, bh ; i = 0
wgfxpl_0:
	; for(i=0;i<cheight;i++)
	push	edi ; addr
	movzx	eax, byte [CRT_COLS] ; nbcols
	mul	bh ; nbcols*i
	; dest=addr+i*nbcols
	add	edi, eax ; dest
	mov	ch, 80h ; mask = 0x80;
	; for(j=0;j<8;j++)
	sub	cl, cl ; j = 0
wgfxpl_1:
	shr	ch, cl ; mask=0x80>>j;
	;
	; outw(VGAREG_GRDC_ADDRESS, (mask << 8) | 0x08);
     	; read_byte(0xa000,dest);
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	mov	ah, ch
	mov	al, 8
	out	dx, ax
	mov	al, [edi] ; ? (io delay?)
	;
	sub	al, al ; attr = 0
	; if (fdata[src+i] & mask)
	test	byte [esi], ch
	jz	short wgfxpl_2  ; zf = 1
	; write_byte(0xa000,dest,attr&0x0f);
	mov	al, bl ; attr;
	and	al, 0Fh	; attr&0x0f
wgfxpl_2:
	; write_byte(0xa000,dest,0x00);
	mov	[edi], al ; dest (+ 0A0000h)
	inc	cl ; j++
	cmp	cl, 8
	jb	short wgfxpl_1
	pop	edi
	; 08/08/2016
	;cmp	bh, 7
	;jnb	short wgfxpl_3
	inc	bh ; i++
	cmp	bh, [CHAR_HEIGHT]
	jnb	short wgfxpl_3
	inc	esi
	jmp	short wgfxpl_0
wgfxpl_3:
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
  	mov	ax, 0FF08h
	out	dx, ax
	mov	ax, 0005h
	out	dx, ax
	mov	ax, 0003h
	out	dx, ax
	;
	pop	eax
	pop	edx
	pop	ebx
	pop	ecx
	retn

vga_write_pixel:
	; 02/08/2022 (TRDOS 386 Kerbel v2.0.5)
	; 09/07/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_write_pixel'

	; INPUT ->
	; 	DX = row (0-239)
	; 	CX = column (0-799)
	; 	AL = pixel value
	;	(AH = [CRT_MODE])
	; OUTPUT ->
	; 	none

	mov	bl, al ; pixel value
	;mov 	ah, [CRT_MODE]
	mov	esi, vga_modes
	mov	edi, esi
	add	edi, vga_mode_count
vga_wp_0:
	lodsb
	cmp	al, ah ; [CRT_MODE]
	je	short vga_wp_1
	cmp	esi, edi
	jb	short vga_wp_0
	retn	; nothing to do
vga_wp_1:
	add	esi, vga_memmodel - (vga_modes + 1)
	; [ESI] = VGA memory model number (LINEAR8, PLANAR4, PLANAR1)
	mov	edi, 0A0000h
	;
	cmp	byte [esi], PLANAR4
	je	short vga_wp_planar
	cmp	byte [esi], PLANAR1
	je	short vga_wp_planar
vga_wp_linear8:
	; addr=CX+DX*(read_word(BIOSMEM_SEG,BIOSMEM_NB_COLS)*8);
	movzx	eax, byte [CRT_COLS] ; = [VGA_COLS] ; nbcols
     	;shl	ax, 3 ; * 8
	; 02/08/2022
	shl	eax, 3
	mul	dx
	push	eax
	;mov	edi, 0A0000h
	add	di, cx
	pop	eax
	add	edi, eax ; addr
	; write_byte(0xa000,addr,AL);
	mov	[edi], bl
	retn
vga_wp_planar:
	; addr = CX/8+DX*read_word(BIOSMEM_SEG,BIOSMEM_NB_COLS);
	movzx	eax, cx
	shr	ax, 3 ; CX/8
	push	eax
	sub	ah, ah ; 0
	mov	al, [CRT_COLS] ; = [VGA_COLS] ; nbcols
	mul	dx
	;mov	edi, 0A0000h
        add     di, ax
	pop	eax
	add	edi, eax ; addr
	and	cl, 7
	mov	ch, 80h ; mask
	shr	ch, cl 	; mask = 0x80 >> (CX & 0x07);
	
	; outw(VGAREG_GRDC_ADDRESS, (mask << 8) | 0x08);
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	mov	ah, ch
	mov	al, 8
	out	dx, ax
	; outw(VGAREG_GRDC_ADDRESS, 0x0205);
	mov	ax, 0205h
	out	dx, ax
	; data = read_byte(0xa000,addr);
	mov	al, [edi] ; (delay?)
	; if (AL & 0x80)
	; {
	;  outw(VGAREG_GRDC_ADDRESS, 0x1803);
	; }
	test	bl, 80h
	jz	short vga_wp_2
	mov	ax, 1803h
	out	dx, ax
vga_wp_2:	
	; write_byte(0xa000,addr,AL);
	mov	[edi], bl
	;
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
  	mov	ax, 0FF08h
	out	dx, ax
	mov	ax, 0005h
	out	dx, ax
	mov	ax, 0003h
	out	dx, ax
	;
	retn

vga_read_pixel: 
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 09/07/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_read_pixel'

	; INPUT ->
	; 	DX = row (0-239)
	; 	CX = column (0-799)
	;	(AH = [CRT_MODE])
	; OUTPUT ->
	; 	AL = pixel value

	;mov 	ah, [CRT_MODE]
	mov	esi, vga_modes
	mov	edi, esi
	add	edi, vga_mode_count
vga_rp_0:
	lodsb
	cmp	al, ah ; [CRT_MODE]
	je	short vga_rp_1
	cmp	esi, edi
	jb	short vga_rp_0
	retn	; nothing to do
vga_rp_1:
	add	esi, vga_memmodel - (vga_modes + 1)  
	; [ESI] = VGA memory model number (LINEAR8, PLANAR4, PLANAR1)
	mov	edi, 0A0000h
	;
	cmp	byte [esi], PLANAR4
	je	short vga_rp_planar
	cmp	byte [esi], PLANAR1
	je	short vga_rp_planar
vga_rp_linear8:
	; addr=CX+DX*(read_word(BIOSMEM_SEG,BIOSMEM_NB_COLS)*8);
	movzx	eax, byte [CRT_COLS] ; = [VGA_COLS] ; nbcols
     	;shl	ax, 3 ; * 8
	; 02/08/2022
	shl	eax, 3
	mul	dx
	push	eax
	;mov	edi, 0A0000h
	add	di, cx
	pop	eax
	add	edi, eax ; addr
	; attr=read_byte(0xa000,addr);
	mov	al, [edi] ; pixel value
	retn
vga_rp_planar:
	; addr = CX/8+DX*read_word(BIOSMEM_SEG,BIOSMEM_NB_COLS);
	movzx	eax, cx
	shr	ax, 3 ; CX/8
	push	eax
	sub	ah, ah ; 0
	mov	al, [CRT_COLS] ; = [VGA_COLS] ; nbcols
	mul	dx
	;mov	edi, 0A0000h
        add     di, ax
	pop	eax
	add	edi, eax ; addr
	and	cl, 7
	mov	ch, 80h ; mask
	shr	ch, cl 	; mask = 0x80 >> (CX & 0x07);
	; attr = 0x00;
	xor	bl, bl ; attr = bl = 0, 
	xor	cl, cl ; i = cl = 0
	; for(i=0;i<4;i++)
      	; {
       	;  outw(VGAREG_GRDC_ADDRESS, (i << 8) | 0x04);
       	;  data = read_byte(0xa000,addr) & mask;
       	;  if (data > 0) attr |= (0x01 << i);
      	; }
vga_rp_2:
	mov	ah, cl ; i << 8
	mov	al, 4  ; | 0x04
	mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	out	dx, ax
	; data = read_byte(0xa000,addr) & mask;
	mov	al, [edi]
	and	al, ch ; & mask
	; if (data > 0) attr |= (0x01 << i);
	or	al, al
	jz	short vga_rp_3 ; al = 0 
	mov	bh, 1
	shl	bh, cl ; (0x01 << i)
	or	bl, bh ; attr |= (0x01 << i)
	mov	al, bl ; pixel value
vga_rp_3:	
	retn

vga_beeper:
	; 04/08/2016  (TRDOS 386 = TRDOS v2.0)
	sti
	;mov	bh, [ACTIVE_PAGE]
        jmp     beeper_gfx

vga_write_teletype:
	; 03/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 12/04/2021 (TRDOS 386 v2.0.3, 32 bit push/pop)
	; 09/12/2017
	; 06/08/2016
	; 04/08/2016
	; 01/08/2016
	; 31/07/2016
	; 09/07/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_write_teletype'
	; 'biosfn_write_char_only'

	; INPUT ->
	; [CRT_MODE] = current video mode (>7)
	; AL = Character to write
	; BL = Color of character
	; OUTPUT ->
	; Regen buffer updated

	; biosfn_write_teletype (car, page, attr, flag) 
	; car = character (AL)
	; page = 0
	; attr = color (BL)
	; 'flag' not used

	mov 	ah, [CRT_MODE]
	mov	bh, al ; character
	mov	dx, [CURSOR_POSN] ; cursor pos for page 0

	mov	esi, vga_g_modes
	mov	edi, esi
	add	edi, vga_g_mode_count
vga_wtty_0:
	lodsb
	cmp	al, ah ; [CRT_MODE]
	je	short vga_wtty_2
	cmp	esi, edi
	jb	short vga_wtty_0
vga_wtty_1:
	retn	; nothing to do
vga_wtty_2:
	cmp	bh, 07h ; bell (beep)
	je	short vga_beeper  ; u11
	cmp	bh, 08h ; backspace
	jne	short vga_wtty_3
	; if(xcurs>0)xcurs--;
	or	dl, dl ; xcurs (column)
	jz	short vga_wtty_1
	dec	dl ; xcurs--;
        jmp     short vga_wtty_12
vga_wtty_3:			
	cmp	bh, 0Dh ; carriage return (\r)
	jne	short vga_wtty_4
	; xcurs=0;
	sub	dl, dl ; 0
        jmp     short vga_wtty_12
vga_wtty_4:	
	cmp	bh, 0Ah ; new line (\n)
	jne	short vga_wtty_5
	; ycurs++;
	inc	dh ; next row
        jmp     short vga_wtty_11
vga_wtty_5:
	cmp 	bh, 09h ; tab stop
	jne	short vga_wtty_8
	mov	al, dl
	;cbw
	xor	ah, ah ; 09/12/2017
	mov	cl, 8
	div	cl
	sub	cl, ah
	;
	mov	bh, 20h ; space
vga_wtty_6: ; tab stop loop
	;push	cx
	;push	bx
	; 12/04/2021
	push	ecx
	push	ebx
	call	vga_wtty_8
	;pop	bx  ; bh = character, bl = color
	;pop	cx
	; 12/04/2021
	pop	ebx  ; bh = character, bl = color
	pop	ecx
	dec	cl
	jz	short vga_wtty_7
	mov	dx, [CURSOR_POSN] ; new cursor position (pg 0)
	jmp	short vga_wtty_6
vga_wtty_7:
	retn
	;
vga_wtty_8:
	add	esi, vga_g_memmodel - (vga_g_modes + 1)
	; [ESI] = VGA memory model number (LINEAR8, PLANAR4, PLANAR1)
	mov	edi, 0A0000h
	;
	mov	al, bh ; character
	;
	cmp	byte [esi], PLANAR4
	je	short vga_wtty_planar
	cmp	byte [esi], PLANAR1
	je	short vga_wtty_planar
vga_wtty_linear8:
	; write_gfx_char_lin(car,attr,xcurs,ycurs,nbcols);
	; AL = car, BL = attr (color), DL = xcurs, DH = ycurs,
	; [CRT_COLS] = nbcols
	call	write_gfx_char_lin
	jmp	short vga_wtty_9

vga_wtty_12:
	; 09/07/2016
	; set cursor position
	; NOTE: Hardware cursor position will not be set
	;   in any VGA modes (>7)
	;   But, cursor position will be saved into
	;   [CURSOR_POSN].
	;   TRDOS 386 (TRDOS v2.0) uses only one page
	;   (page 0) for all graphics modes.

	mov	[CURSOR_POSN], dx ; save cursor pos for pg 0
	; 04/08/2016
	;mov	bh, [ACTIVE_PAGE] ; = 0
	;call	_set_cpos
	retn

vga_wtty_planar:
	; write_gfx_char_pl4(car,attr,xcurs,ycurs,nbcols,cheight);
	; AL = car, BL = attr (color), DL = xcurs, DH = ycurs, 
	; [CRT_COLS]= nbcols, [CHAR_HEIGHT] = cheight
	call	write_gfx_char_pl4
vga_wtty_9:
	inc	dl ; xcurs++;
vga_wtty_10:
	; Do we need to wrap ?
	; if(xcurs==nbcols)
	cmp	dl, [CRT_COLS] ; [VGA_COLS]
	jb	short vga_wtty_11 ; no
	sub	dl, dl ; xcurs=0;
	inc	dh ;  ycurs++;
vga_wtty_11:
	; Do we need to scroll ?
	; if(ycurs==nbrows)
	cmp	dh, [VGA_ROWS]
	jb	short vga_wtty_12 ; no
	;
	; biosfn_scroll (nblines,attr,rul,cul,rlr,clr,page,dir)
	; al = nblines = 1, bl = attr (color) = 0
	; ch = rul, cl = cul, dh = rlr, dl = clr, page = 0
	; dir = SCROLL_UP

	mov	al, 1
	sub	bl, bl ; 0 ; blank/black line (attr=0) will be used
	;sub	cx, cx ; 0,0
	; 03/08/2022
	sub	ecx, ecx

	; 06/08/2016
	mov	dh, [VGA_ROWS]
	dec	dh ; nbrows -1

	;push	dx ; 04/08/2016
	; 12/04/2021
	push	edx
	mov	dl, [CRT_COLS]
	dec	dl ; nbcols -1

	mov	ah, [CRT_MODE]

	; biosfn_scroll(0x01,0x00,0,0,nbrows-1,nbcols-1,page,SCROLL_UP);
	call	vga_graphics_up
	; 04/08/2016
	;pop	dx
	; 12/04/2021
	pop	edx

	;dec	dh ; ycurs-=1
	jmp	short vga_wtty_12

font_setup:
	; 03/08/2022 (TRDOS 386 v2.0.5)
	; 09/01/2021 (TRDOS 386 v2.0.3)
	; 09/07/2016
	; character generator (font loading) functions
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'int10_func'

	; AX = 1100H ; Load User-Defined Font (EGA/VGA)
	;
        ; BH =  height of each character (bytes per character definition)
        ; (BL = font block to load (EGA: 0-3; VGA: 0-7))
	; CX =  number of characters to redefine (<=256)
        ; DX =  ASCII code of the first character defined at ES:BP
        ; EBP =	address of font-definition information
	;	(in user's memory space)

	; case 0x11:
     	; switch(GET_AL())
      	; {
	; case 0x00:
        ; case 0x10:
        ; biosfn_load_text_user_pat(GET_AL(),ES,BP,CX,DX,GET_BL(),GET_BH());
        ; break;

	; AX = 1110H ; Load and Activate User-Defined Font (EGA/VGA)
	or	al, al ; 0
	jz	short font_setup_0
	cmp	al, 10h
	jne	short font_setup_1
font_setup_0:
	call	transfer_user_fonts
	jc	short font_setup_error
	call	load_text_user_pat
        jmp     VIDEO_RETURN 
font_setup_1:
	; AX = 1101H ; Load ROM 8x14 Character Set (EGA/VGA)
	; case 0x01:
        ; case 0x11:
        ; biosfn_load_text_8_14_pat(GET_AL(),GET_BL());
        ; break;
	cmp	al, 1
	je	short font_setup_2
	cmp	al, 11h
	jne	short font_setup_3
font_setup_2:
	; AX = 1111H ; Load and Activate ROM 8x14 Character Set (EGA/VGA)
	; (BL = font block to load (EGA: 0-3; VGA: 0-7))
	call	load_text_8_14_pat
        jmp     VIDEO_RETURN
font_setup_error:
	sub	eax, eax ; 0 -> fonts could not be loaded
	jmp	_video_return
font_setup_3:
	; AX = 1102H ; Load ROM 8x8 Character Set (EGA/VGA)
	; case 0x02:
        ; case 0x12:
        ; biosfn_load_text_8_8_pat(GET_AL(),GET_BL());
        ; break;
	cmp	al, 2
	je	short font_setup_4
	cmp	al, 12h
	jne	short font_setup_5
font_setup_4:
	; AX = 1112H ; Load and Activate ROM 8x8 Character Set (EGA/VGA)
	; (BL = font block to load (EGA: 0-3; VGA: 0-7))
	call	load_text_8_8_pat
        jmp     VIDEO_RETURN
font_setup_5:
	; AX = 1104H ; Load ROM 8x16 Character Set (EGA/VGA)
	; case 0x04:
        ; case 0x14:
        ; biosfn_load_text_8_16_pat(GET_AL(),GET_BL());
        ; break;
	cmp	al, 4
	je	short font_setup_6
	cmp	al, 14h
	jne	short font_setup_7
font_setup_6:
	; AX = 1114H ; Load and Activate ROM 8x16 Character Set (EGA/VGA)
	; (BL = font block to load (EGA: 0-3; VGA: 0-7))
	call	load_text_8_16_pat
        jmp     VIDEO_RETURN
font_setup_7:
	; Note: AX=1120h (Setup INT 1Fh, EXT_PTR) is not needed
	; for TRDOS 386 (TRDOS v2.0) video functionality;
	; because, originally EXT_PTR (font address) was used for
	; chars 80h to 0FFh (after the first 128 ASCII char fonts), for
	; CGA graphics mode; currenty, 'vgafont8' address has 256 chars!
	; 
	; case 0x20:
        ; biosfn_load_gfx_8_8_chars(ES,BP);
        ; break; 
	; case 0x21:
        ; biosfn_load_gfx_user_chars(ES,BP,CX,GET_BL(),GET_DL());
        ; break;
	; AX = 1121H ; Setup User-Defined Font for Graphics Mode (VGA)
	; BL   screen rows code: 00H = user-specified (in DL)
        ;                        01H = 14 rows
        ;                        02H = 25 rows
        ;                        03H = 43 rows
        ; CX   bytes per character definition
        ; DL   (when BL=0) custom number of character rows on screen
        ; EBP  address of font-definition information (user's mem space)

	cmp	al, 21h
	jne	short font_setup_9

	; TRDOS 386 modification !
	; dh = 0 -> 256 characters
	; dh = 80h -> second 128 characters
	; dh = 0FFh -> first 128 characters

	; 09/01/2021 (TRDOS 386 v2.0.3)
	;push	ebx
	push	ecx
	push	edx
	xor	dl, dl
	mov	bh, cl ; character height
	;mov	cx, 100h ; 256
	; 03/08/2022
	xor 	ecx, ecx
	inc	ch
	; ecx = 100h
	or	dh, dh ; 0
	jz	short  font_setup_8
	dec	ch ; cx = 0
	cmp	dh, 0FFh
	je	short font_setup_8 ; 1st 128 chars
	; 2nd 128 chars
	cmp	dh, 80h
	jne	short font_setup_error ; invalid !
	mov	cl, dh
	xchg	dl, dh
	; number of chars, cx = 80h
	; start char, dl = 80h
font_setup_8:
	call	transfer_user_fonts
	pop	edx
	pop	ecx
	;pop	ebx
	jc	short font_setup_error
	; ebp = user's font data address in system's memory space
	call	load_gfx_user_chars
        jmp     VIDEO_RETURN 
font_setup_9:
	; case 0x22:
        ; biosfn_load_gfx_8_14_chars(GET_BL());
        ; break;
	cmp	al, 22h
	jne	short font_setup_10
	call	load_gfx_8_14_chars
        jmp     VIDEO_RETURN 
font_setup_10:
	; case 0x23:
        ; biosfn_load_gfx_8_8_dd_chars(GET_BL());
        ; break;
	cmp	al, 23h
	jne	short font_setup_11
	call	load_gfx_8_8_chars
        jmp     VIDEO_RETURN 
font_setup_11:
	; case 0x24:
        ; biosfn_load_gfx_8_16_chars(GET_BL());
        ; break;
	cmp	al, 24h
	jne	short font_setup_12
	call	load_gfx_8_16_chars
        jmp     VIDEO_RETURN 
font_setup_12:
 	; case 0x30:
        ; biosfn_get_font_info(GET_BH(),&ES,&BP,&CX,&DX);
        ; break;
	cmp	al, 30h
	jne	short font_setup_13
	call	get_font_info
	; eax = return value (info: 4 bytes for 4 parms)
	; eax = 0 -> invalid function (input)
        jmp     _video_return
font_setup_13:
	cmp	al, 03h ; AX = 1103h
	jne	short font_setup_14
	; biosfn_set_text_block_specifier:
	; BL = font block selector code	
	; NOTE: TRDOS 386 only uses and sets font block 0
	; (It is as BL = 0 for TRDOS 386)
	mov	dx, 3C4h ; VGAREG_SEQU_ADDRESS
	;mov	ah, bl
	sub	ah, ah ; 0
	;mov	al, 03h
	out	dx, ax
	jmp     VIDEO_RETURN 

font_setup_14:
	sub	eax, eax ; 0 = invalid function
        jmp     _video_return

transfer_user_fonts:
	; 02/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 19/01/2021
	; 09/01/2021
	; 05/01/2021 (TRDOS 386 v2.0.3)

	; BH =  height of each character (bytes per character)
	; CX =  number of characters to redefine (<=256)
        ; DX =  ASCII code of the first character defined at EBP
        ; EBP =	address of font-definition information
	;	(in user's memory space)

	; Modified registers: eax, edx, ecx, esi, edi, ebp
	;
	; output:
	;	ebp = user font data address in system memory

	and	edx, 0FFFFh
	and	ecx, 0FFFFh
	jnz	short transfer_user_fonts_5

	or	edx, edx
	jnz	short transfer_user_fonts_4
	or	ebp, ebp
	jnz	short transfer_user_fonts_4

	; cx = 0, dx = 0, ebp = 0
	; copy system font to user font

	mov	cl, 64 ; 64 dwords

	cmp	bh, 16
	je	short transfer_user_fonts_2
	cmp	bh, 8
	je	short transfer_user_fonts_1

	mov	ebp, vgafont14
	retn

transfer_user_fonts_1:
	mov	edi, VGAFONT8USER
	mov	esi, vgafont8
	jmp	short transfer_user_fonts_3

transfer_user_fonts_2:
	mov	edi, VGAFONT16USER
	mov	esi, vgafont16
transfer_user_fonts_3:
	mov	ebp, edi
	rep	movsd
	retn

transfer_user_fonts_4:
	stc
	retn
	
transfer_user_fonts_5:
	or	ebp, ebp
	jz	short transfer_user_fonts_4 ; invalid address !

	cmp	cx, 256
	ja	short transfer_user_fonts_4
	sub	ecx, edx
	jna	short transfer_user_fonts_4

	cmp	bh, 14 ; 8x14 font
		       ; (there is not an alternative buffer)
	jne	short transfer_user_fonts_6

	; use system's 8x14 font space if permission flag is 1
	test	byte [ufont], 80h
	jz	short transfer_user_fonts_4 ; not allowed

	; permission is given (for vgafont14 location etc.)
	; (for permanent font modification)
	;
	; 19/01/2021
	; Note: Permission flag can be set by 'root' while
	;	system is not in multi tasking/user mode
	;	while [multi_tasking] = 0 and [u.uid] = 0

	;push	edx
	; 02/08/2022
	xchg	edx, ecx
	;xor	ah, ah
	; 02/08/2022
	xor	eax, eax
	mov	al, bh ; mov al, 14
	;mul	cx 
	; 02/08/2022
	mul	edx  ; char count * 14
	; 02/08/2022
	mov	edx, ecx ; ascii code
	mov	ecx, eax
		; ecx = byte count
	;pop	edx
	; 02/08/2022
	;xor	eax, eax
	xor	ah, ah
	mov	al, bh ; mov ax, 14 ; bytes per character
	;mul	dx
	;mov	dx, ax ; char offset
	; 02/08/2022
	mul	edx
	mov	edx, eax ; char offset
	mov	edi, vgafont14
	jmp	short transfer_user_fonts_8
transfer_user_fonts_6:
	cmp	bh, 8 ; 8x8 font
	jne	short transfer_user_fonts_7 ; 8x16 font
	;shl	dx, 3 ; * 8
	;shl	cx, 3 ; * 8
	; 02/08/2022
	shl	edx, 3 ; byte offset
	shl	ecx, 3 ; byte count
	; 09/01/2021
	mov	edi, VGAFONT8USER
	test	byte [ufont], 8  ; already loaded ?
	jnz	short transfer_user_fonts_8 ; yes
	mov	esi, vgafont8
	call	transfer_user_fonts_10
	jmp	short transfer_user_fonts_8
transfer_user_fonts_7:
	cmp	bh, 16 ; 8x16 font
	jne	short transfer_user_fonts_4  ; invalid !
	;shl	dx, 4 ; * 16
	;shl	cx, 4 ; * 16
	; 02/08/2022
	shl	edx, 4 ; byte offset
	shl	ecx, 4 ; byte count
 	mov	edi, VGAFONT16USER
	test	byte [ufont], 16  ; already loaded ?
	jnz	short transfer_user_fonts_8 ; yes
	mov	esi, vgafont16
	call	transfer_user_fonts_10
transfer_user_fonts_8:
	add	edi, edx ; char offset
	; 09/07/2016
	;and	ecx, 0FFFFh
	; ECX = byte count
	;push	ecx
	mov	esi, ebp ; user's font buffer
	; 09/01/2021
	mov	ebp, edi ; system addr for user's font
	; 05/01/2021
	;mov	edi, Cluster_Buffer  ; system buffer
	call	transfer_from_user_buffer
	;pop	ecx
	; ecx = transfer (byte) count = character count
	jc	short transfer_user_fonts_9
	; 05/01/2021
	;mov	ebp, Cluster_Buffer

	or	byte [ufont], bh 
			; 8x8 or 8x16 user font ready 
transfer_user_fonts_9:
	retn

transfer_user_fonts_10:
	; 02/08/2022
	; 09/01/2021
	push	esi
	push	edi
	push	ecx
	;mov	cx, 64
	; 02/08/2022
	xor	ecx, ecx
	mov	cl, 64
	rep	movsd
	pop	ecx
	pop	edi
	pop	esi
	retn

load_text_user_pat:
	; 02/08/2022 (TRDOS 3386 v2.0.5)
	; 26/07/2016
	; 09/07/2016
	; load user defined (EGA/VGA) text fonts
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_load_text_user_pat'

	; biosfn_load_text_user_pat (AL,ES,BP,CX,DX,BL,BH)

	; get_font_access();
	; blockaddr = ((BL & 0x03) << 14) + ((BL & 0x04) << 11);
	; for(i=0;i<CX;i++)
	; {
	;  src = BP + i * BH;
	;  dest = blockaddr + (DX + i) * 32;
	;  memcpyb(0xA000, dest, ES, src, BH);
	; }
	; release_font_access();
	; if(AL>=0x10)
	; {
	; set_scan_lines(BH);
	; }

	push	eax
	call	get_font_access
	sub	bl, bl ; i = 0
	; 02/08/2022
	; ecx <= 256
	xor	ch, ch
ltup_1:
	mov	al, bl
	mul	bh
	;movzx	esi, ax
	; 02/08/2022
	mov	esi, eax
	add	esi, ebp
	mov	al, bl
	sub	ah, ah
	;add	ax, dx ; (DX + i)
	;shl	ax, 5  ; * 32
	; 02/08/2022
	add	eax, edx
	shl	eax, 5
	;movzx	edi, ax
	; 02/08/2022
	mov	edi, eax
	add	edi, 0A0000h
	push	ecx
	;movzx	ecx, bh
	; 02/08/2022
	mov	cl, bh
	rep	movsb
	pop	ecx
	inc	bl
	cmp	bl, cl
	jne	short ltup_1
	;
	call	release_font_access
	;
	pop	eax
	; if(AL>=0x10)
	cmp	al, 10h
	jb	short ltup_2
   	; set_scan_lines(BH);
	call	set_scan_lines
ltup_2:
	retn

get_font_access:
	; 02/08/2022
	; 09/07/2016
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'get_font_access'

	; get_font_access()
	push	edx
	mov	dx, 3C4h ; VGAREG_SEQU_ADDRESS
	; 02/08/2022
 	xor	eax, eax
	;mov	ax, 0100h
	mov	ah, 1
	; ax = 0100h
	out	dx, ax
	mov	ax, 0402h
	out	dx, ax
	mov	ax, 0704h
	out	dx, ax
	mov	ax, 0300h
	out	dx, ax
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	; 02/08/2022
	mov	dl, 0CEh
	mov	ax, 0204h
	out	dx, ax
	mov	ax, 0005h
	out	dx, ax
	mov	ax, 0406h
	out	dx, ax
	pop	edx
	retn

release_font_access:
	; 02/08/2022
	; 29/07/2016
	; 09/07/2016
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'release_font_access'

	mov	dx, 3C4h ; VGAREG_SEQU_ADDRESS
	;mov	ax, 0100h
	; 02/08/2022
	sub	eax, eax
	mov	ah, 1
	; ax = 0100h
	out	dx, ax
	mov	ax, 0302h
	out	dx, ax
	mov	ax, 0304h
	out	dx, ax
	mov	ax, 0300h
	out	dx, ax
	;mov	dx, 3CCh ; VGAREG_READ_MISC_OUTPUT
 	; 02/08/2022
	mov	dl, 0CCh
	in	al, dx
	and	al, 01h
	shl	al, 2
	or	al, 0Ah
	mov	ah, al
	mov	al, 06h
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	; 02/08/2022
	mov	dl, 0CEh
	out	dx, ax
	mov	ax, 0004h
	out	dx, ax
	mov	ax, 1005h
	out	dx, ax
	retn

set_scan_lines:
	; 02/08/2022
	; 09/07/2016
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'set_scan_lines'

	; set_scan_lines(lines)
	; BH = lines

	; outb(crtc_addr, 0x09);
	mov	dx, 3D4h ; CRTC_ADDRESS = 3D4h (always)
	mov	al, 09h
	out	dx, al
	; crtc_r9 = inb(crtc_addr+1);
	;inc	dx ; 3D5h
	; 02/08/2022
	inc	dl
	in	al, dx
	; crtc_r9 = (crtc_r9 & 0xe0) | (lines - 1);
	and	al, 0E0h
	dec	bh ; lines - 1
	or	al, bh
	; outb(crtc_addr+1, crtc_r9);
	out	dx, al
	;inc	bh 
	; if(lines==8)
	;cmp	bh, 8
	cmp	bh, 7
	jne	short ssl_1
	; biosfn_set_cursor_shape(0x06,0x07);
	mov	cx, 0607h
	jmp	short ssl_2
ssl_1:
	; biosfn_set_cursor_shape(lines-4,lines-3);
	mov	cl, bh ; lines - 1
	mov	ch, cl ; lines - 1 (16 -> 15)
	dec	ch  ; lines - 2 (16 -> 14)
ssl_2:
	; CH = start line, CL = stop line
	mov	ah, 10	; 6845 register for cursor set
	mov	[CURSOR_MODE], cx ; save in data area
	call	m16	; output cx register
	; write_word(BIOSMEM_SEG,BIOSMEM_CHAR_HEIGHT, lines);
	inc	bh ; lines
	mov	[CHAR_HEIGHT], bh
	;  outb(crtc_addr, 0x12);
	mov	dx, 3D4h ; CRTC_ADDRESS
	mov	al, 12h
	out	dx, al
	; vde = inb(crtc_addr+1);
	;inc	dx
	; 02/08/2022
	inc	dl
	in	al, dx
	mov	ah, al
	; outb(crtc_addr, 0x07);
	;dec	dx
	; 02/08/2022
	dec	dl
	mov	al, 07h
	out	dx, al
	; ovl = inb(crtc_addr+1);
	;inc	dx
	; 02/08/2022
	inc	dl
	in	al, dx
	; vde += (((ovl & 0x02) << 7) + ((ovl & 0x40) << 3) + 1);
	mov	dl, ah ; vde
	mov	dh, al ; ovl
	;and	ax, 02h
	;shl	ax, 7
	; 02/08/2022
	xor	eax, eax
	mov	al, dh
	and	al, 2
	shl	eax, 7
	;mov	cx, ax ; (ovl & 0x02) << 7)
	; 02/08/2022
	mov	ecx, eax
	sub	ah, ah
	mov	al, dh ; ovl
	;and	ax, 40h
	;shl	ax, 3  ; (ovl & 0x40) << 3)
	;inc	ax ; + 1 
	;add	ax, cx
	; 02/08/2022
	and	al, 40h
	shl	eax, 3
	inc	eax
	add	eax, ecx
	xor	dh, dh
	;add	ax, dx ; + vde
	; 02/08/2022
	add	eax, edx
	; rows = vde / lines;
	div	bh
	;dec	al ; rows -1
	; write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, rows-1);
	mov	[VGA_ROWS], al ; rows (not 'rows-1' !)
	; write_word(BIOSMEM_SEG,BIOSMEM_PAGE_SIZE, rows * cols * 2);
	;mov	ah, [CRT_COLS]
	;mul	ah
	; 17/11/2020
	mul	byte [CRT_COLS]
	;shl	ax, 1
	; 02/08/2022
	shl	eax, 1
	mov 	[CRT_LEN], ax
	retn

load_text_8_14_pat:
	; 02/08/2022 (TRDOS 3386 v2.0.5)
	; 26/07/2016
	; 25/07/2016
	; 23/07/2016
	; 09/07/2016
	; load user defined (EGA/VGA) text fonts
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_load_text_8_14_pat'

	; biosfn_load_text_8_14_pat (AL,BL)

	; get_font_access();
	; blockaddr = ((BL & 0x03) << 14) + ((BL & 0x04) << 11);
	; for(i=0;i<0x100;i++)
	; {
	;  src = i * 14;
	;  dest = blockaddr + i * 32;
	;  memcpyb(0xA000, dest, 0xC000, vgafont14+src, 14);
	; }
	; release_font_access();
	; if(AL>=0x10)
	; {
	; set_scan_lines(14);
	; }

	push	eax
	call	get_font_access

	; blockaddr = ((BL & 0x03) << 14) + ((BL & 0x04) << 11);
	;mov	dl, bl
	;and	dl, 3
	;shl	dx, 14
	;xchg	dx, bx
	;and	dl, 4
	;shl	dx, 11
	;add	dx, bx

	;xor	dx, dx  ; blockaddr = 0
	; Always block 0 for TRDOS 386 ! (blockaddr=0)

	sub	bl, bl ; i = 0
	mov	bh, 14
	mov	esi, vgafont14
	mov	edi, 0A0000h
	; 02/08/2022
	sub	ecx, ecx
lt8_14_1:
	;mov	al, bl
	;mul	bh
	;movzx	esi, ax
	;add	esi, vgafont14
	;mov	al, bl
	;sub	ah, ah
	;shl	ax, 5 ; * 32
	;;add	ax, dx ; blockaddr + i * 32;
	;movzx	edi, ax ; dest
	;add	edi, 0A0000h
	;02/08/2022
	;movzx	ecx, bh
	mov	cl, bh
	rep	movsb
	add	edi, 18 ; 32 - 14
	inc	bl
	jnz	short lt8_14_1	
	;
	call	release_font_access
	;
	pop	eax
	; if(AL>=0x10)
	cmp	al, 10h
	jb	short lt8_14_4
	; BH = 14
   	; set_scan_lines(14);
	call	set_scan_lines
lt8_14_4:
	retn

load_text_8_8_pat:
	; 02/08/2022 (TRDOS 3386 v2.0.5)
	; 05/01/2021 (TRDOS 386 v2.0.3)
	; 26/07/2016
	; 25/07/2016
	; 23/07/2016
	; 09/07/2016
	; load user defined (EGA/VGA) text fonts
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_load_text_8_8_pat'

	; biosfn_load_text_8_8_pat (AL,BL)

	; get_font_access();
	; blockaddr = ((BL & 0x03) << 14) + ((BL & 0x04) << 11);
	; for(i=0;i<0x100;i++)
	; {
	;  src = i * 8;
	;  dest = blockaddr + i * 32;
	;  memcpyb(0xA000, dest, 0xC000, vgafont8+src, 8);
	; }
	; release_font_access();
	; if(AL>=0x10)
	; {
	; set_scan_lines(8);
	; }

	push	eax
	call	get_font_access

	; blockaddr = ((BL & 0x03) << 14) + ((BL & 0x04) << 11);
	;mov	dl, bl
	;and	dl, 3
	;shl	dx, 14
	;xchg	dx, bx
	;and	dl, 4
	;shl	dx, 11
	;add	dx, bx

	;xor	dx, dx  ; blockaddr = 0
	; Always block 0 for TRDOS 386 ! (blockaddr=0)

	sub	bl, bl ; i = 0
	mov	bh, 8
	;mov	esi, vgafont8
	mov	edi, 0A0000h

	; 02/08/2022
	sub	ecx, ecx

	; 05/01/2021
	test	byte [ufont], 80h
	jz	short lt8_8_0
		; user font permission (after set mode)
 	test	byte [ufont], 8
	jz	short lt8_8_0
	mov	esi, VGAFONT8USER
	jmp	short lt8_8_1
lt8_8_0:
	mov	esi, vgafont8
lt8_8_1:
	;mov	al, bl
	;mul	bh
	;movzx	esi, ax
	;add	esi, vgafont8
	;mov	al, bl
	;sub	ah, ah
	;shl	ax, 5 ; * 32
	;;add	ax, dx ; blockaddr + i * 32;
	;movzx	edi, ax ; dest
	;add	edi, 0A0000h
	; 02/08/2022
	;movzx	ecx, bh
	mov	cl, bh
	rep	movsb
	add	edi, 24 ; 32 - 8
	inc	bl
	jnz	short lt8_8_1
	;
	call	release_font_access
	;
	pop	eax
	; if(AL>=0x10)
	cmp	al, 10h
	jb	short lt8_8_2
	; BH = 8
   	; set_scan_lines(8);
	call	set_scan_lines
lt8_8_2:
	retn

load_text_8_16_pat:
	; 02/08/2022 (TRDOS 3386 v2.0.5)
	; 05/01/2021 (TRDOS 386 v2.0.3)
	; 26/07/2016
	; 25/07/2016
	; 23/07/2016
	; 09/07/2016
	; load user defined (EGA/VGA) text fonts
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_load_text_8_16_pat'

	; biosfn_load_text_8_16_pat (AL,BL)

	; get_font_access();
	; blockaddr = ((BL & 0x03) << 14) + ((BL & 0x04) << 11);
	; for(i=0;i<0x100;i++)
	; {
	;  src = i * 16;
	;  dest = blockaddr + i * 32;
	;  memcpyb(0xA000, dest, 0xC000, vgafont16+src, 16);
	; }
	; release_font_access();
	; if(AL>=0x10)
	; {
	; set_scan_lines(16);
	; }

	push	eax
	call	get_font_access

	; blockaddr = ((BL & 0x03) << 14) + ((BL & 0x04) << 11);
	;mov	dl, bl
	;and	dl, 3
	;shl	dx, 14
	;xchg	dx, bx
	;and	dl, 4
	;shl	dx, 11
	;add	dx, bx

	;xor	dx, dx  ; blockaddr = 0
	; Always block 0 for TRDOS 386 ! (blockaddr=0)

	sub	bl, bl ; i = 0
	mov	bh, 16
	;mov	esi, vgafont16
	mov	edi, 0A0000h
	;movzx	eax, bh
	; 02/08/2022
	sub	eax, eax
	mov	al, bh

	; 05/01/2021
	test	byte [ufont], 80h
	jz	short lt8_16_0
		; user font permission (after set mode)
 	test	byte [ufont], 16
	jz	short lt8_16_0
	mov	esi, VGAFONT16USER
	jmp	short lt8_16_1
lt8_16_0:
	mov	esi, vgafont16
lt8_16_1:
	;mov	al, bl
	;mul	bh
	;movzx	esi, ax
	;add	esi, vgafont16
	;mov	al, bl ; i
	;sub	ah, ah
	;shl	ax, 5 ; * 32
	;;add	ax, dx ; blockaddr + i * 32;
	;movzx	edi, ax ; dest
	;add	edi, 0A0000h
	;movzx	ecx, bh
	mov	ecx, eax ; 16
	rep	movsb
	add	edi, eax ; add edi, 16
	inc	bl
	jnz	short lt8_16_1
	;
	call	release_font_access
	;
	pop	eax
	; if(AL>=0x10)
	cmp	al, 10h
	jb	short lt8_16_2
	; BH = 16
   	; set_scan_lines(16);
	call	set_scan_lines
lt8_16_2:
	retn

load_gfx_user_chars:
	; 08/01/2021
	; 05/01/2021 (TRDOS 386 v2.0.3)
	; 08/08/2016
	; 10/07/2016
	; Setup User-Defined Font for Graphics Mode (VGA)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_load_gfx_user_chars'

	; biosfn_load_gfx_user_chars (ES,BP,CX,BL,DL)
	; /* set 0x43 INT pointer */
    	; write_word(0x0, 0x43*4, BP);
    	; write_word(0x0, 0x43*4+2, ES);

	; 08/01/2021

	; BL   screen rows code: 00H = user-specified (in DL)
        ;                        01H = 14 rows
        ;                        02H = 25 rows
        ;                        03H = 43 rows
        ; CX   bytes per character definition
        ; DL   (when BL=0) custom number of character rows on screen
        ; EBP  address of font-definition information (user's mem space)

	; 05/01/2021
	;xor	eax, eax
	;dec	eax ; 0FFFFFFFFh (user defined fonts)
	;mov	[VGA_INT43H], eax

	; 08/01/2021
	; ebp = video font data (buffer) address
	mov	[VGA_INT43H], ebp

	; switch (BL) {
	; case 0:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, DL-1);
	;    break;
	and	bl, bl
	jnz	short l_gfx_uc_1
	mov	[VGA_ROWS], dl  ; not DL-1 !
	jmp	short l_gfx_uc_4
l_gfx_uc_1:
	; case 1:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 13);
	;    break;
	dec	bl
	jnz	short l_gfx_uc_2
	; bl = 1
	mov	byte [VGA_ROWS], 14  ; not 13 !
	jmp	short l_gfx_uc_4
l_gfx_uc_2:
	dec	bl
	jz	short l_gfx_uc_3 ; bl = 2
	dec	bl
	jnz	short l_gfx_uc_4 ; bl > 3
	; bl = 3
	; case 3:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 42);
	;    break;
	mov	byte [VGA_ROWS], 43  ; not 42 !
l_gfx_uc_3:
    	; case 2:
    	; default:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 24);
	;    break;
	; bl = 2 or bl > 3
	mov	byte [VGA_ROWS], 25  ; not 24 !
    	; }
l_gfx_uc_4:
    	; write_byte(BIOSMEM_SEG, BIOSMEM_CHAR_HEIGHT, CX);
	mov	[CHAR_HEIGHT], cl
	; }
	retn

load_gfx_8_14_chars:
	; 08/08/2016
	; 10/07/2016
	; Setup ROM 8x14 Font for Graphics Mode (VGA)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_load_gfx_8_14_chars'

	; biosfn_load_gfx_8_14_chars (BL)
	; /* set 0x43 INT pointer */
    	; write_word(0x0, 0x43*4, &vgafont14);
    	; write_word(0x0, 0x43*4+2, 0xC000);
	mov	dword [VGA_INT43H], vgafont14

	; BL    screen rows code: 00H = user-specified (in DL)
        ;                         01H = 14 rows
        ;                         02H = 25 rows
        ;                         03H = 43 rows
        ; DL    (when BL=0) custom number of char rows on screen

	; switch (BL) {
	; case 0:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, DL-1);
	;    break;
	and	bl, bl
	jnz	short l_gfx_8_14c_1
	mov	[VGA_ROWS], dl  ; not DL-1 !
	jmp	short l_gfx_8_14c_4
l_gfx_8_14c_1:
	; case 1:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 13);
	;    break;
	dec	bl
	jnz	short l_gfx_8_14c_2
	; bl = 1
	mov	byte [VGA_ROWS], 14  ; not 13 !
	jmp	short l_gfx_8_14c_4
l_gfx_8_14c_2:
	dec	bl
	jz	short l_gfx_8_14c_3 ; bl = 2
	dec	bl
	jnz	short l_gfx_8_14c_4 ; bl > 3
	; bl = 3
	; case 3:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 42);
	;    break;
	mov	byte [VGA_ROWS], 43  ; not 42 !
l_gfx_8_14c_3:
    	; case 2:
    	; default:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 24);
	;    break;
	; bl = 2 or bl > 3
	mov	byte [VGA_ROWS], 25  ; not 24 !
    	; }
l_gfx_8_14c_4:
    	; write_byte(BIOSMEM_SEG, BIOSMEM_CHAR_HEIGHT, 14);
        mov     byte [CHAR_HEIGHT], 14
	; }
	retn

load_gfx_8_8_chars:
	; 08/08/2016
	; 10/07/2016
	; Setup ROM 8x14 Font for Graphics Mode (VGA)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_load_gfx_8_8_dd_chars'

	; biosfn_load_gfx_8_8_dd_chars (BL)
	; /* set 0x43 INT pointer */
    	; write_word(0x0, 0x43*4, &vgafont8);
    	; write_word(0x0, 0x43*4+2, 0xC000);
	mov	dword [VGA_INT43H], vgafont8
		
	; BL    screen rows code: 00H = user-specified (in DL)
        ;                         01H = 14 rows
        ;                         02H = 25 rows
        ;                         03H = 43 rows
        ; DL    (when BL=0) custom number of char rows on screen

	; switch (BL) {
	; case 0:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, DL-1);
	;    break;
	and	bl, bl
	jnz	short l_gfx_8_8c_1
	mov	[VGA_ROWS], dl  ; not DL-1 !
	jmp	short l_gfx_8_8c_4
l_gfx_8_8c_1:
	; case 1:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 13);
	;    break;
	dec	bl
	jnz	short l_gfx_8_8c_2
	; bl = 1
	mov	byte [VGA_ROWS], 14  ; not 13 !
	jmp	short l_gfx_8_8c_4
l_gfx_8_8c_2:
	dec	bl
	jz	short l_gfx_8_8c_3 ; bl = 2
	dec	bl
	jnz	short l_gfx_8_8c_4 ; bl > 3
	; bl = 3
	; case 3:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 42);
	;    break;
	mov	byte [VGA_ROWS], 43  ; not 42 !
l_gfx_8_8c_3:
    	; case 2:
    	; default:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 24);
	;    break;
	; bl = 2 or bl > 3
	mov	byte [VGA_ROWS], 25  ; not 24 !
    	; }
l_gfx_8_8c_4:
    	; write_byte(BIOSMEM_SEG, BIOSMEM_CHAR_HEIGHT, 8);
        mov     byte [CHAR_HEIGHT], 8
	; }
	retn

load_gfx_8_16_chars:
	; 08/08/2016
	; 10/07/2016
	; Setup ROM 8x14 Font for Graphics Mode (VGA)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_load_gfx_8_16_chars'

	; biosfn_load_gfx_8_16_chars (BL)
	; /* set 0x43 INT pointer */
    	; write_word(0x0, 0x43*4, &vgafont16);
    	; write_word(0x0, 0x43*4+2, 0xC000);
	mov	dword [VGA_INT43H], vgafont16
		
	; BL    screen rows code: 00H = user-specified (in DL)
        ;                         01H = 14 rows
        ;                         02H = 25 rows
        ;                         03H = 43 rows
        ; DL    (when BL=0) custom number of char rows on screen

	; switch (BL) {
	; case 0:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, DL-1);
	;    break;
	and	bl, bl
	jnz	short l_gfx_8_16c_1
	mov	[VGA_ROWS], dl  ; not DL-1 !
	jmp	short l_gfx_8_16c_4
l_gfx_8_16c_1:
	; case 1:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 13);
	;    break;
	dec	bl
	jnz	short l_gfx_8_16c_2
	; bl = 1
	mov	byte [VGA_ROWS], 14  ; not 13 !
	jmp	short l_gfx_8_16c_4
l_gfx_8_16c_2:
	dec	bl
	jz	short l_gfx_8_16c_3 ; bl = 2
	dec	bl
	jnz	short l_gfx_8_16c_4 ; bl > 3
	; bl = 3
	; case 3:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 42);
	;    break;
	mov	byte [VGA_ROWS], 43  ; not 42 !
l_gfx_8_16c_3:
    	; case 2:
    	; default:
	;    write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, 24);
	;    break;
	; bl = 2 or bl > 3
	mov	byte [VGA_ROWS], 25  ; not 24 !
    	; }
l_gfx_8_16c_4:
    	; write_byte(BIOSMEM_SEG, BIOSMEM_CHAR_HEIGHT, 16);
        mov     byte [CHAR_HEIGHT], 16
	; }
	retn

get_font_info:
	; 03/08/2022 (TRDOS 386 v2.0.5)
	; 08/01/2021 (TRDOS 386 v2.0.3)
	; 19/09/2016
	; 08/08/2016
	; 10/07/2016
	; Get Current Character Generator Info (VGA)
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'biosfn_get_font_info'

	; Modified for TRDOS 386 !
	;
	; INPUT ->
	;    AX = 1130h
	;    BL = 0 -> Get info for current VGA font
	;	       (BH = unused)
	;    19/09/2016
	;    BL > 0 -> Get requested character font data
	;	 BL = 1 -> vgafont8
	;        BL = 2 -> vgafont14
	;	 BL = 3 -> vgafont16
	;	;08/01/2021
	;	 BL = 4 -> user defined 8x8 font
	;	 BL = 5 -> user defined 8x14 font
	;	 BL = 6 -> user defined 8x16 font
	;	 BL > 6 -> Invalid function (for now!)
	;	 BH = ASCII code of the first character
	;	 ECX = Number of characters from the 1st char
	;	 ECX >= 256 -> All (256-BH) characters
	;	 ECX = 0 -> All characters (BH = unused)
	;        EDX = User's Buffer Address
	; OUTPUT ->
	;    AL = height (scanlines), bytes per character
	;    AH = screen rows
	;    Byte 16-23 of EAX = number of columns
	;    Byte 24-31 of EAX = 
	;	  0 -> default font (not configured yet)
	;	  0FFh -> user defined font
	;	  14 = vgafont14
	;	   8 = vgafont8
	;	  16 = vgafont16
	;   If BL input > 0 -> 
	;       EAX = Actual transfer count
	;
	and	bl, bl
	jz	short gfi_1
	; invalid function (input)
	; 08/01/2021
	cmp	bl, 4
	jb	short gfi_5
	je	short gfi_3
	cmp	bl, 6
	je	short gfi_4
	; bh = 5 or bh > 6
gfi_0:
	xor	eax, eax ; 0
	retn
gfi_1:
	mov	al, [CHAR_HEIGHT]
	mov	ah, [VGA_ROWS]
	shl	eax, 16
	mov	al, [CRT_COLS]
	mov	ecx, [VGA_INT43H]
	and	ecx, ecx
	jz	short gfi_2 ; 0 = default font
	; 08/01/2021
	dec	ah ; 0FFh
	cmp	ecx, VGAFONT16USER
	je	short gfi_2
	cmp	ecx, VGAFONT8USER
	je	short gfi_2	
	mov	ah, [CHAR_HEIGHT] ; font size = height
gfi_2:
	rol	eax, 16
	retn
gfi_3:
	; 08/01/2021
	test	byte [ufont], 08h ; 8x8 user font
	jz	short gfi_0 ; not loaded !
	mov	esi, VGAFONT8USER ; *
	;mov	bl, 8
	;jmp	short gfi_8
	jmp	short gfi_10
gfi_4:
	; 08/01/2021
	test	byte [ufont], 10h ; 8x16 user font
	jz	short gfi_0 ; not loaded !
	mov	esi, VGAFONT16USER ; *
	jmp	short gfi_7
gfi_5:
	cmp	bl, 2
	jb	short gfi_9
	ja	short gfi_6
	; BL = 2 -> vgafont14
	mov	esi, vgafont14 ; *
	mov	bl, 14
	jmp	short gfi_8 
gfi_6:
	; BL = 3 -> vgafont16
	mov	esi, vgafont16 ; *
gfi_7:
	mov	bl, 16
gfi_8:
	mov	edi, edx ; **
	or	ecx, ecx
	jz	short gfi_11 ; all chars from the 00h
	;mov	al, bh ; character index
	; 03/08/2022
	movzx	eax, bh
	mul	bl ; char index * char height/size
	;movzx	edx, ax
	; 03/08/2022
	;add	esi, edx ; *
	add	esi, eax
	xor	edx, edx
	dec	dl 
	; edx = 0FFh = 255
	;mov	dx, 255
	sub	dl, bh
	;inc	dx
	; 03/08/2022
	inc	edx
	cmp	ecx, edx
	ja	short gfi_11
	je	short gfi_12
	mov	ecx, edx
	jmp	short gfi_12
gfi_9:
	;BL = 1 -> vgafont8
	mov	esi, vgafont8 ; *
gfi_10:
	mov	bl, 8
	jmp	short gfi_8
gfi_11:
	;mov	ecx, 256
	; 03/08/2022
	sub	ecx, ecx
	inc	ch
	; ecx = 100h 
gfi_12:
	; 08/01/2021
	mov	eax, ecx ; character count
	xor	bh, bh
	mul	bx ; char count * char height/size
 	mov	ecx, eax

	; ESI = source address in system space
	; EDI = user's buffer address
	; ECX = transfer (byte) count
	call	transfer_to_user_buffer
	mov	eax, ecx ; actual transfer count
	retn

set_all_palette_reg:
	; 03/08/2022
	; 10/08/2016
	; Set All Palette Registers and Overscan
	; EDX = Address of 17 bytes; 
	; an rgbRGB value for each of 16 palette
	; registers plus one for the border.

	mov	esi, edx ; user buffer
	;mov	ecx, 17
	; 03/08/2022
	sub	ecx, ecx
	mov	cl, 17
	mov	edi, esp
	sub	esp, 20
	call	transfer_from_user_buffer
	;jc	VIDEO_RETURN

	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	;mov	cl, 0
	; 03/08/2022
	sub	cl, cl
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
set_palette_loop:
	mov	al, cl
	out	dx, al
	mov	al, [edi]
	out	dx, al
	inc	edi
	inc	cl
	cmp	cl, 10h
	jne	short set_palette_loop
	mov	al, 11h
	out	dx, al
	mov	al, [edi]
	out	dx, al
	mov	al, 20h
	out	dx, al
	; ifdef VBOX
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	; endif ; VBOX
	add	esp, 20
	jmp	VIDEO_RETURN
	
	; 07/08/2022
toggle_intensity:
	; 03/08/2022
	; 10/08/2016
	; Select Foreground Blink or Bold Background
	; BL = 00h = enable bold backgrounds
	;	    (16 background colors)
        ;      01h = enable blinking foreground
	;	    (8 background colors)

	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
	mov	al, 10h
	out	dx, al
	;mov	dx, 3C1h ; VGAREG_ACTL_READ_DATA
	; 03/08/2022
	inc	dl ; dx = 3C1h
	in	al, dx
	and	al, 0F7h
	and	bl, 01h
	shl	bl, 3
	or	al, bl
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	dec	dl ; dx = 3C0h
	out	dx, al
	mov	al, 20h
	out	dx, al
	; ifdef VBOX
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	; endif ; VBOX
	jmp	VIDEO_RETURN

	; 07/08/2022
vga_palf_unknown:
	sub	eax, eax ; 0 = invalid function
        jmp     _video_return

	; 07/08/2022
vga_palf_101B:
	cmp	al, 1Bh
	;jne	short vga_palf_unknown
	ja	short vga_palf_unknown

	call	gray_scale_summing
        jmp     VIDEO_RETURN 

vga_pal_funcs:
	; 07/08/2022
	; 10/08/2016
	; VGA Palette functions
	;
	; derived from 'Plex86/Bochs VGABios' source code
	; vgabios-0.7a (2011)
	; by the LGPL VGABios developers Team (2001-2008)
	; 'vgabios.c', 'vgarom.asm'

	cmp	al, 0
        je      short set_single_palette_reg
vga_palf_1002:
	cmp	al, 2
        je      short set_all_palette_reg
	; 07/08/2022
	ja	short vga_palf_1003
	jmp	short set_overscan_border_color
	; 07/08/2022
;vga_palf_1001:
;	cmp	al, 1
;	je	short set_overscan_border_color
vga_palf_1003:
	cmp	al, 3
	je	short toggle_intensity
vga_palf_1007:
	cmp	al, 7
        je      short get_single_palette_reg
	jb	short vga_palf_unknown
vga_palf_1008:
	cmp	al, 8
        je      short read_overscan_border_color
vga_palf_1009:
	cmp	al, 9
	;je	short get_all_palette_reg
	; 07/08/2022
	jne	short vga_palf_1010
	jmp	get_all_palette_reg
vga_palf_1010:
	cmp	al, 10h
	;je 	short set_single_dac_reg
	; 07/08/2022
	ja	short vga_palf_1012
	jb	short vga_palf_unknown
	jmp	set_single_dac_reg
vga_palf_1012:
	cmp	al, 12h
	;je	short set_all_dac_reg
	; 07/08/2022
	ja	short vga_palf_1013
	jb	short vga_palf_unknown
	jmp	set_all_dac_reg	
vga_palf_1013:
	cmp	al, 13h
	;je	short select_video_dac_color_page
	; 07/08/2022
	jne	short vga_palf_1015
	jmp	select_video_dac_color_page
vga_palf_1015:
	cmp	al, 15h
	;je	short read_single_dac_reg
	; 07/08/2022
	ja	short vga_palf_1017
	jb	short vga_palf_unknown
	jmp	read_single_dac_reg
vga_palf_1017:
	cmp	al, 17h
	;je	short read_all_dac_reg
	; 07/08/2022
	ja	short vga_palf_1018
	jb	short vga_palf_unknown
	jmp	read_all_dac_reg
vga_palf_1018:
	cmp	al, 18h
        je	short set_pel_mask
vga_palf_1019:
	cmp	al, 19h
        je	short read_pel_mask
vga_palf_101A:
	cmp	al, 1Ah
        ;je	short read_video_dac_state
	; 07/08/2022
	jne	short vga_palf_101B
	jmp	read_video_dac_state

	; 07/08/2022
set_overscan_border_color:
	; 10/08/2016
	; Set Overscan/Border Color Register
	; BH = 6-bit RGB color to display
	;      for that attribute
	
	mov	bl, 11h
  	; 07/08/2022
	;jmp	short set_single_palette_reg

set_single_palette_reg:
	; 03/08/2022 (TRDOS 386 v2.0.5)
	; 12/04/2021 (TRDOS 386 v2.0.3, 32 bit push/pop)
	; 10/08/2016
	; Set One Palette Register
	; BL = register number to set
	;     (a 4-bit attribute nibble: 00h-0Fh)
	; BH = 6-bit RGB color to display
	;      for that attribute

	cmp	bl, 14h
	;;ja	short no_actl_reg1
	;ja	VIDEO_RETURN
	; 03/08/2022
	jna	short sspr_1
	jmp	VIDEO_RETURN
sspr_1:
	;push	ax
	;push	dx
	; 12/04/2021
	push	eax
	push	edx
	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
	mov	al, bl
	out	dx, al
	mov	al, bh
	out	dx, al
	mov	al, 20h
	out	dx, al
	; ifdef VBOX
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	; endif ; VBOX
	;pop	dx
	;pop	ax
	; 12/04/2021
	pop	edx
	pop	eax
;no_actl_reg1:
	jmp	VIDEO_RETURN

	; 07/08/2022
read_overscan_border_color:
	; 10/08/2016
	; Read Overscan Register
	; OUTPUT:
	; BH = current rgbRGB value 
	;      of the overscan/border register

	mov	bl, 11h
	; 07/08/2022
	;jmp	short get_single_palette_reg

get_single_palette_reg:
	; 03/08/2022
	; 10/08/2016
	; Read One Palette Register
        ; INPUT:
	; BL = Palette register to read (00h-0Fh)
	; OUTPUT:
	; BH = Current rgbRGB value of specified register
	;      for that attribute

	cmp	bl, 14h
	;;ja	short no_actl_reg2
	;ja	VIDEO_RETURN
	; 03/08/2022
	jna	short gspr_1
	jmp	VIDEO_RETURN
gspr_1:
	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
	mov	al, bl
	out	dx, al
	;mov	dx, 3C1h ; VGAREG_ACTL_READ_DATA
	; 03/08/2022
	inc	dl ; dx = 3C1h
	in	al, dx
	mov	[esp+13], al ; bh
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
	mov	al, 20h
	out	dx, al
	; ifdef VBOX
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	; endif ; VBOX
	jmp	VIDEO_RETURN

set_pel_mask:
	; 10/08/2016
	; BL = mask value
	mov	dx, 3C6h ; VGAREG_PEL_MASK
	mov	al, bl
	out	dx, al
	jmp	VIDEO_RETURN

read_pel_mask:
	; 10/08/2016
	; Output: BL = mask value
	mov	dx, 3C6h ; VGAREG_PEL_MASK
	in	al, dx
	mov	[esp+12], al ; bl
	jmp	VIDEO_RETURN

read_single_dac_reg:
	; 02/08/2022
	; 10/08/2016
	; Read One DAC Color Register
	; INPUT:
	; BX = color register to read (0-255)
	; OUTPUT:
	; CH = green value (00h-3Fh)
        ; CL = blue value  (00h-3Fh)
        ; DH = red value   (00h-3Fh)

	mov	dx, 3C7h ; VGAREG_DAC_READ_ADDRESS
	mov	al, bl
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	; 02/08/2022
	mov	dl, 0C9h
	in	al, dx
	mov	[esp+21], al ; dh
	in	al, dx
	mov	ch, al
	in	al, dx
	mov	cl, al
	mov	[esp+16], cx ; cx
	jmp	VIDEO_RETURN

read_all_dac_reg:
	; 02/08/2022
	; 12/08/2016
	; 11/08/2016
	; 10/08/2016
	; Read a Block of DAC Color Registers
        ; BX = first DAC register to read (0-00FFh)
        ; ECX = number of registers to read (0-00FFh)
        ; EDX = addr of a buffer to hold R,G,B values
	;	(CX*3 bytes long)

	mov	edi, edx ; user buffer
	mov	edx, ecx
	;shl	dx, 1 ; *2
	; 02/08/2022
	shl	edx, 1
	add	edx, ecx ; edx = 3*ecx
	mov	ebp, esp
	mov	esi, ebp
	sub	esi, edx
	and	si, 0FFFCh ; (dword alignment)
	mov	esp, esi
	push	edx ; 3*ecx
	mov	dx, 3C7h ; VGAREG_DAC_READ_ADDRESS
	mov	al, bl
	out	dx, al
	mov	dx, 3C9h ; VGAREG_DAC_DATA
	mov	ebx, esi
read_dac_loop:
	in	al, dx
	mov	[ebx], al
	inc	ebx
	in	al, dx
	mov	[ebx], al
	inc	ebx
	in	al, dx
	mov	[ebx], al
	inc	ebx
	;dec	cx
	; 02/08/2022
	dec	ecx
	jnz	short read_dac_loop
	pop	ecx ; 3*ecx
	; ECX = transfer (byte) count
	; ESI = source address in system space
	; EDI = user's buffer address
	call	transfer_to_user_buffer
	mov	esp, ebp
	jmp	VIDEO_RETURN

set_single_dac_reg:
	; 03/08/2022 (TRDOS 386 v2.0.5)
	; 12/04/2021 (TRDOS 386 v2.0.3, 32 bit push/pop)
	; 10/08/2016
	; Set One DAC Color Register
	; BX = color register to set (0-255)
        ; CH = green value (00h-3Fh)
        ; CL = blue value  (00h-3Fh)
        ; DH = red value   (00h-3Fh)

	;push	dx
	; 12/04/2021
	push	edx
	mov	dx, 3C8h ; VGAREG_DAC_WRITE_ADDRESS
	mov	al, bl
	out	dx, al
	;;mov	dx, 3C9h ; VGAREG_DAC_DATA
	;inc	dx
	; 03/08/2022
	inc	dl
	;pop	ax
	; 12/04/2021
	pop	eax
	mov	al, ah
	out	dx, al
	mov	al, ch
	out	dx, al
	mov	al, cl
	out	dx, al
	jmp	VIDEO_RETURN

set_all_dac_reg:
	; 02/08/2022 
	; 12/08/2016
	; 11/08/2016
	; 10/08/2016
	; Set a Block of DAC Color Register
	; BX = first DAC register to set (0-00FFh)
	; ECX = number of registers to set (0-00FFh)
	; EDX = addr of a table of R,G,B values 
	;	(it will be CX*3 bytes long)

	mov	esi, edx ; user buffer
	mov	edx, ecx
	;shl	cx, 1 ; *2
	; 02/08/2022
	shl	ecx, 1
	add	ecx, edx ; ecx = 3*ecx
	mov	ebp, esp
	mov	edi, ebp
	sub	edi, ecx
	and	di, 0FFFCh ; (dword alignment)
	mov	esp, edi
	call	transfer_from_user_buffer
	;jc	VIDEO_RETURN

	mov	ecx, edx
	mov	dx, 3C8h ; VGAREG_DAC_WRITE_ADDRESS
	mov	al, bl
	out	dx, al
	;mov	dx, 3C9h ; VGAREG_DAC_DATA
	; 02/08/2022
	inc	dl
set_dac_loop:
	mov	al, [edi]
	out	dx, al
	inc	edi
	mov	al, [edi]
	out	dx, al
	inc	edi
	mov	al, [edi]
	out	dx, al
	inc	edi
	;dec	cx
	; 02/08/2022
	dec	ecx
	jnz	short set_dac_loop
	mov	esp, ebp
	jmp	VIDEO_RETURN

get_all_palette_reg:
	; 03/08/2022
	; 10/08/2016
	; Read All Palette Registers
	; EDX = Address of 17-byte buffer 
	;	to receive data
	
	mov	edi, edx
	mov	ebx, esp
	mov	esi, ebx
	sub	esp, 20

	mov	cl, 0
get_palette_loop:
	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
	mov	al, cl
	out	dx, al
	;mov	dx, 3C1h ; VGAREG_ACTL_READ_DATA
	; 03/08/2022
	;mov	dl, 0C1h
	inc	dl
	in	al, dx
	mov	[ebx], al
	inc	ebx
	inc	cl
	cmp	cl, 10h
	jne	short get_palette_loop
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
	mov	al, 11h
	out	dx, al
	;mov	dx, 3C1h ; VGAREG_ACTL_READ_DATA
	; 03/08/2022
	inc	dl ; dx = 3C1h
	in	al, dx
	mov	[ebx], al
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
	mov	al, 20h
	out	dx, al
	; ifdef VBOX
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx
	; endif ; VBOX

	;mov	ecx, 17 ; transfer (byte) count
	; 03/08/2022
	sub	ecx, ecx
	mov	cl, 17

	; ESI = source address in system space
	; EDI = user's buffer address
	call	transfer_to_user_buffer

	add	esp, 20
	jmp	VIDEO_RETURN

select_video_dac_color_page:
	; 02/08/2022 (TRDOS 386 v2.0.5, code optimization)
	; 12/04/2021 (TRDOS 386 v2.0.3, 32 bit push/pop)
	; 10/08/2016
	; DAC Color Paging Functions
	; BL = 00H = select color paging mode
        ;       BH = paging mode
        ;            00h = 4 blocks of 64 registers
        ;            01h = 16 blocks of 16 registers
	; BL = 01H = activate color page
        ;       BH = DAC color page number
        ;            00h-03h (4-page/64-reg mode)
        ;            00h-0Fh (16-page/16-reg mode)

	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 02/08/2022
	mov	dl, 40h
	mov	al, 10h
	out	dx, al
	;mov	dx, 3C1h ; VGAREG_ACTL_READ_DATA
	; 02/08/2022
	inc	dl ; mov dl, 0C1h
	in	al, dx
	and	bl, 01h
	jnz	short set_dac_page
	and	al, 07Fh
	shl	bh, 7
	or	al, bh
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 02/08/2022
	dec	dl ; mov dl, 0C0h
	out	dx, al
	jmp	short set_actl_normal
set_dac_page:
	;push	ax
	; 12/04/2021
	push	eax
	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 02/08/2022
	mov	dl, 0C0h
	mov	al, 14h
	out	dx, al
	;pop	ax
	; 12/04/2021
	pop	eax
	and	al, 80h
	jnz	short set_dac_16_page
	shl	bh, 2
set_dac_16_page:
	and	bh, 0Fh
	mov	al, bh
	out	dx, al
set_actl_normal:
	mov	al, 20h
	out	dx, al
	; ifdef VBOX
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 02/08/2022
	mov	dl, 0DAh
	in	al, dx
	; endif ; VBOX
	jmp	VIDEO_RETURN

read_video_dac_state:
	; 10/08/2016
	; Query DAC Color Paging State
	; Output:
	; BH = current active DAC color page
        ; BL = current active DAC paging mode

	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	mov	al, 10h
	out	dx, al
	mov	dx, 3C1h ; VGAREG_ACTL_READ_DATA
	in	al, dx
	mov	bl, al
	shr	bl, 7
	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	mov	al, 14h
	out	dx, al
	mov	dx, 3C1h ; VGAREG_ACTL_READ_DATA
	in	al, dx
	mov	bh, al
	and	bh, 0Fh
	test	bl, 01
	jnz	short get_dac_16_page
	shr	bh, 2
get_dac_16_page:
	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	mov	al, 20h
	out	dx, al
	; ifdef VBOX
	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx
	; endif ; VBOX 
	mov	[esp+12], bx ; bx
	jmp	VIDEO_RETURN

; 23/11/2020 - TRDOS 386 v2.0.3
; VBE 2 BOCHS/QEMU emulator extensions
;	for TRDOS 386 v2 kernel (video bios)

; BOCH/QEMU VBE2 VGA BIOS code 
;	by Jeroen Janssen (2002)
;	by Volker Rupper (2003-2020)
; vbe.c (02/01/2020) 

; vbe.h (02/01/2020)

VBE_DISPI_BANK_ADDRESS	equ	0A0000h
VBE_DISPI_BANK_SIZE_KB	equ	64

VBE_DISPI_MAX_XRES	equ	2560
VBE_DISPI_MAX_YRES	equ	1600

VBE_DISPI_IOPORT_INDEX	equ	01CEh
VBE_DISPI_IOPORT_DATA	equ	01CFh

VBE_DISPI_INDEX_ID	equ	00h
VBE_DISPI_INDEX_XRES	equ	01h
VBE_DISPI_INDEX_YRES	equ	02h
VBE_DISPI_INDEX_BPP	equ	03h
VBE_DISPI_INDEX_ENABLE	equ	04h
VBE_DISPI_INDEX_BANK	equ	05h
VBE_DISPI_INDEX_VIRT_WIDTH equ	06h
VBE_DISPI_INDEX_VIRT_HEIGHT equ	07h
VBE_DISPI_INDEX_X_OFFSET equ	08h
VBE_DISPI_INDEX_Y_OFFSET equ	09h
VBE_DISPI_INDEX_VIDEO_MEMORY_64K equ 0Ah
VBE_DISPI_INDEX_DDC	equ	0Bh

VBE_DISPI_ID0		equ	0B0C0h
VBE_DISPI_ID1		equ	0B0C1h
VBE_DISPI_ID2		equ	0B0C2h
VBE_DISPI_ID3		equ	0B0C3h
VBE_DISPI_ID4		equ	0B0C4h
VBE_DISPI_ID5		equ	0B0C5h

VBE_DISPI_DISABLED	equ	00h
VBE_DISPI_ENABLED	equ	01h
VBE_DISPI_GETCAPS	equ	02h
VBE_DISPI_8BIT_DAC	equ	20h
VBE_DISPI_LFB_ENABLED	equ	40h
VBE_DISPI_NOCLEARMEM	equ	80h

VBE_DISPI_LFB_PHYSICAL_ADDRESS equ 0E0000000h

; ***

;// VBE Return Status Info
;// AL
VBE_RETURN_STATUS_SUPPORTED	equ	4Fh
VBE_RETURN_STATUS_UNSUPPORTED	equ	00h
;// AH
VBE_RETURN_STATUS_SUCCESSFULL	equ	00h
VBE_RETURN_STATUS_FAILED	equ	01h
VBE_RETURN_STATUS_NOT_SUPPORTED	equ	02h
VBE_RETURN_STATUS_INVALID	equ	03h

;// VBE Mode Numbers

VBE_MODE_VESA_DEFINED		equ	0100h
VBE_MODE_REFRESH_RATE_USE_CRTC	equ	0800h
VBE_MODE_LINEAR_FRAME_BUFFER	equ	4000h
VBE_MODE_PRESERVE_DISPLAY_MEMORY equ	8000h

;// Mode Attributes

VBE_MODE_ATTRIBUTE_SUPPORTED		   equ	0001h
VBE_MODE_ATTRIBUTE_EXTENDED_INFO_AVAILABLE equ	0002h
VBE_MODE_ATTRIBUTE_COLOR_MODE		   equ	0008h
VBE_MODE_ATTRIBUTE_GRAPHICS_MODE	   equ	0010h
VBE_MODE_ATTRIBUTE_LINEAR_FRAME_BUFFER_MODE equ	0080h
VBE_MODE_ATTRIBUTE_DOUBLE_SCAN_MODE	   equ	0100h
VBE_MODE_ATTRIBUTE_INTERLACE_MODE	   equ	0200h

;// Window attributes

VBE_WINDOW_ATTRIBUTE_RELOCATABLE equ	01h
VBE_WINDOW_ATTRIBUTE_READABLE	 equ	02h
VBE_WINDOW_ATTRIBUTE_WRITEABLE	 equ	04h

;/* Video memory */
VGAMEM_GRAPH equ 0A000h
VGAMEM_CTEXT equ 0B800h
;VGAMEM_MTEXT equ 0B000h

;// Memory model

;VBE_MEMORYMODEL_TEXT_MODE	equ	00h
;VBE_MEMORYMODEL_CGA_GRAPHICS	equ	01h
;VBE_MEMORYMODEL_PLANAR		equ	03h
VBE_MEMORYMODEL_PACKED_PIXEL	equ	04h
;VBE_MEMORYMODEL_NON_CHAIN_4_256 equ	05h
VBE_MEMORYMODEL_DIRECT_COLOR	equ	06h
;VBE_MEMORYMODEL_YUV		equ	07h

;// DirectColorModeInfo

;VBE_DIRECTCOLOR_COLOR_RAMP_PROGRAMMABLE equ 01h
VBE_DIRECTCOLOR_RESERVED_BITS_AVAILABLE equ 02h

VBE_DISPI_TOTAL_VIDEO_MEMORY_MB	equ 16

; 24/11/2020
; vbe.c

%if 1

_vbe_biosfn_return_mode_info:
	; 15/12/2020
	; 12/12/2020
	; Return VBE Mode Information	
	; (call from 'sysvideo')
	;
	; Input:
	;	cx = video (bios) mode
	; Output:
	;	cf = 0 -> (successful)
	;	   MODE_INFO_LIST addr contains MODEINFO
	;	cf = 1 -> error
	;
	; Modified registers: eax, edx, edi
	;

	; pushes for subroutine stack pops compatibility

	;push	ds  ; *
	;push	es  ; **

	push	ebp ; ***
	push	esi ; ****

	xor	edi, edi  ; mov edi, 0

	cmp	byte [vbe3], 3
	jb	short _vbe_rmi_1

	;sub	edi, edi  ; 0 = kernel call (sign)
	;	; no transfer to user's buffer

	; cx = Video mode (for 4F01h, with LFB flag)

	mov	ax, 4F01h

	call	_vbe3_pmfn_return_mode_info

	cmp	ax, 004Fh
	jne	short _vbe_rmi_2 ; fail

	; 15/12/2020
	; cx = vbe video mode
	and	ch, 01h ; clear LFB flag
	mov	esi, VBE3MODEINFOBLOCK - 2
	mov	[esi], cx ; MODEINFO.mode
	call	set_lfbinfo_table
	jmp	short _vbe_rmi_3  ; cf = 0
_vbe_rmi_1:
	cmp	byte [vbe3], 2
	jb	short _vbe_rmi_3 ; cf = 1
	mov	al, [vbe2bios] ; 0C0h-0C5h for emu (*)
	cmp	al, 0C0h ; BOCHS/QEMU/VIRTUALBOX (*) ?
	jb	short _vbe_rmi_3  ; cf = 1
	cmp	al, 0C5h ; (*)
	ja	short _vbe_rmi_2 ; unknown vbios !?

	;xor	edi, edi  ; 0 = kernel call (sign)
	;	; no transfer to user's buffer

	;mov	ax, 4F01h

	; cx = Video mode (for 4F01h, with LFB flag)

	call	vbe_biosfn_return_mode_info
	cmp	ax, 004Fh ; successful ?
	je	short _vbe_rmi_3  ; cf = 0
_vbe_rmi_2:
	stc
	; cf = 1
_vbe_rmi_3:
	pop	esi ; ****
	pop	ebp ; ***

	;pop	es  ; **
	;pop	ss  ; *

	retn


; * (TRDOS 386, INT 31h, VESA Video Bios functions)
; * ---------------------------------------------------------
; * Function 01h - Return VBE Mode Information
; * ---------------------------------------------------------
; * Input:
; *		AX = 4F01h
; *		CX = Mode number
; *    (ES:DI) EDI = Pointer to ModeInfoBlock structure
; * Output:
; *		AX = VBE Return Status
; *
; *----------------------------------------------------------
; *

vbe_biosfn_return_mode_info:
	; 03/08/2022 (TRDOS 386 v2.0.5)
	; 15/12/2020 
	; 14/12/2020
	; 12/12/2020
	; 11/12/2020 (TRDOS 386 v2.0.3)
	;
	; Input:
	;	cx = video (bios) mode
	;      edi = ModeInfoBlock buffer address
	;	     (in user's memory space)
	;      (ax = 4F01h)
	; Output:
	;	ax = 004Fh (successful)
	;	ah > 0 -> error
	;
	; Modified registers: esi

	;;push	ds  ; *
	;;push	es  ; **
	;;push	ebp ; ***
	;;push	esi ; ****

	test	ch, 1
	jnz	short vbe_rmi_1

	; mode number < 100h
	; CGA/VGA mode is not proper this VBE function 

	sub	eax, eax
vbe_rmi_0:	
	;mov	ax, 0100h  ; Function is not supported
	mov	ah, 1
	retn
vbe_rmi_1:
	push	edx ; *****
	push	ecx ; ******
	push	ebx ; *******
	push	edi ; ********

	; 14/12/2020
	mov	ebx, ecx

	;xor	eax, eax
	and	bh, 0C1h ; use bit 15, 14, 8 only (for bh)
	mov	[vbe_mode_x], bh
	;and	bx, 1FFh
	and	bh, 1
	;mov	bh, 1

	; Alternative 2 (instead of 'Mode_info_find_mode')
	call	set_mode_info_list ; (alternative 2)

	; eax = 0

	;mov	bx, [esi] ; mode

	; Alternative 1 (instead of 'set_mode_info_list')
	;call	mode_info_find_mode ; (alternative 1)

	or	esi, esi
	; 14/12/2020
	jz	short vbe_rmi_4	; VBE mode number is wrong
				; or it is not supported

	; 15/12/2020
	;mov	bx, [esi] ; mode

	; 12/12/2020
	;call	set_lfbinfo_table

	test	byte [vbe_mode_x], 40h ; LFB model ?
	jz	short vbe_rmi_2
	
	mov	byte [esi+MODEINFO.NumberOfBanks], 1
vbe_rmi_2:
	; (vbe.c, 02/01/2020, vruppert)
	; 11/12/2020 (Erdogan Tan, video.s) 
	; Bochs Graphics Adapter
	; vendor_id: 1111h, device id: 1234h

	call	pci_get_lfb_addr
	;or	eax, eax
	jz	short vbe_rmi_3
	; zf = 0, ax > 0 (high word of LFB address)
	; set/change LFB address in MODEINFO structure
	mov	[esi+MODEINFO.PhysBasePtr+2], ax
	; 12/12/2020
	;mov	[edi+LFBINFO.LFB_addr+2], ax
vbe_rmi_3:
	;test	byte [esi+MODEINFO.WinAAttributes], 1
	;		; VBE_WINDOW_ATTRIBUTE_RELOCATABLE = 1
        ;jz	short vbe_rmi_4
	;; 11/12/2020
	;; In fact, this is far call address in (Bochs/BGA) Video Bios
	;; Direct user access to kernel subroutines is not possible
	;; in TRDOS 386. Also, TRDOS 386 kernel will support only LFB.
	;; Bank select may be a seperate sysvideo function in future
	;; (if it will be required).
	;mov	dword [esi+MODEINFO.WinFuncPtr], dispi_set_bank_farcall
;vbe_rmi_4:
	; 12/12/2020
	call	set_lfbinfo_table

	; 11/12/2020
	; copy 68 bytes of MODE_INFO_LIST to user

	mov	edi, [esp]  ; user's buffer address
	; 12/12/2020
	or	edi, edi ; 0 = kernel call 
			 ; (call from '_vbe_biosfn_return_mode_info')
	jz	short vbe_rmi_6

	; 15/12/2020
	; prepare 256 bytes MODEINFO buffer at VBE3MODEINFOBLOCK
	; and then, copy buffer conttent to user's buffer
	push	edi
	mov	esi, MODE_INFO_LIST + 2	; MODEINFO.ModeAttributes
	mov	edi, VBE3MODEINFOBLOCK
	;mov	ecx, 66/4  ; 66 bytes
	; 03/08/2022
	sub	ecx, ecx
	mov	cl, 66/4
	rep	movsd
	xor	eax, eax
	mov	cl, (256-68)/4 ; 188 bytes
	rep	stosd
	stosw	; 2 bytes
	pop	edi
	mov	esi, VBE3MODEINFOBLOCK
	;mov	cx, 256 
	inc	ch ; cx = 256
	call	transfer_to_user_buffer
	jnc	short vbe_rmi_5
vbe_rmi_4:
	;mov	eax, 014Fh ; fail/error
	xor	eax, eax
	mov	ah, 01h
	;jmp	short vbe_rmi_6
	jmp	vbe_sm_ret1 ; 11/12/2020
vbe_rmi_5:
	; 256 bytes of MODEINFO have been transferred to user
	;mov	eax, 4Fh ; succesfull
vbe_rmi_6: ; 12/12/2020
	xor	eax, eax
;vbe_rmi_6:
	jmp	vbe_sm_ret1 ; 11/12/2020

	;pop	edi ; ********
	;pop	ebx ; *******
	;pop	ecx ; ******
	;pop	edx ; *****

	;;pop	esi ; ****
	;;pop	ebp ; ***
	;;pop	es  ; **
	;;pop	ds  ; * 

	;retn

set_lfbinfo_table:
	; 19/12/2020
	; 11/12/2020
	; Set/Fill LFBINFO structure/table
	;
	; Input:
	;	esi = Mode info list address
	; Output:
	;	LFB_Info address is filled with LFBINFO
	;	edi = LFB_Info address
	;
	; Modified registers: eax, edx (=0), edi

	mov	edi, LFB_Info
	mov	eax, [esi+MODEINFO.PhysBasePtr]
	mov	[edi+LFBINFO.LFB_addr], eax ; LFB address
	;mov	ax, [esi+MODEINFO.mode]
	mov	ax, [esi]
	mov	[edi+LFBINFO.mode],ax
	mov	al, [esi+MODEINFO.BitsPerPixel]
	mov	[edi+LFBINFO.bpp], al
	sub	eax, eax
	mov	ax, [esi+MODEINFO.XResolution]
	mov	[edi+LFBINFO.X_res], ax
	mov	edx, eax ; 19/12/2020
	mov	ax, [esi+MODEINFO.YResolution]
	mov	[edi+LFBINFO.Y_res], ax
	; eax = Y_res ; screen height
	; 19/12/2020
	mul	edx ; X_res*Y_res
	; edx = 0
	mov	dl, [edi+LFBINFO.bpp]
	; Note:
	; Bits per pixel may be 8,16,24,32 for TRDOS 386 v2.
	; (4 bits for pixel is not used for VESA modes here)
	shr	dl, 3 ; convert bits to byte
	mul	edx
	; eax = screen/page/buffer size in bytes
	mov	[edi+LFBINFO.LFB_size], eax
	; edx = 0
	; clear reserved byte in LFBINFO structure/table
	mov	[edi+LFBINFO.reserved], dl ; not necessary
	retn

; * (TRDOS 386, INT 31h, VESA Video Bios functions)
; * ---------------------------------------------------------
; * Function 02h - Set VBE Mode
; * ---------------------------------------------------------
; * Input:
; *		AX = 4F02h
; *		BX = Desired Mode to set
; * Output:
; *		AX = VBE Return Status
; *
; *----------------------------------------------------------
; *

vbe_biosfn_set_mode:
	; 07/03/2021
	; 12/12/2020
	; 11/12/2020 (LFBINFO table for VESA VBE modes)
	; 27/11/2020
	; 25/11/2020
	; 23/11/2020 (TRDOS 386 v2.0.3)
	; (ref: vbe.c, 02/01/2020, vruppert)
	;
	; Input:
	;	bx = video (bios) mode
	;	ax = 4F02h
	; Output:
	;	ax = 004Fh (successful)
	;	ah > 0 -> error
	;
	; Modified registers: esi

	; 27/11/2020

	;;push	ds  ; *
	;;push	es  ; **
	;;push	ebp ; ***
	;;push	esi ; **** 

	; 11/12/2020
	push	edx ; *****
	push	ecx ; ******
	push	ebx ; *******
	push	edi ; ********

	;xor	eax, eax
	and	bh, 0C1h ; use bit 15, 14, 8 only (for bh)
	mov	[vbe_mode_x], bh
	and	bh, 1
	jnz	short vbe_sm_3  ; VESA VBE mode

 	;;test	bx, 4000h ; VBE_MODE_LINEAR_FRAME_BUFFER
	;test	bh, 40h
	;jz	short vbe_sm_0
	;; lfb_flag
	;mov	al, 40h	; VBE_DISPI_LFB_ENABLED
vbe_sm_0:
	; 27/11/2020
	mov	al, 80h
	;test	bh, 80h ; VBE_MODE_PRESERVE_DISPLAY_MEMORY
	;jnz	short vbe_sm_1 ; no_clear
	;; clear
	;sub	al, al ; 0
	test	[vbe_mode_x], al ; 80h
	jz	short vbe_sm_1 ; clear display memory
	; no_clear
	or	bl, al ; VBE_MODE_PRESERVE_DISPLAY_MEMORY
vbe_sm_1:
	; check non vesa mode
	;;cmp	bx, 100h ; VBE_MODE_VESA_DEFINED
	;;jna	short vbe_sm_2
	;and	bh, 1
	;jnz	short vbe_sm_3

	; BX <= 1FFh

	; 27/11/2020
	;or	bl, al	; al = 80h if no_clear option is set
	;		; al = 0 if no_clear option is not set

	; 25/11/2020
	; VBE DISPI will be disabled in 'biosfn_set_video_mode'
 
	;xor	al, al ; 0 ; VBE_DISPI_DISABLED
	;call	dispi_set_enable

	; call the vgabios in order to set the video mode
	; this allows for going back to textmode with a VBE call
	; (some applications expect that to work)

	;and	bx, 0FFh
	
	; 27/11/2020
biosfn_set_video_mode:
	; _call: call subroutine
	; 26/11/2020 (TRDOS 386 v2.0.3)
	; (ref: vgabios.c, 02/01/2020, vruppert)
	; Input:
	;	bl = VGA video (bios) mode
	; Output:
	;	cf = 1 -> error
	;	cf = 0 -> ok
	;
	; Modified registers: esi

	; 'dispi_set_enable(VBE_DISPI_DISABLED);'

	;mov	ax, 0 ; VBE_DISPI_DISABLED 
	xor	eax, eax ; 0 
	call	dispi_set_enable

	mov	al, bl
	;jmp	_set_mode ; (in 'biosfn_set_video_mode' sub)
	call	_set_mode ; will return with cf=1 only if
			; desired mode is not implemented
	; _retn: return from subroutine
	jc	short vbe_sm_2 ; 25/11/2020

	; 26/11/2020
	xor	eax, eax 
	mov	al, [CRT_MODE]
	; 27/11/2020
	mov	ah, [noclearmem] ; 80h or 0
	;and	ah 80h
	mov	[video_mode], ax ; bit 15 = no_clear flag
				 ; bit 14 = 0 (not LFB model)
vbe_sm_ret1:
	; 11/12/2020
	; (vbe_rmi_4 and vbe_rmi_6 jump here)
	; 27/11/2020
	mov	al, 4Fh ; Function call successful
	; eax = 004Fh
vbe_sm_ret2:
	; 11/12/2020
	pop	edi ; ********
	pop	ebx ; *******
	pop	ecx ; ******
	pop	edx ; *****

	;;pop	esi ; ****
	;;pop	ebp ; ***
	;;pop	es  ; **
	;;pop	ds  ; * 

	retn

vbe_sm_2:
	;mov	ax, 0100h ; Function is not supported
	; 27/11/2020
	xor	eax, eax
	mov	ah, 01h
	; eax = 0100h
	;retn
	jmp	short vbe_sm_ret2

vbe_sm_3:
	; 12/12/2020
	; check current mode, if it is 03h
	; save page contents and cursor positions
	cmp	byte [CRT_MODE], 03h
	;jne	short vbe_sm_0
	jne	short vbe_sm_4 ; 07/03/2021
	call	save_mode3_multiscreen
	; set current mode to extended (SVGA) mode
	;mov	byte [CRT_MODE], 0FFh ; VESA VBE mode
vbe_sm_4:
	; 27/11/2020
	; bx = mode (bit 0 to 8)

	; 25/11/2020

	; Alternative 2 (instead of 'Mode_info_find_mode')
	;push	edi
	call	set_mode_info_list ; (alternative 2)
	;pop	edi

	;mov	bx, [esi] ; mode

	; Alternative 1 (instead of 'set_mode_info_list')
	;call	mode_info_find_mode ; (alternative 1)

	or	esi, esi
	jz	short vbe_sm_2 ; VBE mode number is wrong
			       ; or it is not supported

	; 11/12/2020
	mov	bx, [esi] ; mode

	; 27/11/2020
	or	bh, [vbe_mode_x]

	; save VESA VBE mode
	mov	[video_mode], bx
		; 27/11/2020
		; bit 0 to 8 = VESA VBE mode
		; bit 9 to 13 = 0 (bit 0 to 13 = mode)
		; bit 14 = Linear/Flat Frame Buffer flag
		; bit 15 = 'memory not cleared
		;	   at last mode set' flag

	; first disable current mode
	; (when switching between vesa modes)
	; 'dispi_set_enable(VBE_DISPI_DISABLED);'

	;mov	ax, VBE_DISPI_DISABLED ; 0
	sub	eax, eax ; 0

	call	dispi_set_enable

	; 11/12/2020
	mov	al, [esi+MODEINFO.BitsPerPixel]
	; ah = 0

	;cmp	byte [esi+MODEINFO.BitsPerPixel], 8
	cmp	al, 8
	jne	short vbe_sm_5

	; 11/12/2020
	;push	edi
	push	eax
	; 'load_dac_palette(3);'
	push	esi
	mov	ah, 3  ; palette3, 256 colors
	call	load_dac_palette
	pop	esi
	; 11/12/2020
	pop	eax
	;pop	edi
vbe_sm_5:
  	;'dispi_set_bpp(cur_info->info.BitsPerPixel);'
	; 11/12/2020 (al = bits per pixel, ah = 0)
	;xor	ah, ah
	;mov	al, [esi+MODEINFO.BitsPerPixel]
	call	dispi_set_bpp
        ;'dispi_set_xres(cur_info->info.XResolution);'
	mov	ax, [esi+MODEINFO.XResolution]
	call	dispi_set_xres
        ;'dispi_set_yres(cur_info->info.YResolution);'
	mov	ax, [esi+MODEINFO.YResolution]
	call	dispi_set_yres

	;'dispi_set_bank(0);'
	;xor	ax, ax
	xor	eax, eax ; 0
	call	dispi_set_bank
        ;'dispi_set_enable(VBE_DISPI_ENABLED|no_clear|lfb_flag);'
	;mov	ax, di
	; ah = 0 ; 27/11/2020
	mov	al, [vbe_mode_x] ; restore VBE mode bit 14 & 15
	or	al, 1 ; VBE_DISPI_ENABLED
	call	dispi_set_enable

        ; 'vga_compat_setup();'
	call	vga_compat_setup

	; 11/12/2020
	call	set_lfbinfo_table

	; 26/11/2020
	xor	eax, eax
	dec	al 
	mov	[CRT_MODE], al ; 0FFh = VESA VBE mode sign

	; 27/11/2020
	jmp	vbe_sm_ret1 ; Function call successful

	; 27/11/2020
	;mov	al, 4Fh
	;	; eax = 004Fh = Function call successful
	;jmp	short vbe_sm_ret2

; * (TRDOS 386, INT 31h, VESA Video Bios functions)
; * ---------------------------------------------------------
; * Function 03h - Return Current VBE Mode
; * ---------------------------------------------------------
; * Input:
; *		AX = 4F03h
; * Output:
; *		AX = VBE Return Status
; *		BX = Current VBE Mode
; * 
; *----------------------------------------------------------
; *

vbe_biosfn_return_current_mode:
	; 11/12/2020
	; 27/11/2020 (TRDOS 386 v2.0.3)
	; (ref: vbe.c, 02/01/2020, vruppert)
	;
	; Input:
	;	none
	; Output:
	;	ax = 004Fh (successful)
	;	ah > 0 -> error
	;	bx = current video (bios) mode (if ah = 0)
	;
	; Modified registers: eax, ebx

	; 27/11/2020

	;;push	ds  ; *
	;;push	es  ; **
	;;push	ebp ; ***
	;;push	esi ; **** 

	;push	edx ; *****

	; (vbe.c)
	;call	dispi_get_enable
	;	; ax = vbe display interface status
	;and	al, 1 ; VBE_DISPI_ENABLED
	;jnz	short vbe_gm_1  ; VBE graphics mode

	mov	al, [CRT_MODE] ; current cga/vga mode
	cmp	al, 0FFh ; VBE extension signature
	jb	short vbe_gm_1 ; get CGA/VGA mode

	; get VBE mode
vbe_gm_0:
	mov	ax, [video_mode]
		; BX bits:
		; bit 0 to 8 = VESA VBE video mode
		; bit 9 to 13 = 0 
		; bit 14 = last mode set LFB option
		;	   1 - linear/flat frame buffer
		;	   0 - windowed frame buffer
		; bit 15 = last mode set no_clear option
		;	   0 - video memory cleared
		;	   1 - video memory not cleared

vbe_gm_return:
	;pop	edx ; ******
	movzx	ebx, ax
;vbe_srs_retn:
	xor	eax, eax ; 0
	mov	al, 4Fh ; ax = 004Fh (successful)
	retn

vbe_gm_1:
	; legacy (old, standard) CGA/VGA bios video mode
	mov	ah, [noclearmem] ; 80h or 0
		; BX bits: 
		; bit 0 to 7 = video mode
		; bit 8 to 13 = 0 
		; bit 14 = 0 (not LFB mode) CGA/VGA
		; bit 15 = 1 if [noclearmem] = 80h
		;	   0 if [noclearmem] = 0
	jmp	short vbe_gm_return

; * (TRDOS 386, INT 31h, VESA Video Bios functions) 
; * ---------------------------------------------------------
; * Function 04h - Save/Restore State
; * ---------------------------------------------------------
; * Input:
; *		AX = 4F04h
; *             DL = 00h Return Save/Restore State buff size
; *                  01h Save State
; *                  02h Restore State
; *             CX = Requested states
; *		     bit 0 - controller hardware state
; *		     bit 1 - BIOS data state
; *		     bit 2 - DAC state
; *		     bit 3 - register state
; *    (ES:BX) EBX = Pointer to buffer (if DL <> 00h)
; * Output:
; *		AX = VBE Return Status
; *		BX = Number of 64-byte blocks
; *		     to hold the state buffer (if DL=00h)
; *
; *----------------------------------------------------------
; *

vbe_biosfn_save_restore_state:
	; 23/01/2021
	; 16/01/2021
	; 14/01/2021
	; 13/01/2021
	; 12/01/2021
	; 11/01/2021 (TRDOS 386 v2.0.3)
	; (ref: vbe.c, 02/01/2020, vruppert)
	;
	; Input:
	;	dl = sub function
	;	cl = requested state
	;      ebx = pointer to buffer (if dl<>00h)
	; Output:
	;	ax = 004Fh (successful)
	;	ah > 0 -> error
	;	bx = Number of 64-byte blocks 
	;	     to hold the state buffer (if DL=00h)

	; Modified registers: eax, ebx, edi

	; 14/01/2021
	or	ebx, ebx ; user's buffer address
	jnz	short _vbe_biosfn_save_restore_state

	and	dl, dl
	jz	short _vbe_biosfn_save_restore_state

	; function failed
	;mov	eax, 0100h
	;xor	eax, eax
	;inc	ah  ; eax = 0100h
	; 16/01/2021
	mov	eax, 014Fh
	retn

_vbe_biosfn_save_restore_state:
	; 23/01/2021
	; 14/01/2021
	; ebx = 0 if the caller is kernel ('sysvideo')

	; 13/01/2021
	push	edi
	push	edx
	push	ecx

	; 23/01/2021
	; 12/01/2021
	cmp	dl, 2
	ja	short vbe_srs_7 ; 23/01/2021
			; invalid sub function
	cmp	ecx, 0Fh
	ja	short vbe_srs_7 ; invalid !

	and	dl, dl
	jnz	short vbe_srs_4

	; DL = 0
	; Return Save/Restore State buffer size

	;mov	ebx, ecx
	;shl	bl, 1
	;mov	bx, [ebx+vbestatebufsize]
	call	vbe_srs_gbs

;	; 11/01/2021
;	test	cl, 8
;	jz	short vbe_srs_3
;	; vbe_biosfn_read_video_state_size();
;	; return 9 * 2;
;	mov	bl, 18 ; register state size
;vbe_srs_0:
;	test	cl, 1
;	jz	short vbe_srs_1
;	; size += 0x46;
;	add	bl, 70	; controller state size
;vbe_srs_1:
;	test	cl, 2
;	jz	short vbe_srs_2
;	; size += (5 + 8 + 5) * 2 + 6;
;	;add	bl, 42 ; BIOS data state size ; Bochs/Plex86
;	; 12/01/2021
;	add	bl, 40 ; TRDOS 386 v2 VBIOS data state size
;vbe_srs_2:
;	test	cl, 4
;	jz	short vbe_srs_3
;	; size += 3 + 256 * 3 + 1;
;	add	bx, 772 ; DAC state size

vbe_srs_3:
	add	bx, 63
	shr	bx, 6 ; / 64

vbe_srs_retn:
	xor	eax, eax ; 0
vbe_srs_0:  ; 16/01/2021
	mov	al, 4Fh ; ax = 004Fh (successful)
;vbe_srs_0:
	; 13/01/2021
	pop	ecx
	pop	edx
	pop	edi

	retn

	; 23/01/2021
;vbe_srs_10:
	;; 14/01/2021
	; return to 'sysvideo'
	;mov	ebx, ecx ; transfer count
	;	; (byte count for saving current video state)
	;jmp	short vbe_srs_retn

vbe_srs_4:
	; 23/01/2021
	and	cl, 0Fh ; 8, 4, 2, 1
	jz	short vbe_srs_7 ; cx = 0 -> invalid !

	mov	edi, VBE3SAVERESTOREBLOCK

	cmp	dl, 1
	ja	short vbe_srs_8

	; save video state

	test	cl, 07h ; 4, 2, 1
	jz	short vbe_srs_5  ; vbe dispi regs state

	call	biosfn_save_video_state
	; edi = current position
	;	in VBE3SAVERESTOREBLOCK
	; 	(VGA save_state offset)
	; modified regs: edi, eax, edx, ch
	test	cl, 8
	jz	short vbe_srs_6
vbe_srs_5:
	call	vbe_biosfn_save_video_state
	; edi = end position
	;	in VBE3SAVERESTOREBLOCK
	; 	(VGA save_state offset)
	; modified regs: edi, eax, edx, ch
vbe_srs_6:
	; 23/01/2021
	and	ebx, ebx 
	jz	short vbe_srs_retn ; the caller is kernel

	mov	esi, VBE3SAVERESTOREBLOCK
	sub	edi, esi
	mov	ecx, edi ; transfer count in bytes

	;; 14/01/2021
	;and	ebx, ebx 
	;jz	short vbe_srs_10 ; the caller is kernel

	mov	edi, ebx ; user's buffer address
	call	transfer_to_user_buffer
	jnc	short vbe_srs_retn
vbe_srs_7:
	; // function failed
	;mov	eax, 0100h
	xor	eax, eax
	inc	ah  ; eax = 0100h
	; 16/01/2021
	; ax = 0014Fh
	;retn
	; 13/01/2021
	jmp	short vbe_srs_0
vbe_srs_8:
	;cmp	dl, 2
	;jne	short vbe_srs_7
	;		; invalid sub function

	; 14/01/2021
	or	ebx, ebx ; user's buffer address
	;jnz	short vbe_srs_11

	; the caller is kernel ('sysvideo')
	;jmp	short vbe_srs_12
	; 23/01/2021
	jz	short vbe_srs_12 ; 'sysvideo' call
vbe_srs_11:
	mov	esi, ebx ; user's buffer address
	; 23/01/2021
	;push	ebx

	call	vbe_srs_gbs

	; restore video state

	;mov	edi, VBE3SAVERESTOREBLOCK
	push	ecx
	mov	ecx, ebx ; transfer count in bytes
	call	transfer_from_user_buffer
	pop	ecx
	; 23/01/2021
	;pop	ebx
	mov	ebx, esi
	jc	short vbe_srs_7

vbe_srs_12:
	;mov	esi, VBE3SAVERESTOREBLOCK
	mov	esi, edi

	test	cl, 07h ; 4, 2, 1
	jz	short vbe_srs_9	; vbe dispi regs state

	call	biosfn_restore_video_state
	jc	short vbe_srs_7 ; invalid buffer content !
	; esi = current position
	;	in VBE3SAVERESTOREBLOCK
	; 	(VGA save_state offset)
	; modified regs: esi, eax, edx, ch
	test	cl, 8
	;jz	short vbe_srs_10
	; 23/01/2020
	jmp	short vbe_srs_retn
vbe_srs_9:
	call	vbe_biosfn_restore_video_state

	; modified regs: esi, eax, edx, ch

	jmp	short vbe_srs_retn

;vbe_srs_10:
;	; successful
;	xor	eax, eax ; 0
;	mov	al, 4Fh ; ax = 004Fh (successful)
;	retn

vbe_srs_gbs:
	; return buffer size according to flags
	mov	ebx, ecx ; options/flags
	shl	bl, 1
	mov	bx, [ebx+vbestatebufsize]
	retn

vbestatebufsize:
	; ----------------------------------------
	; CL =	0  1   2    3    4    5    6    7
	; ----------------------------------------
	dw	0, 70, 40, 110, 772, 842, 812, 882
	; ----------------------------------------
	; CL =	8   9   10  11   12   13   14   15
	; ----------------------------------------
	dw	18, 88, 58, 128, 790, 860, 830, 900

; 11/01/2021
VGAREG_ACTL_ADDRESS	equ 3C0h
VGAREG_ACTL_WRITE_DATA	equ 3C0h
VGAREG_ACTL_READ_DATA	equ 3C1h

VGAREG_INPUT_STATUS	equ 3C2h
VGAREG_WRITE_MISC_OUTPUT equ 3C2h
VGAREG_VIDEO_ENABLE	equ 3C3h
VGAREG_SEQU_ADDRESS	equ 3C4h
VGAREG_SEQU_DATA	equ 3C5h

VGAREG_PEL_MASK		equ 3C6h
VGAREG_DAC_STATE	equ 3C7h
VGAREG_DAC_READ_ADDRESS	equ 3C7h
VGAREG_DAC_WRITE_ADDRESS equ 3C8h
VGAREG_DAC_DATA		equ 3C9h

VGAREG_READ_FEATURE_CTL	equ 3CAh
VGAREG_READ_MISC_OUTPUT	equ 3CCh

VGAREG_GRDC_ADDRESS	equ 3CEh
VGAREG_GRDC_DATA	equ 3CFh

;VGAREG_MDA_CRTC_ADDRESS equ 3B4h
;VGAREG_MDA_CRTC_DATA	equ 3B5h
VGAREG_VGA_CRTC_ADDRESS	equ 3D4h
VGAREG_VGA_CRTC_DATA	equ 3D5h

;VGAREG_MDA_WRITE_FEATURE_CTL equ 3BAh
VGAREG_VGA_WRITE_FEATURE_CTL equ 3DAh
VGAREG_ACTL_RESET	equ 3DAh

;VGAREG_MDA_MODECTL	equ 3B8h
VGAREG_CGA_MODECTL	equ 3D8h
VGAREG_CGA_PALETTE	equ 3D9h

biosfn_save_video_state:
	; 03/08/2022 (TRDOS 386 v2.0.5)
	; 22/01/2021
	; 12/01/2021
	; 11/01/2021 (TRDOS 386 v2.0.3)
	; (vgabios.c)

	; modified registers: eax, edx, edi, ch

	;mov	edi, VBE3SAVERESTOREBLOCK

	; input: edi = state buffer address

	test	cl, 1
	jz	bfn_svs_4

	mov	dx, VGAREG_SEQU_ADDRESS ; 3C7h
	in	al, dx
	stosb
	;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	mov	dl, 0D4h
	in	al, dx
	stosb
  	;mov	dx, VGAREG_GRDC_ADDRESS ; 3CEh
        mov	dl, 0CEh
	in	al, dx
	stosb
	;mov	dx, VGAREG_ACTL_RESET ; 3DAh
	mov	dl, 0DAh
	in	al, dx
	;mov	dx, VGAREG_ACTL_ADDRESS ; 3C0h
	mov	dl, 0C0h
	in	al, dx
	stosb
	mov	ah, al ; ar_index
	;mov	dx, VGAREG_READ_FEATURE_CTL ; 3CAh
	mov	dl, 0CAh
	in	al, dx
	stosb
	; (5 bytes are writen above)

	; for(i=1;i<=4;i++){
	mov	al, 1
	;;mov	dx, VGAREG_SEQU_ADDRESS ; 3C4h
	;mov	dl, 0C4h
	mov	ch, 4
bfn_svs_0:
	; outb(VGAREG_SEQU_ADDRESS, i);
	;mov	dx, VGAREG_SEQU_ADDRESS ; 3C4h
	mov	dl, 0C4h
	out	dx, al
	;mov	dx, VGAREG_SEQU_DATA  ; 3C5h
	inc	dl  ; dx = 3C5h
	; inb(VGAREG_SEQU_DATA)
	push	eax
	in	al, dx
	stosb	; (4 bytes in loop)
	pop	eax
	;mov	dx, VGAREG_SEQU_ADDRESS ; 3C4h
	;dec	dl
	inc	al  ; i++
	dec	ch
	jnz	short bfn_svs_0

	; outb(VGAREG_SEQU_ADDRESS, 0);
	sub	al, al ; 0
	out	dx, al
	; inb(VGAREG_SEQU_DATA)
	;mov	dx, VGAREG_SEQU_DATA ; 3C5h
	inc	dl  ; dx = 3C5h
	in	al, dx
	stosb	; (+1 byte)

        ; for(i=0;i<=0x18;i++) {
	sub	al, al ; 0
	;;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	;mov	dl, 0D4h
	mov	ch, 25
bfn_svs_1:
        ; outb(crtc_addr,i);
	;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	mov	dl, 0D4h
	out	dx, al
	;mov	dx, VGAREG_VGA_CRTC_DATA ; 3D5h
	inc	dl  ; dx = 3D5h
	; inb(crtc_addr+1)
	push	eax
	in	al, dx
	stosb	; (25 bytes in loop)
	pop	eax
	;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	;dec	dl
	inc	al  ; i++
	dec	ch
	jnz	short bfn_svs_1

	and	ah, 20h  ; (ar_index & 0x20)
        ; for(i=0;i<=0x13;i++) {
	sub	al, al ; 0
	mov	ch, 20
bfn_svs_2:
	; inb(VGAREG_ACTL_RESET);
	;mov	dx, VGAREG_ACTL_RESET ; 3DAh
	mov	dl, 0DAh
	push	eax
	in	al, dx
	mov	al, [esp]
 	; outb(VGAREG_ACTL_ADDRESS, i | (ar_index & 0x20));
	or	al, ah
	;mov	dx, VGAREG_ACTL_ADDRESS ; 3C0h
	mov	dl, 0C0h
	out	dx, al
	;mov	dx, VGAREG_ACTL_READ_DATA ; 3C1h
	;mov	dl, 0C1h
	inc	dl
	in	al, dx
	stosb	; (20 bytes in loop)
	pop	eax
	inc	al  ; i++
	dec	ch
	jnz	short bfn_svs_2

	; inb(VGAREG_ACTL_RESET);
	;mov	dx, VGAREG_ACTL_RESET ; 3DAh
	mov	dl, 0DAh
	in	al, dx

        ; for(i=0;i<=8;i++) {
	sub	al, al ; 0
	;;mov	dx, VGAREG_GRDC_ADDRESS ; 3CEh
	;mov	dl, 0CEh
	mov	ch, 9
bfn_svs_3:
	; outb(VGAREG_GRDC_ADDRESS,i)
	;mov	dx, VGAREG_GRDC_ADDRESS ; 3CEh
	mov	dl, 0CEh
	out	dx, al
	; inb(VGAREG_ACTL_READ_DATA)
	push	eax
	;mov	dx, VGAREG_GRDC_DATA ; 3CFh
	;mov	dl, 0CFh
	inc	dl
	in	al, dx
	stosb	; (9 bytes in loop)
	pop	eax
	;dec	dl
	inc	al  ; i++
	dec	ch
	jnz	short bfn_svs_3

	; write_word(ES, BX, crtc_addr); BX+= 2;
	; (offset 64)
	mov	ax, 3D4h ; VGAREG_VGA_CRTC_ADDRESS
	stosw	; (2 bytes (1 word))

        ; /* XXX: read plane latches */
	xor	eax, eax  ; 0
       	stosd	; (4 bytes)

	; (total 70 bytes are written above as controller hardware state)

bfn_svs_4:
	; 12/01/2021 (TRDOS 386 v2.0.3)
	test	cl, 2
	jz	short bfn_svs_6

	; VIDEO BIOS DATA
	; !!! this data is valid for TRDOS 386 v2 kernel only !!!
	; (this is not same with BOCHS/PLEX86 video bios, BIOS data)

    	; if (CX & 2) {
        ;write_byte(ES, BX, read_byte(BIOSMEM_SEG,BIOSMEM_CURRENT_MODE)); BX++;
        ;write_word(ES, BX, read_word(BIOSMEM_SEG,BIOSMEM_NB_COLS)); BX += 2;
        ;write_word(ES, BX, read_word(BIOSMEM_SEG,BIOSMEM_PAGE_SIZE)); BX += 2;
        ;write_word(ES, BX, read_word(BIOSMEM_SEG,BIOSMEM_CRTC_ADDRESS)); BX += 2;
        ;write_byte(ES, BX, read_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS)); BX++;
        ;write_word(ES, BX, read_word(BIOSMEM_SEG,BIOSMEM_CHAR_HEIGHT)); BX += 2;
        ;write_byte(ES, BX, read_byte(BIOSMEM_SEG,BIOSMEM_VIDEO_CTL)); BX++;
        ;write_byte(ES, BX, read_byte(BIOSMEM_SEG,BIOSMEM_SWITCHES)); BX++;
        ;write_byte(ES, BX, read_byte(BIOSMEM_SEG,BIOSMEM_MODESET_CTL)); BX++;
        ;write_word(ES, BX, read_word(BIOSMEM_SEG,BIOSMEM_CURSOR_TYPE)); BX += 2;
        ;for(i=0;i<8;i++) {
        ;   write_word(ES, BX, read_word(BIOSMEM_SEG, BIOSMEM_CURSOR_POS+2*i));
        ;   BX += 2;
        ;}
        ;write_word(ES, BX, read_word(BIOSMEM_SEG,BIOSMEM_CURRENT_START)); BX += 2;
        ;write_byte(ES, BX, read_byte(BIOSMEM_SEG,BIOSMEM_CURRENT_PAGE)); BX++;
        ;/* current font */
        ;write_word(ES, BX, read_word(0, 0x1f * 4)); BX += 2;
        ;write_word(ES, BX, read_word(0, 0x1f * 4 + 2)); BX += 2;
        ;write_word(ES, BX, read_word(0, 0x43 * 4)); BX += 2;
        ;write_word(ES, BX, read_word(0, 0x43 * 4 + 2)); BX += 2;

	; !!! save TRDOS 386 v2 kernel spesific video bios data !!!
	; (which is/are used by 'SET_MODE' function and/or it's sub functions)

	mov	ax, 3D4h ; CRTC_ADDR, always 3D4h (color VGA) for TRDOS 386 v2
	stosw
	mov	al, [CRT_MODE] ; Current video mode (0FFh for VESA VBE modes)
	stosb
	mov	al, [CRT_MODE_SET] ; 29h for mode 03h ; TRDOS 386 feature only !
	stosb
 	mov	ax, [video_mode] ; Current VESA VBE (SVGA, extended VGA) mode
	stosw			 ; (valid if [CRT_MODE] = 0FFh)
	mov	ax, [CRT_LEN] ; page size (in bytes)
	stosw
	mov	ax, [CRT_START] ; video page start offset
 	stosw
	mov	al, [CRT_COLS] ; nbcols, characters per row
	stosb
	mov	al, [VGA_ROWS] ; nbrows, (character) rows per page (not rows-1)
	stosb
	mov	al, [CHAR_HEIGHT] ; character font height (8 or 16 or 14)
	stosb
	mov	al, [VGA_VIDEO_CTL] ; ROM BIOS DATA AREA Offset 87h
	stosb
	mov	al, [VGA_SWITCHES] ; feature bit switches
	stosb
	mov	al, [VGA_MODESET_CTL] ; basic mode set options
	stosb
	; followings are only used by TRDOS 386 v2 (IBM PC/AT ROMBIOS) code 
	; (bochs/plex86 does not use and return those)
	mov	al, [CRT_PALETTE] ; current color palette ; TRDOS 386 feature only !
	stosb
	mov	al, [ACTIVE_PAGE] ; current video page
	stosb
	mov	ax, [CURSOR_MODE] ; cursor type
	stosw
	;mov	eax, [CURSOR_POSN] ; cursor position for video page 0 and 1
	;stosd
	;mov	eax, [CURSOR_POSN+4] ; cursor position for video page 2 and 3
	;stosd
	;mov	eax, [CURSOR_POSN+8] ; cursor position for video page 4 and 5
	;stosd
	;mov	eax, [CURSOR_POSN+12] ; cursor position for video page 6 and 7
	;stosd
	push	esi
	mov	ch, 4
	mov	esi, CURSOR_POSN
bfn_svs_5:
	movsd
	dec	ch
	jnz	short bfn_svs_5
	pop	esi
	; (font addr) protected mode address in kernel's/system memory space
	; (not accessable/meaningful address value by user)
	mov	eax, [VGA_INT43H] ; VGA current (default) font address
	stosd
	
	; (total 40 bytes are written above as BIOS data state)

bfn_svs_6:
	; 12/01/2021
	test	cl, 4
	jz	short bfn_svs_8

  	;/* XXX: check this */
		; /* read/write mode dac */
        ;write_byte(ES, BX, inb(VGAREG_DAC_STATE)); BX++;
	;	; /* pix address */
        ;write_byte(ES, BX, inb(VGAREG_DAC_WRITE_ADDRESS)); BX++;
        ;write_byte(ES, BX, inb(VGAREG_PEL_MASK)); BX++;
        ;// Set the whole dac always, from 0
        ;outb(VGAREG_DAC_WRITE_ADDRESS,0x00);
        ;for(i=0;i<256*3;i++) {
        ;   write_byte(ES, BX, inb(VGAREG_DAC_DATA)); BX++;
        ;}
        ;write_byte(ES, BX, 0); BX++; /* color select register */

	; /* read/write mode dac */
    	mov	dx, 3C7h ; VGAREG_DAC_STATE
	in	al, dx
	stosb
	; /* pix address */
    	;mov	dx, VGAREG_DAC_WRITE_ADDRESS ; 3C8h
	;mov	dl, 0C8h
	inc	dl
	in	al, dx
	stosb
    	;mov	dx, VGAREG_PEL_MASK  ; 3C6h
	mov	dl, 0C6h
	in	al, dx
	stosb
	;// Set the whole dac always, from 0
	xor	al, al ; 0
	;mov	dx, VGAREG_DAC_WRITE_ADDRESS ; 3C8h
	mov	dl, 0C8h
	out	dx, al

	push	ecx ; 22/01/2021
	;for(i=0;i<256*3;i++) {
	;mov	ecx, 256*3 ; 768 bytes
	; 03/08/2022
	sub	ecx, ecx
	mov	ch, 3
	; ecx = 300h = 768
	;mov	dx, VGAREG_DAC_DATA ; 3C9h
	;mov	dl, 0C9h
	inc	dl ; dx = 3C9h
bfn_svs_7:
	in	al, dx
	stosb
	loop	bfn_svs_7
	pop	ecx ; 22/01/2021

	; /* color select register */
	sub	al, al ; 0
	stosb

	; (total 772 bytes are written above as DAC state)
bfn_svs_8:
	retn

vbe_biosfn_save_video_state:
	; 23/01/2021
	; 13/01/2021
	; 12/01/2021 (TRDOS 386 v2.0.3)
	; (vbe.c)

	; modified registers: eax, edx, edi, ch

	; input: edi = state buffer address
	; output: 
	;	 VBE DISPI register contents will be saved
	;	 (18 bytes, 9 words)

	; outw(VBE_DISPI_IOPORT_INDEX,VBE_DISPI_INDEX_ENABLE);
	; enable = inw(VBE_DISPI_IOPORT_DATA);
  	; write_word(ES, BX, enable);
 	; BX += 2;
	; if (!(enable & VBE_DISPI_ENABLED)) 
	;	return;
	; for(i = VBE_DISPI_INDEX_XRES;
	;		 i <= VBE_DISPI_INDEX_Y_OFFSET; i++) {
	;    if (i != VBE_DISPI_INDEX_ENABLE) {
	;        outw(VBE_DISPI_IOPORT_INDEX, i);
	;        write_word(ES, BX, inw(VBE_DISPI_IOPORT_DATA));
        ;    BX += 2;
	;        }
	; }

	mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	eax, 04h  ; VBE_DISPI_INDEX_ENABLE
	 ;03/08/2022
	out	dx, ax
	;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	inc	dl
	in	ax, dx ; enable (status)
	stosw
	and	ax, 1 ; VBE_DISPI_ENABLED
	jnz	short vbe_bfn_svs_0
	; 23/01/2021
	; ax = 0
	; VBE_DISPI_DISABLED
	; 13/01/2021
	; clear remain 8 bytes 
	;xor	eax, eax
	stosd	; 2
	stosd	; 2
	stosd	; 2
	stosd	; 2
	retn
vbe_bfn_svs_0:
	; VBE_DISPI_ENABLED

	;sub	eax, eax
	sub	al, al ; eax = 0

	; from VBE_DISPI_INDEX_XRES
	;   to VBE_DISPI_INDEX_BPP

 	mov	ch, 3
	; al = 0 ; VBE_DISPI_INDEX_XRES - 1

	call	vbe_bfn_svs_1

	; from VBE_DISPI_INDEX_BANK
	;   to VBE_DISPI_INDEX_Y_OFFSET

	inc	al 
	; al = 4 ; VBE_DISPI_INDEX_BANK - 1

	mov	ch, 5
vbe_bfn_svs_1:
	inc	al ; from VBE_DISPI_INDEX_XRES
		   ;   to VBE_DISPI_INDEX_BPP	
	;mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	dec	dl ; 1CEh
	out	dx, ax
	push	eax
	;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	inc	dl ; 1CFh
	in	ax, dx
	stosw
	pop	eax
	dec	ch
	jnz	short vbe_bfn_svs_1
	retn

biosfn_restore_video_state:
	; 22/01/2021
	; 13/01/2021
	; 12/01/2021 (TRDOS 386 v2.0.3)
	; (vgabios.c)

	; modified registers: eax, edx, esi, edi, ch

	;mov	esi, VBE3SAVERESTOREBLOCK

	; input: esi = state buffer address

	test	cl, 1
	jz	bfn_rvs_6

	cmp	word [esi+64], 3D4h ; must be 3D4h
	je	short bfn_rvs_0  
			; it is seen as valid buffer
	stc
	retn

bfn_rvs_0:
	mov	edi, esi ; addr1
	add	esi, 5 ; skip 1st 5 bytes for now

	; // Reset Attribute Ctl flip-flop
        ; inb(VGAREG_ACTL_RESET);
	mov	dx, 3DAh ; VGAREG_ACTL_RESET
	in	al, dx

	; for(i=1;i<=4;i++){
	mov	al, 1
	;;mov	dx, VGAREG_SEQU_ADDRESS ; 3C4h
	;mov	dl, 0C4h
	mov	ch, 4
bfn_rvs_1:
	; outb(VGAREG_SEQU_ADDRESS, i);
	;mov	dx, VGAREG_SEQU_ADDRESS ; 3C4h
	mov	dl, 0C4h
	out	dx, al
	;mov	dx, VGAREG_SEQU_DATA  ; 3C5h
	inc	dl  ; dx = 3C5h
	; outb(VGAREG_SEQU_DATA)
	push	eax
	lodsb	; (4 bytes in loop)
	out	dx, al
	pop	eax
	;mov	dx, VGAREG_SEQU_ADDRESS ; 3C4h
	;dec	dl
	inc	al  ; i++
	dec	ch
	jnz	short bfn_rvs_1

	; outb(VGAREG_SEQU_ADDRESS, 0);
	sub	al, al ; 0
	out	dx, al
	; outb(VGAREG_SEQU_DATA)
	;mov	dx, VGAREG_SEQU_DATA ; 3C5h
	inc	dl  ; dx = 3C5h
	lodsb	; (+1 byte)
	out	dx, al

	; // Disable CRTC write protection
	; outw(crtc_addr,0x0011);
	;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	mov	dl, 0D4h
	mov	ax, 11h
	out	dx, ax

	; // Set CRTC regs

	; for(i=0;i<=0x18;i++) {
        ;   if (i != 0x11) {
	sub	al, al ; 0
	;;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	;mov	dl, 0D4h
	mov	ch, 25
bfn_rvs_2:
        ; outb(crtc_addr,i);
	;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	mov	dl, 0D4h
	out	dx, al
	;mov	dx, VGAREG_VGA_CRTC_DATA ; 3D5h
	inc	dl  ; dx = 3D5h
	; inb(crtc_addr+1)
	push	eax
	lodsb	; (25 bytes in loop)
	out	dx, al
	pop	eax
	;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	;dec	dl
	inc	al  ; i++
	cmp	al, 17 ; 11h
	jne	short bfn_rvs_3
	lodsb
	mov	ah, al ; *
	mov	al, 18
bfn_rvs_3:
	dec	ch
	jnz	short bfn_rvs_2

	; // select crtc base address
        ; v = inb(VGAREG_READ_MISC_OUTPUT) & ~0x01;
	;if (crtc_addr = 0x3d4)
	;   v |= 0x01;
	; outb(VGAREG_WRITE_MISC_OUTPUT, v);

	;;mov	dx, VGAREG_READ_MISC_OUTPUT ; 3CCh
	;mov	dl, 0CCh
	;in	al, dl
	;and	al, 1
	;;mov	dx, VGAREG_WRITE_MISC_OUTPUT ; 3C2h
	;mov	dl, 0C2h
	;or	al, 1
	;out	dx, al

	; // enable write protection if needed
	;outb(crtc_addr, 0x11);
        ;outb(crtc_addr+1, read_byte(ES, BX - 0x18 + 0x11));
	;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	mov	dl, 0D4h
	mov	al, 11h
	out	dx, al
	mov	al, ah ; *
	inc	dl ; dx = 3D5h
	out	dx, al

	; // Set Attribute Ctl
	mov	ah, [edi+3] ; addr1+3, ah = ar_index
	and	ah, 20h  ; (ar_index & 0x20)

        ; inb(VGAREG_ACTL_RESET);
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	mov	dl, 0DAh
	in	al, dx

        ; for(i=0;i<=0x13;i++) {
	sub	al, al ; 0
	mov	ch, 20
bfn_rvs_4:
 	; outb(VGAREG_ACTL_ADDRESS, i | (ar_index & 0x20));
	push	eax
	or	al, ah
	;mov	dx, VGAREG_ACTL_ADDRESS ; 3C0h
	mov	dl, 0C0h
	out	dx, al
	;mov	dx, VGAREG_ACTL_WRITE_DATA ; 3C0h
	;mov	dl, 0C0h
	lodsb	; (20 bytes in loop)
	out	dx, al
	pop	eax
	inc	al  ; i++
	dec	ch
	jnz	short bfn_rvs_4

	; outb(VGAREG_ACTL_ADDRESS, ar_index);
	;mov	dx, VGAREG_ACTL_ADDRESS ; 3C0h
	;mov	dl, 0C0h
	mov	al, ah ; ar_index
	out	dx, al

	; inb(VGAREG_ACTL_RESET);
	;mov	dx, VGAREG_ACTL_RESET ; 3DAh
	mov	dl, 0DAh
	in	al, dx

        ; for(i=0;i<=8;i++) {
	sub	al, al ; 0
	;;mov	dx, VGAREG_GRDC_ADDRESS ; 3CEh
	;mov	dl, 0CEh
	mov	ch, 9
bfn_rvs_5:
	; outb(VGAREG_GRDC_ADDRESS,i)
	;mov	dx, VGAREG_GRDC_ADDRESS ; 3CEh
	mov	dl, 0CEh
	out	dx, al
	; outb(VGAREG_ACTL_READ_DATA)
	push	eax
	;mov	dx, VGAREG_GRDC_DATA ; 3CFh
	;mov	dl, 0CFh
	inc	dl
	lodsb	; (9 bytes in loop)
	out	dx, al
	pop	eax
	;dec	dl
	inc	al  ; i++
	dec	ch
	jnz	short bfn_rvs_5

	; BX += 2; /* crtc_addr */     ; 3D4h
        ; BX += 4; /* plane latches */ ; 0
	add	esi, 6	      
	push	esi ; *	

	;outb(VGAREG_SEQU_ADDRESS, read_byte(ES, addr1)); addr1++;
        ;outb(crtc_addr, read_byte(ES, addr1)); addr1++;
        ;outb(VGAREG_GRDC_ADDRESS, read_byte(ES, addr1)); addr1++;
        ;addr1++;
        ;outb(crtc_addr - 0x4 + 0xa, read_byte(ES, addr1)); addr1++;

	mov	esi, edi ; start of state buffer

	;mov	dx, VGAREG_SEQU_ADDRESS ; 3C7h
	mov	dl, 0C7h 
	lodsb
	out	dx, al
	;mov	dx, VGAREG_VGA_CRTC_ADDRESS ; 3D4h
	mov	dl, 0D4h
	lodsb
	out	dx, al
	;mov	dx, VGAREG_GRDC_ADDRESS ; 3CEh
        mov	dl, 0CEh
	lodsb
	out	dx, al
	lodsb	; addr1++
	;mov	dx, VGAREG_VGA_WRITE_FEATURE_CTL ; 3DAh
	mov	dl, 0DAh
	lodsb
	out	dx, al

	pop	esi ; *

	; (total 70 bytes are read above as controller hardware state)

bfn_rvs_6:
	; 13/01/2021
	test	cl, 2
	jz	short bfn_rvs_9

	; VIDEO BIOS DATA
	; !!! this data is valid for TRDOS 386 v2 kernel only !!!
	; (this is not same with BOCHS/PLEX86 video bios, BIOS data)  

    	; if (CX & 2) {
	;write_byte(BIOSMEM_SEG,BIOSMEM_CURRENT_MODE, read_byte(ES, BX)); BX++;
        ;write_word(BIOSMEM_SEG,BIOSMEM_NB_COLS, read_word(ES, BX)); BX += 2;
        ;write_word(BIOSMEM_SEG,BIOSMEM_PAGE_SIZE, read_word(ES, BX)); BX += 2;
        ;write_word(BIOSMEM_SEG,BIOSMEM_CRTC_ADDRESS, read_word(ES, BX)); BX += 2;
        ;write_byte(BIOSMEM_SEG,BIOSMEM_NB_ROWS, read_byte(ES, BX)); BX++;
        ;write_word(BIOSMEM_SEG,BIOSMEM_CHAR_HEIGHT, read_word(ES, BX)); BX += 2;
        ;write_byte(BIOSMEM_SEG,BIOSMEM_VIDEO_CTL, read_byte(ES, BX)); BX++;
        ;write_byte(BIOSMEM_SEG,BIOSMEM_SWITCHES, read_byte(ES, BX)); BX++;
        ;write_byte(BIOSMEM_SEG,BIOSMEM_MODESET_CTL, read_byte(ES, BX)); BX++;
        ;write_word(BIOSMEM_SEG,BIOSMEM_CURSOR_TYPE, read_word(ES, BX)); BX += 2;
        ;for(i=0;i<8;i++) {
        ;   write_word(BIOSMEM_SEG, BIOSMEM_CURSOR_POS+2*i, read_word(ES, BX));
        ;   BX += 2;
        ;}
        ;write_word(BIOSMEM_SEG,BIOSMEM_CURRENT_START, read_word(ES, BX)); BX += 2;
        ;write_byte(BIOSMEM_SEG,BIOSMEM_CURRENT_PAGE, read_byte(ES, BX)); BX++;
        ;/* current font */
        ;write_word(0, 0x1f * 4, read_word(ES, BX)); BX += 2;
        ;write_word(0, 0x1f * 4 + 2, read_word(ES, BX)); BX += 2;
        ;write_word(0, 0x43 * 4, read_word(ES, BX)); BX += 2;
        ;write_word(0, 0x43 * 4 + 2, read_word(ES, BX)); BX += 2;

	; !!! save TRDOS 386 v2 kernel spesific video bios data !!!
	; (which is/are used by 'SET_MODE' function and/or it's sub functions)

	lodsw	 ; CRTC_ADDR, always 3D4h (color VGA) for TRDOS 386 v2
	; skip 3D4h check if it is already checked
	test	cl, 1
	jnz	short bfn_rvs_7
	cmp	ax, 3D4h
	je	short bfn_rvs_7
	stc
	retn
bfn_rvs_7:
	lodsb
	mov	[CRT_MODE], al ; Current video mode (0FFh for VESA VBE modes) 
	lodsb
	mov	[CRT_MODE_SET], al ; 29h for mode 03h ; TRDOS 386 feature only !
	lodsw	
 	mov	[video_mode], ax ; Current VESA VBE (SVGA, extended VGA) mode
	lodsw		 ; (valid if [CRT_MODE] = 0FFh)
	mov	[CRT_LEN], ax ; page size (in bytes)
	lodsw
	mov	[CRT_START], ax ; video page start offset
 	lodsb
	mov	[CRT_COLS], al ; nbcols, characters per row
	lodsb
	mov	[VGA_ROWS], al ; nbrows, (character) rows per page (not rows-1)
	lodsb
	mov	[CHAR_HEIGHT], al ; character font height (8 or 16 or 14)
	lodsb
	mov	[VGA_VIDEO_CTL], al ; ROM BIOS DATA AREA Offset 87h
	lodsb
	mov	[VGA_SWITCHES], al ; feature bit switches
	lodsb
	mov	[VGA_MODESET_CTL], al ; basic mode set options
	; followings are only used by TRDOS 386 v2 (IBM PC/AT ROMBIOS) code
	; (bochs/plex86 does not use and return those)
	lodsb
	mov	[CRT_PALETTE], al ; current color palette ; TRDOS 386 feature only !
	lodsb
	mov	[ACTIVE_PAGE], al ; current video page
	lodsw
	mov	[CURSOR_MODE], ax ; cursor type
	;lodsd
	;mov	[CURSOR_POSN], eax ; cursor position for video page 0 and 1 
	;lodsd
	;mov	[CURSOR_POSN+4], eax ; cursor position for video page 2 and 3
	;lodsd
	;mov	[CURSOR_POSN+8], eax ; cursor position for video page 4 and 5
	;lodsd
	;mov	[CURSOR_POSN+12], eax ; cursor position for video page 6 and 7
	mov	ch, 4
	mov	edi, CURSOR_POSN
bfn_rvs_8:
	movsd
	dec	ch
	jnz	short bfn_rvs_8
	; (font addr) protected mode address in kernel's/system memory space
	; (not accessable/meaningful address value by user)
	lodsd
	mov	[VGA_INT43H], eax ; VGA current (default) font address

	; (total 40 bytes are read&written above as BIOS data state)
bfn_rvs_9:
	; 13/01/2021
	test	cl, 4
	jz	short bfn_rvs_11

	;BX++;
        ;v = read_byte(ES, BX); BX++;
        ;outb(VGAREG_PEL_MASK, read_byte(ES, BX)); BX++;
        ;// Set the whole dac always, from 0
        ;outb(VGAREG_DAC_WRITE_ADDRESS,0x00);
        ;for(i=0;i<256*3;i++) {
        ;   outb(VGAREG_DAC_DATA, read_byte(ES, BX)); BX++;
        ;}
        ;BX++;
        ;outb(VGAREG_DAC_WRITE_ADDRESS, v);

	; /* read/write mode dac */
	lodsb	; skip ; VGAREG_DAC_STATE
	lodsb
	mov	ah, al ; * ; v
	lodsb
	mov	dx, VGAREG_PEL_MASK  ; 3C6h
	out	dx, al
	;// Set the whole dac always, from 0
	xor	al, al ; 0
	;mov	dx, VGAREG_DAC_WRITE_ADDRESS ; 3C8h
	mov	dl, 0C8h
	out	dx, al

	push	ecx ; 22/01/2021
	;for(i=0;i<256*3;i++) {
	;mov	ecx, 256*3 ; 768 bytes
	; 03/08/2022
	sub	ecx, ecx
	mov	ch, 3
	; ecx = 300h = 768
	;mov	dx, VGAREG_DAC_DATA ; 3C9h
	;mov	dl, 0C9h
	inc	dl ; dx = 3C9h
bfn_rvs_10:
	lodsb
	out	dx, al
	loop	bfn_rvs_10
	pop	ecx ; 22/01/2021

	; /* color select register */
	lodsb	 ; skip 

	mov	al, ah ; * ; v

	;mov	dx, VGAREG_DAC_WRITE_ADDRESS ; 3C8h
	;mov	dl, 0C8h
	dec	dl ; dx  = 3C8h
	out	dx, al ; * ; v

	; (total 772 bytes are read above as DAC state)
bfn_rvs_11:
	retn

vbe_biosfn_restore_video_state:
	; 23/01/2021
	; 13/01/2021 (TRDOS 386 v2.0.3)
	; (vbe.c)

	; modified registers: eax, edx, esi, ch
	
	; input: esi = state buffer address
	; output: 
	;	 VBE DISPI register contents will be restored
	;	 (18 bytes, 9 words)

 	; enable = read_word(ES, BX);
	; BX += 2;
	;
	; if (!(enable & VBE_DISPI_ENABLED)) {
        ;   outw(VBE_DISPI_IOPORT_INDEX,VBE_DISPI_INDEX_ENABLE);
        ;   outw(VBE_DISPI_IOPORT_DATA, enable);
	; } else {
        ;   outw(VBE_DISPI_IOPORT_INDEX, VBE_DISPI_INDEX_XRES);
        ;   outw(VBE_DISPI_IOPORT_DATA, read_word(ES, BX));
        ;   BX += 2;
        ;   outw(VBE_DISPI_IOPORT_INDEX, VBE_DISPI_INDEX_YRES);
        ;   outw(VBE_DISPI_IOPORT_DATA, read_word(ES, BX));
        ;   BX += 2;
        ;   outw(VBE_DISPI_IOPORT_INDEX, VBE_DISPI_INDEX_BPP);
        ;   outw(VBE_DISPI_IOPORT_DATA, read_word(ES, BX));
        ;   BX += 2;
        ;   outw(VBE_DISPI_IOPORT_INDEX,VBE_DISPI_INDEX_ENABLE);
        ;   outw(VBE_DISPI_IOPORT_DATA, enable);
	;
        ;  for(i = VBE_DISPI_INDEX_BANK; i <= VBE_DISPI_INDEX_Y_OFFSET; i++)
	;    {
        ;     outw(VBE_DISPI_IOPORT_INDEX, i);
        ;     outw(VBE_DISPI_IOPORT_DATA, read_word(ES, BX));
        ;     BX += 2;
        ;    }
        ; }

	lodsw	; enable (status, enabled=1, disabled=0)
	mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	; 23/01/2021
	and	ax, 1 ; VBE_DISPI_ENABLED
	jnz	short vbe_bfn_rvs_1
	; ax = 0
	; VBE_DISPI_DISABLED
vbe_bfn_rvs_0:
	; enable (disable) dispi
	; dx = 01CEh ; VBE_DISPI_IOPORT_INDEX
	; ah = 0
	push	eax
	mov	al, 04h	; VBE_DISPI_INDEX_ENABLE
	out	dx, ax
	;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	inc	dl
	pop	eax
	out	dx, ax ; enable (or disable)
	retn
vbe_bfn_rvs_1:
	; VBE_DISPI_ENABLED

	; from VBE_DISPI_INDEX_XRES
	;   to VBE_DISPI_INDEX_BPP

 	mov	ch, 3
	sub	al, al ; 0 ; VBE_DISPI_INDEX_XRES - 1
	; ax = 0

	call	vbe_bfn_rvs_2

        ;outw(VBE_DISPI_IOPORT_INDEX,VBE_DISPI_INDEX_ENABLE);
	;outw(VBE_DISPI_IOPORT_DATA, enable);

	; 23/01/2021
	mov	al, 1 ; VBE_DISPI_ENABLED
	; ax = 1
	call	vbe_bfn_rvs_0

	; from VBE_DISPI_INDEX_BANK
	;   to VBE_DISPI_INDEX_Y_OFFSET

	mov	ch, 5
	; 23/01/2021
	mov	al, 4  ; VBE_DISPI_INDEX_BANK - 1
	; ax = 4
vbe_bfn_rvs_2:
	inc	al ; from VBE_DISPI_INDEX_XRES
		     ; to VBE_DISPI_INDEX_BPP
	;mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	dl, 0CEh
	out	dx, ax
	push	eax
	;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	inc	dl ; 1CFh
	lodsw
	out	dx, ax
	pop	eax
	dec	dl ; 1CEh
	dec	ch
	jnz	short vbe_bfn_rvs_2
	retn

; ---------------------------------------------------------

dispi_set_enable:
	; 03/08/2022
	; 23/11/2020
	; Input:
	;	ax = VBE_DISPI_ENABLED = 1
	;	     or VBE_DISPI_DISABLED = 0
	;
	; Modified registers: none
	
	;push	edx
	;push	eax
	;mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	ax, 04h	  ; VBE_DISPI_INDEX_ENABLE
	;out	dx, ax
	;pop	eax
	;;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	;;mov	dl, 0CFh
	;inc	dl
	;out	dx, ax
	;pop	edx
	;retn

	; 25/11/2020
	; Modified registers: edx
	;;push	edx
	;mov	dx, 04h ; VBE_DISPI_INDEX_ENABLE
	; 03/08/2022
	sub	dh, dh
	mov	dl, 04h ; VBE_DISPI_INDEX_ENABLE

	;;call	dispi_set_parms
	;;pop	edx
	;;retn
	;jmp	short dispi_set_parms

dispi_set_parms:
	; 03/08/2022
	; 25/11/2020
	; Input:
	;	ax = data
	;	dx = vbe dispi register index
	;
	; Modified registers: edx

	push	eax
	;mov	ax, dx
	; 03/08/2022
	mov	eax, edx
	mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	out	dx, ax
	pop	eax
	;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	;mov	dl, 0CFh
	inc	dl
	out	dx, ax
	retn

dispi_set_bpp:
	; 03/08/2022
	; 25/11/2020
	; Input:
	;	ax = Bits per pixel value
	;	     (8,16,24,32)
	;
	; Modified registers: none

	;push	edx
	;push	eax
	;mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	ax, 03h	  ; VBE_DISPI_INDEX_BPP
	;out	dx, ax
	;pop	eax
	;;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	;;mov	dl, 0CFh
	;inc	dl
	;out	dx, ax
	;pop	edx
	;retn

	; 25/11/2020
	; Modified registers: edx
	;;push	edx
	;mov	dx, 03h ; VBE_DISPI_INDEX_BPP
	; 03/08/2022
	sub	dh, dh
	mov	dl, 03h ; VBE_DISPI_INDEX_BPP	

	;;call	dispi_set_parms
	;;pop	edx
	;;retn
	jmp	short dispi_set_parms

dispi_set_xres:
	; 03/08/2022
	; 25/11/2020
	; Input:
	;	ax = X resolution (screen witdh)
	;	     (320,640,800,1024,1280,1920)
	;
	; Modified registers: none

	;push	edx
	;push	eax
	;mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	ax, 01h	  ; VBE_DISPI_INDEX_XRES
	;out	dx, ax
	;pop	eax
	;;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	;;mov	dl, 0CFh
	;inc	dl
	;out	dx, ax
	;pop	edx
	;retn

	; 25/11/2020
	; Modified registers: edx
	;;push	edx
	;mov	dx, 01h ; VBE_DISPI_INDEX_XRES
	; 03/08/2022
	sub	dh, dh
	mov	dl, 01h ; VBE_DISPI_INDEX_XRES
	;;call	dispi_set_parms
	;;pop	edx
	;;retn
	jmp	short dispi_set_parms

dispi_set_yres:
	; 03/08/2022
	; 25/11/2020
	; Input:
	;	ax = Y resolution (screen height)
	;	     (200,400,600,720,768,1080)
	;
	; Modified registers: none

	;push	edx
	;push	eax
	;mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	ax, 02h	  ; VBE_DISPI_INDEX_YRES
	;out	dx, ax
	;pop	eax
	;;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	;;mov	dl, 0CFh
	;inc	dl
	;out	dx, ax
	;pop	edx
	;retn

	; 25/11/2020
	; Modified registers: edx
	;;push	edx
	;mov	dx, 02h ; VBE_DISPI_INDEX_YRES
	; 03/08/2022
	sub	dh, dh
	mov	dl, 02h ; VBE_DISPI_INDEX_YRES
	;;call	dispi_set_parms
	;;pop	edx
	;;retn
	jmp	short dispi_set_parms

dispi_set_bank:
	; 03/08/2022
	; 25/11/2020
	; Input:
	;	ax = video memory bank number
	;
	; Modified registers: none

	;push	edx
	;push	eax
	;mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	ax, 05h	  ; VBE_DISPI_INDEX_BANK
	;out	dx, ax
	;pop	eax
	;;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	;;mov	dl, 0CFh
	;inc	dl
	;out	dx, ax
	;pop	edx
	;retn

	; 25/11/2020
	; Modified registers: edx
	;;push	edx
	;mov	dx, 05h	; VBE_DISPI_INDEX_BANK
	; 03/08/2022
	sub	dh, dh
	mov	dl, 05h	; VBE_DISPI_INDEX_BANK
	;;call	dispi_set_parms
	;;pop	edx
	;;retn
	jmp	short dispi_set_parms

dispi_get_enable:
	; 03/08/2022
	; 27/11/2020
	; Input:
	;	none
	; Output:
	;	ax = vbe dispi status
	;
	; Modified registers: eax

	;push	edx
	;mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	ax, 04h	  ; VBE_DISPI_INDEX_ENABLE
	;out	dx, ax
	;;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	;;mov	dl, 0CFh
	;inc	dl
	;in	ax, dx
	;pop	edx
	;retn

	; 27/11/2020
	; Modified registers: eax, edx
	;;push	edx
	;mov	ax, 04h ; VBE_DISPI_INDEX_ENABLE
	; 03/08/2022
	sub	ah, ah
	mov	al, 04h ; VBE_DISPI_INDEX_ENABLE

	;;call	dispi_get_parms
	;;pop	edx
	;;retn
	;jmp	short dispi_get_parms

dispi_get_parms:
	; 25/11/2020
	; Input:
	;	ax = vbe dispi register index
	; output:
	;	ax = data
	;
	; Modified registers: eax, edx

	mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	out	dx, ax
	;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	;mov	dl, 0CFh
	inc	dl
	in	ax, dx
	retn

vga_compat_setup:
	; 03/08/2022
	; 26/11/2020
	; 25/11/2020
	; VGA compatibility setup
	; (vbe.c, 02/01/2020, vruppert)
	;
	; Input:
	;	none
	;
	; Modified registers: eax, edx

	; 26/11/2020
  	;push	eax
  	;push	edx

  	; set CRT X resolution
  	mov	dx, 1CEh ; VBE_DISPI_IOPORT_INDEX
  	;mov	ax, 01h  ; VBE_DISPI_INDEX_XRES
  	; 03/08/2022
	xor	eax, eax
	inc	al
	; eax = 1
	out	dx, ax
  	;mov	dx, 1CFh ; VBE_DISPI_IOPORT_DATA
  	inc	dl
	in	ax, dx
	push	eax
	mov	dx, 3D4h ; VGAREG_VGA_CRTC_ADDRESS
	mov	ax, 0011h ; Vertical retrace end register
  	out	dx, ax
  	;pop	eax
  	;push	eax
  	mov	eax, [esp]
	;shr	ax, 3 ; / 8 for pixel to character
	;dec	ax  ; - 1 (EGA or VGA?)
  	; 03/08/2022
	shr	eax, 3
  	dec	eax
	mov	ah, al
  	mov	al, 01h ; Horizontal display end register
  	out	dx, ax
  	pop	eax

	call	vga_set_virt_width

  	; set CRT Y resolution
	; 03/08/2022
  	mov	dx, 1CEh ; VBE_DISPI_IOPORT_INDEX
  	mov	ax, 02h  ; VBE_DISPI_INDEX_YRES
  	out	dx, ax
  	;mov	dx, 1CFh ; VBE_DISPI_IOPORT_DATA
	; 03/08/2022
  	inc	dl
	in	ax, dx
	push	eax
	mov	dx, 3D4h ; VGAREG_VGA_CRTC_ADDRESS
  	mov	ah, al
	mov	al, 12h ; Vertical display end register
  	out	dx, ax
  	pop	eax
  	mov	al, 07h	; Overflow register
  	out	dx, al
  	;inc	dx
  	; 03/08/2022
	inc	dl
	in	al, dx ; read overflow register
  	and	al, 0BDh ; clear VDE 9th and 10th bits
  	test	ah, 01h
  	jz	short bit8_clear
  	or	al, 02h ; VDE 9th bit (bit 8) in bit 1
bit8_clear:
  	test	ah, 02h
  	jz	short bit9_clear
  	or	al, 40h ; VDE 10th bit (bit 9) in bit 6
bit9_clear:
  	out	dx, al

  	; other settings
 	;mov	dx, 3D4h ; VGAREG_VGA_CRTC_ADDRESS
  	; 03/08/2022
	mov	dl, 0D4h
	mov	ax, 0009h ; Maximum scan line register
  	out	dx, ax	; Reset
  	mov	al, 17h ; Mode control register
  	out	dx, al
 	;mov	dx, 3D5h ; VGAREG_VGA_CRTC_DATA
  	inc	dl
	in	al, dx	; Read mode control register
  	or	al, 03h ; Set SRS and CMS bits
  	out	dx, al 
  	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
  	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx	 ; clear flip-flop
  	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
  	; 03/08/2022
	mov	dl, 0C0h
	mov	al, 10h	; Mode control register
  	out	dx, al
	;mov	dx, 3C1h ; VGAREG_ACTL_READ_DATA
	inc	dl
	in	al, dx
	or	al, 01h ; select graphics mode
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	dec	dl
	out	dx, al ; Write to mode control register
	mov	al, 20h ; Palette RAM <-> display memory
	out	dx, al ; Write to attribute addr register
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	; 03/08/2022
	mov	dl, 0CEh 
	mov	ax, 0506h ; Misc. register, graph, mm 1
	out	dx, ax
	;mov	dx, 3C4h ; VGAREG_SEQU_ADDRESS
	; 03/08/2022
	mov	dl, 0C4h
	mov	ax, 0F02h ; Map mask register, all planes
	out	dx, ax

  	; settings for >= 8bpp

	;mov	dx, 1CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	ax, 03h ; VBE_DISPI_INDEX_BPP
	;out	dx, ax
	;;mov	dx, 1CFh ; VBE_DISPI_IOPORT_DATA
	;inc	dl
	;in	ax, dx
	;cmp	al, 08h  ; < 8 bits per pixel
	;jb	short vga_compat_end

	;mov	dx, 3D4h ; VGAREG_VGA_CRTC_ADDRESS
	; 03/08/2022
	mov	dl, 0D4h
	mov	al, 14h  ; Underline location register
	out	dx, al
	;mov	dx, 3D5h ; VGAREG_VGA_CRTC_DATA
	inc	dl
	in	al, dx
	or	al, 40h	 ; enable double word mode
	out	dx, al
	;mov	dx, 3DAh ; VGAREG_ACTL_RESET
	; 03/08/2022
	mov	dl, 0DAh
	in	al, dx	 ; clear flip-flop
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	; 03/08/2022
	mov	dl, 0C0h
	mov	al, 10h	 ; Mode control register
	out	dx, al
	;mov	dx, 3C1h ; VGAREG_ACTL_READ_DATA
	inc	dl
	in	al, dx
	or	al, 40h  ; Pixel clock select is 1
	;mov	dx, 3C0h ; VGAREG_ACTL_ADDRESS
	dec	dl
	out	dx, al	 ; update mode control reggister
	mov	al, 20h	 ; select display memory as PAS	
	out	dx, al
	;mov	dx, 3C4h ; VGAREG_SEQU_ADDRESS
	; 03/08/2022
	mov	dl, 0C4h
	mov	al, 04h	; Memory mode register
	out	dx, al
	;mov	dx, 3C5h ; VGAREG_SEQU_DATA
	inc	dl
	in	al, dx
	or	al, 08h	; enable chain four
	out	dx, al
	;mov	dx, 3CEh ; VGAREG_GRDC_ADDRESS
	; 03/08/2022
	mov	dl, 0CEh
	mov	al, 05h	 ; Mode register
	out	dx, al
	;mov	dx, 3CFh ; VGAREG_GRDC_DATA
	inc	dl
	in	al, dx
	and	al, 9Fh	 ; clear shift register
	or	al, 40h  ; set shift register to 2
	out	dx, al

vga_compat_end:
  	;pop	edx
  	;pop	eax
	retn

vga_set_virt_width:
	; 03/08/2022
	; 27/11/2020
	; 25/11/2020
	; (vbe.c, 02/01/2020, vruppert)
	;
	; Input:
	;	AX = resolution (screen width)
	;
	; Modified registers: eax, edx

  	;;push	ebx
  	;push	edx
	;push	eax
  	;mov	ebx, eax
  	;call	dispi_get_bpp ; bits per pixel
	;cmp	al, 4
	;ja	short set_width_svga  ; 8, 16, 24, 32
	;shr	bx, 1
;set_width_svga:
	;shr	bx, 3
	;mov	eax, [esp]
	;shr	ax, 3	; / 8, bytes per row
	; 03/08/2022
	shr	eax, 3
	mov	dx, 3D4h ; VGAREG_VGA_CRTC_ADDRESS
	;mov	ah, bl	; 
	mov	ah, al	; width in bytes
	mov	al, 13h	; offset register
	out	dx, ax	; index (3D4h) and data (3D5h)
	;pop	eax
	;pop  	edx
	;;pop	ebx
	retn

; 24/11/2020

struc bmi ; BOCHS/PLEX86 MODE INFO structure/table 
 .mode:	  resw 1
 .width:  resw 1
 .height: resw 1
 .depth:  resw 1
 .size:	
endstruc

; 24/11/2020
struc MODEINFO
 .mode:			resw 1  ; 1XXh
 .ModeAttributes:	resw 1
 .WinAAttributes:	resb 1
 .WinBAttributes:	resb 1	; = 0
 .WinGranularity:	resw 1
 .WinSize:		resw 1
 .WinASegment:		resw 1
 .WinBSegment:		resw 1	; = 0
 .WinFuncPtr:		resd 1	; = 0
 .BytesPerScanLine:	resw 1
 .XResolution:		resw 1
 .YResolution:		resw 1
 .XCharSize:		resb 1
 .YCharSize:		resb 1
 .NumberOfPlanes: 	resb 1
 .BitsPerPixel:		resb 1
 .NumberOfBanks:	resb 1
 .MemoryModel:		resb 1
 .BankSize:		resb 1	; = 0
 .NumberOfImagePages: 	resb 1
 .Reserved_page:	resb 1	; = 0
 .RedMaskSize:		resb 1
 .RedFieldPosition: 	resb 1
 .GreenMaskSize:	resb 1
 .GreenFieldPosition: 	resb 1
 .BlueMaskSize:		resb 1
 .BlueFieldPosition: 	resb 1
 .RsvdMaskSize:		resb 1
 .RsvdFieldPosition: 	resb 1
 .DirectColorModeInfo: 	resb 1
 .PhysBasePtr:		resd 1
 .OffScreenMemOffset: 	resd 1	; = 0
 .OffScreenMemSize: 	resw 1	; = 0
 .LinBytesPerScanLine: 	resw 1
 .BnkNumberOfPages: 	resb 1
 .LinNumberOfPages: 	resb 1
 .LinRedMaskSize:	resb 1
 .LinRedFieldPosition1: resb 1
 .LinGreenMaskSize1: 	resb 1
 .LinGreenFieldPosition:resb 1
 .LinBlueMaskSize: 	resb 1
 .LinBlueFieldPosition:	resb 1
 .LinRsvdMaskSize: 	resb 1
 .LinRsvdFieldPosition:	resb 1
 .MaxPixelClock:	resd 1	; = 0
.size:
endstruc

; 10/12/2020
struc LFBINFO
 .mode:			resw 1  ; 1XXh
 .LFB_addr:		resd 1
 .LFB_size:		resd 1
 .X_res:		resw 1
 .Y_res:		resw 1
 .bpp:			resb 1
 .reserved:		resb 1
.size:	; 16 bytes
endstruc

set_mode_info_list:
	; 14/12/2020
	; 11/12/2020
	; 24/11/2020
	; (vbetables-gen.c)
	; Input:
	;	BX = VBE mode (including bochs special modes)
	; Output:
	;	;;EAX = MODE_INFO_LIST address
	;	EAX = 0 ; 11/12/2020
	;	ESI = MODE_INFO_LIST address ; 11/12/2020
	;	(if mode is not found, ESI = 0)
	;
	; Modified registers: eax, ebx, ecx, edx, esi, edi

	mov	esi, b_vbe_modes ; bochs mode info base table
	mov	edi, MODE_INFO_LIST ; mode info list (4F01h)
sml_0:
	lodsw
	cmp	ax, bx ; is mode number same ?
	je	short sml_1 ; yes
	lodsd
	lodsw
	cmp	esi, end_of_b_vbe_modes
	jb	short sml_0
	; not found
	xor	eax, eax ; 0
	; 11/12/2020
	xor	esi, esi
	retn
sml_1:
	stosw	; mode
	lodsd	; width, height
	; 14/12/2020
	mov	ecx, eax
	push	eax ; ***
	sub	eax, eax ; clear high word of eax
	lodsw	; depth
	push	eax ; **

	;add	al, 7 ; only for 15 bit colors (not used here)
	shr	al, 3 ; / 8
	; 14/12/2020
	mul	cx  ; pitch = width * ((depth+7)/8)
		; ax = pitch
	push	eax ; * ; high word of eax = 0
	shr	ecx, 16
	;mul	cx
	;mov	cx, ax
	xor	edx, edx  ; clear high word of edx
	mul	ecx ; height * pitch
	mov	ecx, eax
	mov	eax, VBE_DISPI_TOTAL_VIDEO_MEMORY_MB * 1024 * 1024
	div	ecx
		; eax = pages = vram_size / (height*pitch)

	;mov	cx, ax
	mov	ecx, eax ; pages 
		
	mov	ax, MODE_ATTRIBUTES
	stosw	; ModeAttributes
	mov	al, WINA_ATTRIBUTES
	stosb	; WinAAttributes
	xor	al, al ; WinBAttributes = 0
	stosb
	mov	ax, VBE_DISPI_BANK_SIZE_KB
	stosw	; WinGranularity
	stosw	; WinSize
	mov	ax, VGAMEM_GRAPH
	stosw	; WinASegment
	sub	eax, eax
	stosw	; WinBSegment = 0
	stosd	; WinFuncPtr = 0

	pop	eax ; * ; pitch
	mov	ebx, eax  ; high word of ebx = 0 ; 14/12/2020
	stosw	; BytesPerScanLine

	pop	edx ; ** ; depth (bits per pixel)
	pop	eax ; *** width, height

 	; // Mandatory information for VBE 1.2 and above

	stosw	; XResolution (width)
	shr	eax, 16
	push	eax ; **** height
	stosw	; YResolution (height)
	mov	al, 8
 	stosb	; XCharSize  ; char width
	mov	al, 16
	stosb	; YCharSize  ; char height
	mov	al, 1
	stosb	; NumberOfPlanes
	;movzx	eax, dl
	mov	al, dl ; eax <= 32
	stosb	; BitsPerPixel
	; Number of banks = (height * pitch + 65535) / 65536
	pop	eax ; **** ; height
	; 14/12/2020
	push	edx ; ***** ; depth ; edx <= 32
	mul	ebx  ; pitch (ebx) * height (eax)
	;mov	edx, [esp] ; *****
	;mov	dl, [esp] ; *****
	add	eax, 65535
	shr	eax, 16 ; / 65536 ; <= 127 ; 14/12/2020
	stosb	; NumberOfBanks
	; 14/12/2020
	;cmp	dl, 8 ; 8 bits per pixel
	cmp	byte [esp], 8
	ja	short sml_2
	mov	al, VBE_MEMORYMODEL_PACKED_PIXEL
	jmp	short sml_3
sml_2:
	; 16, 24, 32 bits per pixel
	mov	al, VBE_MEMORYMODEL_DIRECT_COLOR
sml_3:
	stosb
	xor	al, al ; 0
	stosb	; BankSize = 0
	dec	ecx ; pages - 1
	; NumberOfImagePages = 262 for 320x200x8 mode
	;;mov	ax, 255
	; 14/12/2020
	;mov	al, 255
	dec	al ; 255
	cmp	ecx, eax ; ecx <= 261, eax = 255
	;cmp	cx, ax
	jnb	short sml_4
	mov	al, cl
sml_4:
	stosb	; NumberOfImagePages (1 byte)
	sub	al, al
	stosb	; Reserved_page = 0
	pop	eax ; ***** ; depth
	mov	cl, al
	; eax < = 32
	sub	al, 8 ; 8->0, 16->8, 24->16, 32->24
	mov	esi, direct_color_fields
	add	esi, eax
	push	esi ; ******
	lodsd	; RedMaskSize (AL), RedFieldPosition (AH)
	     	; GreenMaskSize (16), GreenFieldPosition (24)
	stosd
	lodsd	; BlueMaskSize (AL), BlueFieldPosition (AH)
	     	; RsvdMaskSize (16), RsvdFieldPosition (24)
	stosd
	pop	esi ; ******

	xor	al, al ; 0
	cmp	cl, 32
	jb	short sml_5
	mov	al, VBE_DIRECTCOLOR_RESERVED_BITS_AVAILABLE
sml_5:
	stosb	; DirectColorModeInfo

	; // Mandatory information for VBE 2.0 and above

	mov	eax, VBE_DISPI_LFB_PHYSICAL_ADDRESS
	stosd	; PhysBasePtr
	sub	eax, eax
	stosd	; OffScreenMemOffset = 0
	stosw	; OffScreenMemSize = 0

	;// Mandatory information for VBE 3.0 and above

	; ebx = pitch
	mov	ax, bx
	;stosw

	;xor	al, al
     	;stosb	; BnkNumberOfPages = 0
     	;stosb	; LinNumberOfPages = 0

	stosd	; pitch (word), 0 (byte), 0 (byte)

	lodsd	; LinRedMaskSize (AL), LinRedFieldPosition (AH)
	     	; LinGreenMaskSize (16), LinGreenFieldPosition (24)
	stosd
	lodsd	; LinBlueMaskSize (AL), LinBlueFieldPosition (AH)
	     	; LinRsvdMaskSize (16), LinRsvdFieldPosition (24)
	stosd

	sub	eax, eax
	stosd	; MaxPixelClock = 0

	;mov	eax, MODE_INFO_LIST
	; 11/12/2020
	mov	esi, MODE_INFO_LIST

	retn

; end of set_mode_info_list ; edi = set_mode_info_list + 68

pci_get_lfb_addr:
	; 11/12/2020
	; Get linear frame buffer base from PCI
	; (vgabios.c, 02/01/2020, vruppert)
	;
	; Input:
	;	ax = PCI device vendor id
	; Output:
	;	ax  = LFB address (high 16 bit) (zf=0)
	;	eax = 0 -> not found (error) (zf=1)
	;
	; Modified registers: eax

	push	ebx
	push	ecx
	push	edx
	;
	mov	ebx, eax
	xor	ecx, ecx
	sub	dl, dl	; mov dl, 0
	call	pci_read_reg
	cmp	ax, 0FFFFh
	je	short pci_get_lfb_addr_fail
pci_get_lfb_addr_next_dev:
	sub	dl, dl ; mov dl, 0
	call	pci_read_reg
	cmp	ax, bx	; check vendor
	je	short pci_get_lfb_addr_found
	add	cx, 08h
	cmp	cx, 200h ; search bus 0 and 1
	jb	short pci_get_lfb_addr_next_dev
pci_get_lfb_addr_fail:
	xor	eax, eax ; no LFB
	; zf = 1
	jmp	short pci_get_lfb_addr_return
pci_get_lfb_addr_found:
	mov	dl, 10h	; I/O space 0
	call	pci_read_reg
	test	ax, 0FFF1h
	jz	short pci_get_lfb_addr_success
	mov	dl, 14h ; I/O space 1
	call	pci_read_reg
	test	ax, 0FFF1h
	jnz	short pci_get_lfb_addr_fail
pci_get_lfb_addr_success:
	shr	eax, 16 ; LFB address (hw)
	; zf = 0
pci_get_lfb_addr_return:
	pop	edx
	pop	ecx
	pop	ebx
	retn

pci_read_reg:
	; 11/12/2020
	; Read PCI register
	; (vgabios.c, 02/01/2020, vruppert)
	;
	; Input:
	;	cx = device/function
	;	dl = register
	; Output:
	;	eax = value
	;
	; Modified registers: eax, edx

	mov	eax, 00800000h
	mov	ax, cx
	shl	eax, 8
	mov	al, dl
	mov	dx, 0CF8h
 	out	dx, eax
	add	dl, 4 ; mov dx, 0CFCh
	in	eax, dx
	retn

%endif

; -----------

%if 0

mode_info_find_mode:
	; 25/11/2020
	; Input:
	;	bx = VESA VBE2 video mode (+ bochs extensions)
	; Output:
	;	esi = mode info address (for BX input)
	;	esi = 0 -> not found
	;
	; Modified registers: eax, esi

	xor	eax, eax
	mov	esi, MODE_INFO_LIST
mifm_1:
	mov	ax, [esi]
	cmp	ax, bx
	je	short mifm_2
	add	esi, MODEINFO.size ; add esi, 68
	cmp	esi, VBE_VESA_MODE_END_OF_LIST
	jb	short mifm_1
	; not found
	sub	esi, esi ; 0
mifm_2
	retn

dispi_get_bpp:
	; 28/11/2020
	; Input:
	;	none
	; Output:
	;	al = Bits per pixel
	;	     (8,16,24,32)
	;	ah = Bytes per pixel
	;	     (1,2,3,4)	
	;
	; Modified registers: none

	;push	edx
	;mov	dx, 01CEh ; VBE_DISPI_IOPORT_INDEX
	;mov	ax, 03h	  ; VBE_DISPI_INDEX_BPP
	;out	dx, ax
	;;mov	dx, 01CFh ; VBE_DISPI_IOPORT_DATA
	;;mov	dl, 0CFh
	;inc	dl
	;in	ax, dx
	;mov	ah, al
	;shr	ah, 3 ; / 8
	;;test	al, 7 ; 15 bit graphics mode
 	;;jz	short get_bpp_noinc
	;;inc	ah
;;get_bpp_noinc:
	;pop	edx
	;retn

	; 28/11/2020
	; Modified registers: edx
	;push	edx
	mov	dx, 03h ; VBE_DISPI_INDEX_BPP
	call	dispi_get_parms
	;pop	edx
	;retn
	mov	ah, al
	shr	ah, 3 ; / 8
	;test	al, 7 ; 15 bit graphips mode
 	;jz	short get_bpp_noinc
	;inc	ah
;get_bpp_noinc:
	;pop	edx
	retn

restore_vesa_video_state:
	; 02/08/2022 (TRDOS 386 v2.0.5)
	; 14/01/2021
	; 06/12/2020
	; Input:
	;	[vbe3stbufsize] <= 32 ; <= 32*64 bytes
	; Output:
	;	AX = 004Fh (successed)
	;
	;	eax = 0 -> buffer size problem
	;	eax > 0 and ax <> 004Fh -> failed
	;
	; Modified regs: eax, ebx, ecx, edx, esi, edi

	;movzx	ecx, word [vbe3stbufsize]
	;cmp	cx, 32 ; 32 * 64 bytes
	;ja	short r_v_b_s_fail

	movzx	ecx, byte [vbe3stbufsize]; <=32
	;shl	cx, 4 ; dword count for movsd
	; 02/08/2022
	shl	ecx, 4 ; <= 32*16 dwords (32*64 bytes)
	mov	edi, VBE3SAVERESTOREBLOCK ; destination
					; (vbe3 pmi buff)
	mov	esi, VBE3VIDEOSTATE ; source (kernel buff)
	rep	movsd

	mov	ax, 4F04h
	mov	dl, 02h ; restore
	;mov	cx, 0Fh
	mov	cl, 0Fh
	xor 	ebx, ebx ; points to VBE3SAVERESTOREBLOCK
	jmp	short int10h_32bit_pmi

;s_v_b_s_fail:
;r_v_b_s_fail:
;	xor	eax, eax
;	retn

save_vesa_video_state:
	; 02/08/2022 (TRDOS 386 v2.0.5)
	; 14/01/2021
	; 06/12/2020
	; Input:
	;	[vbe3stbufsize] <= 32 ; <= 32*64 bytes
	; Output:
	;	AX = 004Fh (successed)
	;
	;	eax = 0 -> buffer size problem
	;	eax > 0 and ax <> 004Fh -> failed
	;
	; Modified regs: eax, ebx, ecx, edx, esi, edi

	;cmp	word [vbe33stbufsize], 32  
	;			; 32 * 64 bytes
	;ja	short s_v_b_s_fail

	mov	ax, 4F04h
	mov	dl, 01h ; save
	;mov	cx, 0Fh
	mov	cl, 0Fh
	xor 	ebx, ebx ; points to VBE3SAVERESTOREBLOCK

	call	int10h_32bit_pmi

	movzx	ecx, byte [vbe3stbufsize]; <=32	
	;shl	cx, 4 ; dword count for movsd
	; 02/08/2022
	shl	ecx, 4 ; <= 32*16 dwords (32*64 bytes)
	mov	esi, VBE3SAVERESTOREBLOCK ; destination
					; (vbe3 pmi buff)
	mov	edi, VBE3VIDEOSTATE ; source (kernel buff)
	rep	movsd
	retn

dispi_set_bank_farcall:
	; 03/08/2022
	; 12/04/2021 (32 bit push/pop)
	; 11/12/2020
	; (This may be 'sysvideo' function, later)
	;
	; Input: 
	;	bx = 0000h, set bank number
	;	   = 0100h, get bank number
	;	dx = bank number (if bx = 0)
	; Output:
	;	dx = bank number

	cmp	bx, 0100h
	je	short dispi_set_bank_farcall_get
	or	bx, bx
	;jnz	dispi_set_bank_farcall_error
	; 03/08/2022
	jz	short dsbfcall_1
	jmp	dispi_set_bank_farcall_error
dsbfcall_1:
	mov	ax, dx
	;push	dx
	;push	ax
	; 12/04/2021
	push	edx
	push	eax
	mov	ax, VBE_DISPI_INDEX_BANK
	mov	dx, 1CEh ; VBE_DISPI_IOPORT_INDEX
	out	dx, ax
	;pop	ax
	; 12/04/2021
	pop	eax
	;;mov	dx, VBE_DISPI_IOPORT_DATA
	;mov	dl, 0CFh
	inc	dl ; 1CFh = VBE_DISPI_IOPORT_DATA
	out	dx, ax
	in	ax, dx
	;pop	dx
	; 12/04/2021
	pop	edx
	cmp	dx, ax
	jne	short dispi_set_bank_farcall_error
	mov	ax, 004Fh
	retn	; retf for real mode far call
dispi_set_bank_farcall_get:
	mov	ax, VBE_DISPI_INDEX_BANK
	; 03/08/2022
	mov	dx, 1CEh ; VBE_DISPI_IOPORT_INDEX
	out	dx, ax
	;;mov	dx, VBE_DISPI_IOPORT_DATA
	;mov	dl, 0CFh
	; 03/08/2022
	inc	dl ; 1CFh = VBE_DISPI_IOPORT_DATA
	in	ax, dx
	mov	dx, ax
	retn	; retf for real mode far call
dispi_set_bank_farcall_error:
	mov	ax, 014Fh
	retn	; retf for real mode far call

%endif

; % include 'vidata.s' ; VIDEO DATA

; /// End Of VIDEO FUNCTIONS ///