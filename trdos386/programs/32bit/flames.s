; ****************************************************************************
; flames.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'flames.prg')
; ----------------------------------------------------------------------------
; FLAMES.PRG ! TEST program !  TRDOS 386 VGA Functionality test !
;
; 15/08/2016
;
; [ Last Modification: 17/08/2016 ]
;
; Derived from disassembly of 'flames.com' file (15099 bytes)
;
; Assembler: NASM 2.11

; Original code disassembler: IDA Pro Free (MASM syntax)
; 
; (Original -msdos- code has been modifed for TRDOS 386 system calls and
; other protected mode (TRDOS 386) interrupts.)
; ****************************************************************************

; NOTE: Assembly source code of FLAMES.COM (by IDA, in MASM syntax)
; has been modified to NASM syntax (for 386 protected mode & for TRDOS 386)
; by Erdogan Tan. (14/08/2016)

; 19/05/2016
; 29/04/2016
; TRDOS 386 system calls (temporary list!)
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_link 	equ 9
_unlink	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_chown	equ 16
_break	equ 17
_stat	equ 18
_seek	equ 19
_tell 	equ 20
_mount	equ 21
_umount	equ 22
_setuid	equ 23
_getuid	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_fstat	equ 28
_emt 	equ 29
_mdate 	equ 30
_video	equ 31
_audio	equ 32
_ilgins	equ 33
_sleep	equ 34
_msg    equ 35
_geterr equ 36
_rsrvd1	equ 37
_pri	equ 38
_rele 	equ 39

%macro sys 1-4
    ; 29/04/2016 - TRDOS 386 (TRDOS v2.0)	
    ; 03/09/2015	
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.	
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3    
            mov ecx, %3
            %if %0 = 4
               mov edx, %4   
            %endif
        %endif
    %endif
    mov eax, %1
    ;int 30h
    int 40h ; TRDOS 386 (TRDOS v2.0)	   
%endmacro

; TRDOS 386 (and Retro UNIX 386 v1) system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; 80386 Protected Mode (32 bit) intructions

[ORG 0] 

start:
		; DIRECT VGA MEMORY ACCESS
		;xor	ebx, ebx
		mov	bh, 5 ; Direct access/map to VGA memory (0A0000h)
		;mov	eax, _video ; 1Fh
		mov	al, 1Fh ; sys _video ; TRDOS 386 Video functions
		int	40h   ; TRDOS 386 system call

		; eax = 0A0000h
		and	eax, eax
		jz      terminate ; error (eax = 0)
		
		; ah = 0
		mov	al, 13h		; set video mode to 13h 
		;int	10h		; - VIDEO - SET	VIDEO MODE
		int	31h  	; TRDOS 386 - VIDEO Interrupt

		;mov	dx, 3C4h
		;mov	ax, 604h
		;out	dx, ax		; EGA: sequencer address reg
					; unknown register

		; TRDOS 386 - IOCTL Interrupt (for ring 3)
		mov	bx, 604h
		mov	ah, 3 ; out (word)
		mov	dx, 3C4h
		; bx = data word
		; dx = port number
		int	34h ; TRDOS 386 - IOCTL interrupt

		;mov	dx, 3C4h
		;mov	ax, 0F02h
		;out	dx, ax		; EGA: sequencer address reg
					; unknown register
		mov	bx, 0F02h
		mov	ah, 3 ; out (word)
		;mov	dx, 3C4h
		; bx = data word
		; dx = port number
		int	34h ; TRDOS 386 - IOCTL interrupt

		mov	edi, 0A0000h
		xor	eax, eax
		mov	ecx, 8000h
		cld
		rep stosw

		;mov	dx, 3D4h
		;mov	ax, 14h
		;out	dx, ax		; Video: CRT cntrlr addr
					; underline location.  Bits 0-5	are scan line number.
		mov	dx, 3D4h
		mov	bx, 14h
		mov	ah, 3 ; out (word)
		; bx = data word
		; dx = port number
		int	34h ; TRDOS 386 - IOCTL interrupt

		;mov	ax, 0E317h
		;out	dx, ax		; Video: CRT cntrlr addr
					;
		mov	bx, 0E317h
		mov	ah, 3 ; out (word)
		; bx = data word
		; dx = port number
		int	34h ; TRDOS 386 - IOCTL interrupt

		;mov	ax, 409h
		;out	dx, ax		; Video: CRT cntrlr addr
					;
		mov	bx, 409h
		mov	ah, 3 ; out (word)
		; bx = data word
		; dx = port number
		int	34h ; TRDOS 386 - IOCTL interrupt

		call	sub_101DF

		;lea	si, [unk_138FB]
		;mov	cx, 300h
		mov	dx, 3C8h
		xor	al, al
		;out	dx, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		inc	dx  ; 3C9h
		;cld
		;rep outsb

		mov	cx, 300h		
		lea	si, [unk_138FB]
outsb_1:
		lodsb
		;mov	ah, 1 ; out (byte)
		; al = data byte
		; dx = port number
		int	34h ; TRDOS 386 - IOCTL interrupt
		
		dec	cx
		jnz	short outsb_1


		;lea	di, [unk_101F9]
		;mov	cx, 1B80h
		;xor	ax, ax
		;rep stosw

		mov	bx, 4
		;xor	ebp, ebp
loc_1015E:				; CODE XREF: start+D0j	start+D6j
		call	sub_101DF
		lea	si, [unk_101F9]
		mov	edi, 0A0000h
		mov	cx, 0C80h

loc_1016A:				; CODE XREF: start+73j
		lodsw
		mov	dl, al
		lodsw
		mov	dh, al
		mov	ax, dx
		stosw
		loop	loc_1016A
		mov	cx, 1AE0h
		lea	si, [unk_10299]

loc_1017C:				; CODE XREF: start+98j
		mov	ax, [si-2]
		add	ax, [si]
		add	ax, [si+2]
		add	ax, [si+0A0h]
		sub	ax, bx
		sar	ax, 2
		jge	short loc_10191
		xor	ax, ax

loc_10191:				; CODE XREF: start+8Dj
		mov	[si-0A0h], ax
		add	si, 2
		loop	loc_1017C
		lea	si, [unk_137B9]
		mov	cx, 50h
		xor	dx, dx

loc_101A3:				; CODE XREF: start+BBj
		call	sub_101ED
		test	ax, ax
		js	short loc_101B2
		call	sub_101ED
		mov	dl, al
		and	dl, 7Fh

loc_101B2:				; CODE XREF: start+A8j
		mov	[si], dx
		mov	[si+0A0h], dx
		add	si, 2
		loop	loc_101A3
		mov	ah, 1
		;int	16h		; KEYBOARD - CHECK BUFFER, DO NOT CLEAR
					; Return: ZF clear if character	in buffer
					; AH = scan code, AL = character
					; ZF set if no character in buffer
		int	32h	; TRDOS 386 - KEYBOARD Interrupt
		jz	short loc_101CC
		mov	ah, 0
		;int	16h		; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
					; Return: AH = scan code, AL = character
		int	32h	; TRDOS 386 - KEYBOARD Interrupt

		mov	bp, 0DACh
		jmp	short loc_101D2

; ---------------------------------------------------------------------------

loc_101CC:				; CODE XREF: start+C1j
		cmp	bp, 0DACh
		jb	loc_1015E

loc_101D2:				; CODE XREF: start+CAj
		inc	bx
		cmp	bx, 64h
		jbe	loc_1015E
		mov	ax, 3
		;int	10h		; - VIDEO - SET	VIDEO MODE
					; AL = mode
		;int	20h		; DOS -	PROGRAM	TERMINATION

		int	31h  	; TRDOS 386 - VIDEO Interrupt	

terminate:
		sys 	_exit   ; INT 40h
here:
		jmp	short here

; --------------- S U B	R O U T	I N E ---------------------------------------

sub_101DF:				; CODE XREF: start+31p
					; start:loc_1015Ep
		mov	dx, 3DAh
		mov	ah, 0 ; in (byte)

loc_101E2:				; CODE XREF: sub_101DF+6j
		;in	al, dx		; Video	status bits:
					; 0: retrace.  1=display is in vert or horiz retrace.
					; 1: 1=light pen is triggered; 0=armed
					; 2: 1=light pen switch	is open; 0=closed
					; 3: 1=vertical	sync pulse is occurring.
		; al = data byte
		; dx = port number
		int	34h ; TRDOS 386 - IOCTL interrupt

		test	al, 8
		jz	short loc_101E2

loc_101E7:				; CODE XREF: sub_101DF+Bj
		;in	al, dx		; Video	status bits:
					; 0: retrace.  1=display is in vert or horiz retrace.
					; 1: 1=light pen is triggered; 0=armed
					; 2: 1=light pen switch	is open; 0=closed
					; 3: 1=vertical	sync pulse is occurring.

		int	34h ; TRDOS 386 - IOCTL interrupt

		test	al, 8
		jnz	short loc_101E7
		retn

; --------------- S U B	R O U T	I N E ---------------------------------------

sub_101ED:				; CODE XREF: start:loc_101A3p
					; start+AAp
		mov	ax, [word_138F9]
		imul	ax, 8905h
		inc	ax
		mov	[word_138F9], ax
		retn

; ---------------- DATA -----------------------------------------------------

word_138F9:	dw 1234h		; DATA XREF: sub_101EDr sub_101ED+8w
unk_138FB:	db    0			; DATA XREF: start+34o
		db    0
		db    0
		db    0
		db    0
		db    2
		db    0
		db    0
		db    4
		db    0
		db    0
		db    6
		db    0
		db    0
		db    8
		db    0
		db    0
		db  0Ah
		db    0
		db    0
		db  0Ch
		db    0
		db    0
		db  0Eh
		db    0
		db    0
		db  10h
		db    2
		db    0
		db  0Eh
		db    4
		db    0
		db  0Ch
		db    6
		db    0
		db  0Ah
		db    8
		db    0
		db    8
		db  0Ah
		db    0
		db    6
		db  0Ch
		db    0
		db    4
		db  0Eh
		db    0
		db    2
		db  10h
		db    0
		db    0
		db  11h
		db    0
		db    0
		db  13h
		db    0
		db    0
		db  15h
		db    0
		db    0
		db  17h
		db    0
		db    0
		db  19h
		db    0
		db    0
		db  1Bh
		db    0
		db    0
		db  1Dh
		db    0
		db    0
		db  1Fh
		db    0
		db    0
		db  21h	; !
		db    0
		db    0
		db  23h	; #
		db    0
		db    0
		db  25h	; %
		db    0
		db    0
		db  27h	; '
		db    0
		db    0
		db  29h	; )
		db    0
		db    0
		db  2Bh	; +
		db    0
		db    0
		db  2Dh	; -
		db    0
		db    0
		db  2Fh	; /
		db    0
		db    0
		db  31h	; 1
		db    0
		db    0
		db  33h	; 3
		db    0
		db    0
		db  35h	; 5
		db    0
		db    0
		db  37h	; 7
		db    0
		db    0
		db  39h	; 9
		db    0
		db    0
		db  3Bh	; ;
		db    0
		db    0
		db  3Dh	; =
		db    0
		db    0
		db  3Fh	; ?
		db    0
		db    0
		db  3Fh	; ?
		db    1
		db    0
		db  3Fh	; ?
		db    3
		db    0
		db  3Fh	; ?
		db    5
		db    0
		db  3Fh	; ?
		db    7
		db    0
		db  3Fh	; ?
		db    9
		db    0
		db  3Fh	; ?
		db  0Bh
		db    0
		db  3Fh	; ?
		db  0Dh
		db    0
		db  3Fh	; ?
		db  0Fh
		db    0
		db  3Fh	; ?
		db  11h
		db    0
		db  3Fh	; ?
		db  13h
		db    0
		db  3Fh	; ?
		db  15h
		db    0
		db  3Fh	; ?
		db  17h
		db    0
		db  3Fh	; ?
		db  19h
		db    0
		db  3Fh	; ?
		db  1Bh
		db    0
		db  3Fh	; ?
		db  1Dh
		db    0
		db  3Fh	; ?
		db  1Fh
		db    0
		db  3Fh	; ?
		db  20h
		db    0
		db  3Fh	; ?
		db  22h	; "
		db    0
		db  3Fh	; ?
		db  24h	; $
		db    0
		db  3Fh	; ?
		db  26h	; &
		db    0
		db  3Fh	; ?
		db  28h	; (
		db    0
		db  3Fh	; ?
		db  2Ah	; *
		db    0
		db  3Fh	; ?
		db  2Ch	; ,
		db    0
		db  3Fh	; ?
		db  2Eh	; .
		db    0
		db  3Fh	; ?
		db  30h	; 0
		db    0
		db  3Fh	; ?
		db  32h	; 2
		db    0
		db  3Fh	; ?
		db  34h	; 4
		db    0
		db  3Fh	; ?
		db  36h	; 6
		db    0
		db  3Fh	; ?
		db  38h	; 8
		db    0
		db  3Fh	; ?
		db  3Ah	; :
		db    0
		db  3Fh	; ?
		db  3Ch	; <
		db    0
		db  3Fh	; ?
		db  3Fh	; ?
		db    0
		db  3Fh	; ?
		db  3Fh	; ?
		db    2
		db  3Fh	; ?
		db  3Fh	; ?
		db    5
		db  3Fh	; ?
		db  3Fh	; ?
		db    7
		db  3Fh	; ?
		db  3Fh	; ?
		db  0Ah
		db  3Fh	; ?
		db  3Fh	; ?
		db  0Dh
		db  3Fh	; ?
		db  3Fh	; ?
		db  0Fh
		db  3Fh	; ?
		db  3Fh	; ?
		db  12h
		db  3Fh	; ?
		db  3Fh	; ?
		db  15h
		db  3Fh	; ?
		db  3Fh	; ?
		db  17h
		db  3Fh	; ?
		db  3Fh	; ?
		db  1Ah
		db  3Fh	; ?
		db  3Fh	; ?
		db  1Ch
		db  3Fh	; ?
		db  3Fh	; ?
		db  1Fh
		db  3Fh	; ?
		db  3Fh	; ?
		db  22h	; "
		db  3Fh	; ?
		db  3Fh	; ?
		db  24h	; $
		db  3Fh	; ?
		db  3Fh	; ?
		db  27h	; '
		db  3Fh	; ?
		db  3Fh	; ?
		db  2Ah	; *
		db  3Fh	; ?
		db  3Fh	; ?
		db  2Ch	; ,
		db  3Fh	; ?
		db  3Fh	; ?
		db  2Fh	; /
		db  3Fh	; ?
		db  3Fh	; ?
		db  31h	; 1
		db  3Fh	; ?
		db  3Fh	; ?
		db  34h	; 4
		db  3Fh	; ?
		db  3Fh	; ?
		db  37h	; 7
		db  3Fh	; ?
		db  3Fh	; ?
		db  39h	; 9
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Ch	; <
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?
		db  3Fh	; ?

bss_start:

ABSOLUTE bss_start

alignb 2

unk_101F9:
		resb  160 
unk_10299:
		resb  13600
unk_137B9:	
		resb  320

_bss_end: