; ****************************************************************************
; argon.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'argon.prg')
; ----------------------------------------------------------------------------
; ARGON.PRG ! TEST program !  TRDOS 386 VGA Functionality test !
;
; 14/08/2016
;
; [ Last Modification: 15/08/2016 ]
;
; Derived from disassembly of 'ARGON.COM' (ARGON.ASM, 1997) demo by Insomniac
;
; Assembler: NASM 2.11
; 
; (Original -msdos- code has been modifed for TRDOS 386 system calls and
; other protected mode (TRDOS 386) interrupts.)
; ****************************************************************************
; ARGON.ASM
;                    FULL SOURCE-CODE TO THE 128-BYTE DEMO
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                      Copyright (C) 1997 Matrix Software
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;                                code .Insomniac
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

; NOTE: Assembly source code of ARGON.COM (ARGON.ASM by Insomniac, 1997)
; has been modified to NASM syntax for 386 protected mode & for TRDOS 386
; by Erdogan Tan. (14/08/2016)

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
	mov	al, 13h	; set video mode to 13h 
	;int	10h	; - VIDEO -
	int	31h  	; TRDOS 386 - VIDEO Interrupt

	; CALCULATE SINE/COS - VALUES (Wally/Rage)

	mov	cx, 783Fh	; "don't ask me, I just use it.."
	;xor	si, si
	xor	bh, bh
plc1:	mov	ax, 65497
	imul	cx
	add	si, dx
	add	cx, si
	mov	[ebx], ch
	sar	byte [ebx], 1
	dec	bx
	cmp	bx, 16383
	jne	short plc1

	inc	ebx		; BX points to the sine/cos-values

; DRAW THE PLASMA-CURVES (Blue,Red)

	mov	edi, 0A0000h	; "the beginning is a good place to start ;)"
	mov	esi, edi
	xor	cx, cx
	mov	dx, 380
plc2:	mov	[edi], ch
	mov	[edi+380], ch
	mov	si, 381
	sub	si, di
	mov	[esi], ch
	mov	[esi+380], ch
	add	di, 3
	inc	ch
	cmp	ch, 64			; "are we crossing eachother?"
	jne	short plc2

	mov	esi, 0A0000h		; "set a nice palette"
	mov	dx, 3C8h
	;out	dx, al
	mov	ah, 1 ; out (byte)
	; al = data byte
	; dx = port number
	int	34h ; TRDOS 386 - IOCTL 

	inc	dx
	mov	cx, 256*3		; R,G,B - values
	;rep	outsb
	mov	ah, 1 ; out (byte)
pcl7:
	lodsb
	; al = data byte
	; dx = port number
	int	34h ; TRDOS 386 - IOCTL
	dec	cx
	jnz	short pcl7
 
plc3:	inc	dx
	mov	cl, 200			; "yes Max-Y = 200.."
	mov	edi, 0A0000h	

plc4:	mov	bl, cl
	add	bl, dl
	mov	dh, [ebx+_end]
	xor	ch, ch
	mov	bl, dh

plc5:	mov	al, [ebx+_end]
	sub	al, cl
	;mov	ah, al
	cbw
	add	ah, dh
	sub	ax, cx
	test	cl, 1
	jz	short plc6

	xchg	ah, al
plc6:	stosw			; "Use this since we have overlapping"
	inc	bl
	inc	ch
	cmp	ch, 160		; "since we use STOSW, we only need 320/2"
	jb	short plc5
	dec	cl
	jnz	short plc4

	mov	ah, 1
	;int	16h		; KEYBOARD - CHECK BUFFER, DO NOT CLEAR
				; Return: ZF clear if character	in buffer
				; AH = scan code, AL = character
				; ZF set if no character in buffer
	int	32h	; TRDOS 386 - KEYBOARD Interrupt
	jz      short plc3

	mov	ax, 3
	;int	10h		; - VIDEO - SET	VIDEO MODE
				; AL = mode
	int	31h  	; TRDOS 386 - VIDEO Interrupt	

terminate:
	sys 	_exit   ; INT 40h
here:
	jmp	short here

_end: