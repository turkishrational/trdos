; ****************************************************************************
; m13y60hz.s (Mode 13h .. 320x200, 256 colors, 60hz test for TRDOS 386)
; ----------------------------------------------------------------------------
; M13Y60HZ.PRG ! MODE 13h 60 Hz (for LCD monitor) Test program by Erdogan TAN
;
; 07/12/2023
;
; Assembler: NASM 2.15 (2.11)
; ----------------------------------------------------------------------------
;	   nasm  m13y60hz.s -l m13y60hz.txt -o M13Y60HZ.PRG	
; ****************************************************************************

;------------------------------------------------------------------------------
; TRDOS 386, TRDOS v2.0
;------------------------------------------------------------------------------

; 14/07/2020
; 31/12/2017
; TRDOS 386 (v2.0) system calls
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_create	equ 8
_rename	equ 9
_delete	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_rmdir	equ 16
_break	equ 17
_drive	equ 18
_seek	equ 19
_tell 	equ 20
_memory	equ 21
_prompt	equ 22
_path	equ 23
_env	equ 24
_stime	equ 25
_quit	equ 26
_intr	equ 27
_dir	equ 28
_emt 	equ 29
_ldrvt 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpstat	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45		

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

;------------------------------------------------------------------------------
; CODE
;------------------------------------------------------------------------------

[BITS 32]
[org 0]

Start:
	; set videdo mode 13h (320x200, 256 colors)
	mov	al, 13h
	int	31h

	; DIRECT VGA MEMORY ACCESS
	; bl = 0, bh = 5
	; Direct access/map to VGA memory (0A0000h)

	sys	_video, 0500h
	
	cmp	eax, 0A0000h
	je	short _x
	jmp	ENDPROG
_x:
	xor	eax, eax
	xor	ebx, ebx

	mov	edx, 3CCh
	;in	al, dx
	;mov	ah, 0
	int	34h
	mov	dl, 0C2h	; misc. output (3C2h)
	or	al, 0C0h
	;out	dx, al
	mov	ah, 1
	int	34h

	mov	dx, 3D4h	; CRTC port
	
	mov	al, 11h
	mov	ah, 1
	int	34h
	inc	edx
	mov	ah, 0
	int	34h
	and	al, 7Fh
	mov	ah, 1
	int	34h		; unlock cr0 - cr7
	dec	edx

	;mov	ebx, 0011h	; Turn off write protect to CRTC registers
	;;mov	ax, bx
	;;out	dx, ax
	;mov	ah, 3
	;int	34h
	
	;mov	dx, 3D4h
	;mov	bx, 0B06h	; New vertical total=525 lines, bits 0-7
	mov	bx, 0B06h	; vertical total
	;mov	ax, bx
	;out	dx, ax
	mov	ah, 3
	int	34h

	;mov	dx, 3D4h
	;mov	bx, 3E07h	; New vertical total=525 lines, bits 8-9
	mov	bx, 3E07h	; overflow reg
	;mov	ax, bx
	;out	dx, ax
	mov	ah, 3
	int	34h

	;mov	dx, 3D4h
	;mov	bx, 0B910h	; Vsync start=scanline 185
	mov	bx, 0C310h	; vertical retrace start
	;mov	ax, bx
	;out	dx, ax
	mov	ah, 3
	int	34h

	mov	bx, 8C11h	; vertical retrace end
	mov	ah, 3
	int	34h

	;mov	dx, 3D4h
	;mov	bx, 8F12h	; Vertical display end=scanline 399, bits 0-7
	mov	bx, 8F12h	; vertical display enable end
	;mov	ax, bx
	;out	dx, ax
	mov	ah, 3
	int	34h

	;mov	dx, 3D4h
	;mov	bx, 0B815h	; vertical blanking start=scanline 440, bits 0-7
	mov	bx, 0B815h	; vertical blank start
	;mov	ax, bx
	;out	dx, ax
	mov	ah, 3
	int	34h

	;mov	dx, 3D4h
	;mov	bx, 0E216h	; Adjust vblank end position
	mov	bx, 0E216h	; vertical blank end
	;mov	ax, bx
	;out	dx, ax
	mov	ah, 3
	int	34h

	;mov	dx, 3D4h
	mov	bx, 8511h	; Vsync length=2 lines + turn write protect back on
	;mov	ax, bx
	;out	dx, ax
	mov	ah, 3
	int	34h

	; fill screen with VGA palette
	mov	edi, 0A0000h
	xor	esi, esi
	dec	esi
	mov	ecx, 64000
	mov	ebx, 320
filloop:	
	inc	esi
	mov	eax, esi
	xor	edx, edx
	div	ebx
	stosb
	loop	filloop

	; getchar (wait for keystroke before exit)
	xor	eax, eax
	int	32h

ENDPROG:
	; set video mode 03h
	mov	ax, 3
	int	31h

	sys	_exit
hang:
	nop
	jmp	short hang

; ref:	VGA60HZ.COM - VGA 60hz vertical refresh 
;	https://github.com/wbcbz7/dos-vga-tsr-goodies
; ref:	github, juj/60hz.cpp
;	https://gist.github.com/juj/
