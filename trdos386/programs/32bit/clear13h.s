; ****************************************************************************
; clear13h.s - TRDOS 386 (TRDOS v2.0.3) Test Program - reset mode 3 after 13h 
; ----------------------------------------------------------------------------
;
; 08/12/2020 (07/12/2020)
;
; ****************************************************************************

; 01/03/2017
; 16/10/2016
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
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpsave	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44

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

;========================================================================
; TEST VESA VBE 3 16 BIT PROTECTED MODE INTERFACE FAR CALL FOR TRDOS 386 
;========================================================================

[BITS 32]	; 32-bit intructions

[ORG 0] 
START_CODE:
		; Screen will be cleared and if there is,
		; mode 03h defect (after mode 13h) will be fixed.

		; get status of vesa vbe3 protected mode interface
		xor	ebx, ebx	
		mov	bx, 0902h
		sys	_video
		
		or	al, al
		jz	short clearscreen ; vbe3 pmi is not available

		mov	bl, al ; 1 (disabled) or 2 (enabled)
		dec	bl  ; 0 (disabled) or 1 (enabled)
		mov	[pmi32status], bl
		jnz	short pmistatus1  ; 1
		; Enable pmi32
		inc	bl ; 0 -> 1
		;mov	bh, 9
		sys	_video
pmistatus1:
		; Set video mode to 13h
		mov	ax, 13h
		int	31h ; int 10h in x86 real mode

		; write message in mode 13h
		sys 	_msg, clearmsg, 255, 0Fh 
				; message with white color 
				; (max. 255 chars)

		; Set video mode to 03h
		mov 	ax, 3
		int	31h
		
		; restore status of vesa vbe3 pmi
		mov	bl, [pmi32status] 
		mov	bh, 09h
		sys	_video	 
clearscreen:
		; Set video mode to 03h (clear screen)
		mov 	ax, 3
		int	31h

		sys	_exit
here:
		jmp	 short here

;-----------------------------------------------------------------
;  MESSAGE
;-----------------------------------------------------------------

clearmsg:
	db	0Dh, 0Ah
	db	"Screen will be cleared !"
	db	0Dh, 0Ah,0 

	db	"Erdogan Tan - 08/12/2020 (TRDOS 386 v2.0.3)"
pmi32status:
	db	0 	