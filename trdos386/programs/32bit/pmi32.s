; ****************************************************************************
; pmi32s - TRDOS 386 (TRDOS v2.0.3) Test Program - switch vbe3 pmi32 state 
; ----------------------------------------------------------------------------
;
; 04/12/2020 (03/12/2020)
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
		; get status of vesa vbe3 protected mode interface
		xor	ebx, ebx	
		mov	bx, 0902h
		sys	_video
		
		or	al, al
		jz	short not_vbe3_vbios ; not available

		mov	bl, al ; 1 (disabled) or 2 (enabled)
		dec	bl  ; 0 (disabled) or 1 (enabled)
		xor	bl, 1 ; 1 = 0, 0 = 1 ; switch
		;mov	bh, 9
		sys	_video
		
		cmp	al, 1
		ja	short pmienabled
pmidisabled:
		mov	esi, pmidisabledmsg		
		jmp	print_msg
pmienabled:
		mov	esi, pmienabledmsg		
		jmp	print_msg		 
not_vbe3_vbios:
		mov	esi, novbe3msg
print_msg:
		sys 	_msg, esi, 255, 0Fh 
				; message with white color 
				; (max. 255 chars)
		sys	_exit

;-----------------------------------------------------------------
;  MESSAGE
;-----------------------------------------------------------------

novbe3msg:
	db	0Dh, 0Ah
	db	"VESA VBE3 video bios PMI is not detected !"
	db	0Dh, 0Ah,0 

pmienabledmsg:
	db	0Dh, 0Ah, 07h
	db	"PMI Enabled !"
	db	0Dh, 0Ah,0 

pmidisabledmsg:
	db	0Dh, 0Ah, 07h
	db	"PMI Disabled !"
	db	0Dh, 0Ah,0 

	db	"Erdogan Tan - 04/12/2020 (TRDOS 386 v2.0.3)"
	db	0 	