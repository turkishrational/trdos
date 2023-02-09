; ****************************************************************************
; writestr.s - TRDOS 386 (TRDOS v2.0.3) Test Program - write string 
; ----------------------------------------------------------------------------
;
; 05/12/2020 (03/12/2020)
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
		; clear video page (set video mode to 03h again)
		;mov 	eax, 13h
		;int	31h
		mov	eax, 03h
		int	31h

		; write character string

		mov	ebp, message
		;mov	ecx, msg_end - message
		mov	ecx, m_length
		mov	ebx, 0Fh  ; white
		mov	edx, 0100h ; row 1, column 0
		mov	eax, 1301h
		int	31h ; int 10h

		xor	ah, ah
		int	32h

		;mov 	eax, 03h
		;int	31h		

		sys	_exit

;-----------------------------------------------------------------
;  MESSAGE
;-----------------------------------------------------------------

message:
	db	0Dh,0Ah
	db	"TRDOS 386 Video Bios Write String Test"
	db	0Dh,0Ah,0Dh,0Ah
	db	"00,01,02,03,04,05,06,07,08,09,10"
	db	"11,12,13,14,15,16,17,18,19,20,21"
	db	0Dh,0Ah
	db	"A,B,C,D,E,F,G,H,I,J,K,L,M"
	db	0Dh, 0Ah
	db	"N,O,P,Q,R,S,T,U,V,X,W,Y,Z"
	db	0Dh,0Ah,0Dh, 0Ah
	db	"(press a key to exit)"
	db	0Dh,0Ah,0Dh,0Ah	
m_length equ $ - message
;msg_end:
	db	0 

	db	"Erdogan Tan - 05/12/2020 (TRDOS 386 v2.0.3)"
	db	0 	