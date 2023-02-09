; ****************************************************************************
; fptest1.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'fptest1.prg')
; ----------------------------------------------------------------------------
; FPTEST1.PRG ! TEST program !  TRDOS 386 Floating Point test !
;
; 07/09/2016
;
; [ Last Modification: 07/09/2016 ]
;
; ****************************************************************************

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
_timer	equ 33
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


[BITS 32]	; 80386 Protected Mode (32 bit) intructions

[ORG 0]		; (Start at Virtual Address 0)

start:
    		fld	dword [f1]
    		fld	dword [f2]
    		fcomip
    		fstp   st0
    		jna    short terminate
    
		mov     esi, msg
    		call	print_msg

terminate:
		sys 	_exit   ; INT 40h
here:
		jmp	short here

print_msg:
		mov	bx, 7
        	mov     ah, 0Eh
pmsg_loop:
		lodsb
		and	al, al
		jz	short terminate
		int	31h	; TRDOS 386 video interrupt
		jmp	short pmsg_loop	

msg:
	db 0Dh, 0Ah, 07h	
	db "f2 (=10.0) > f1 (=9.0)"
	db 0Dh, 0Ah, 0
f1:
	dd	9.0
f2:
	dd	10.0		
_end: