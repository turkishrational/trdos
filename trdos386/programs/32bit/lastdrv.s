; ****************************************************************************
; lastdrv.s - TRDOS 386 (TRDOS v2.0) Program - Get Last Logical Drive Number
; ----------------------------------------------------------------------------
;
; 14/07/2020
;
; ****************************************************************************

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

;========================================================================
; GET LAST (LOGICAL) DRIVE NUMBER - CODE
;========================================================================

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 
START_CODE:
	sys 	_drive, 0FFh ; bl = 0FFh -> get current (& last) drv num
	jc 	short error

	add	[cdrv], al
	add	[cdrvl], al
	add	[ldrv], ah
	add	[ldrvl], ah

	mov 	esi, drive_msg
p_msg:
	call 	print_msg
here:	
	sys	_exit
	; hlt
	; jmp 	short here

error:
	mov 	esi, msg_err
	jmp 	short p_msg
 
print_msg:
	sys 	_msg, esi, 255, 0Ah ; message with light green color 
				    ; (max. 255 chars)
	retn

;-----------------------------------------------------------------
;  last drive message
;-----------------------------------------------------------------

drive_msg:	db 0Dh, 0Ah
		db 'Current Drive : '
cdrv:		db 30h
		db ' ('
cdrvl:		db 'A:)'
		db 0Dh, 0Ah
		db 'Last Logical Drive : '
ldrv:		db 30h
		db ' ('
ldrvl:		db 'A:)'
		db 0Dh, 0Ah
		db 0
msg_err:
		db 0Dh, 0Ah 
                db 'Error ! '
		db 0Dh, 0Ah, 0