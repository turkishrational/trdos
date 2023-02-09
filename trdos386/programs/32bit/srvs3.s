; ****************************************************************************
; srvs3.s - TRDOS 386 (TRDOS v2.0.3) Test Program - Save/Restore Video State
; ----------------------------------------------------------------------------
;
; 24/01/2021
;
; ****************************************************************************
; nasm srvs3.s -l srvs3.txt -o SRVS3.PRG -Z error.txt

; save and restore video state (correctness test)

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

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	; clear bss
	mov	edi, bss_start
	mov	ecx, (bss_end - bss_start)/4
	;xor	eax, eax
	rep	stosd

	; program message
	mov	esi, program_msg
	call	print_msg

	; save video state

	mov	eax, 4F04h
	xor	edx, edx ; dl = 0 ; get buffer size  
	mov	ecx, 0Fh
	int	31h 
	cmp	eax, 4Fh
	je	short _0
_save_err1:
	mov	esi, save_error_msg
	jmp	short _save_err3
_save_err2:
	mov	esi, save_error_msg
	call	print_msg
	mov	esi, vstate_size_msg
_save_err3:
	call	print_msg
_terminate:
	sys	_exit
_hang:
	jmp	short _hang
_0:
	mov	eax, ebx
	shl	ax, 6 ; * 64
	call	write_number

	mov	eax, 4F04h
	mov	dl, 1 ; save video state  
	mov	ecx, 0Fh
	mov	ebx, vstate
	int	31h 
	cmp	eax, 4Fh
	jne	short _save_err2

	mov	esi, save_ok_msg
	call	print_msg
 
	mov	esi, press_any_key_msg
	call	print_msg

	xor	ah, ah
	int	32h

	mov	esi, nextline
	call	print_msg

	; restore video state
	mov	eax, 4F04h
	mov	dl, 2 ; restore video state  
	mov	ecx, 0Fh
	mov	ebx, vstate
	int	31h 
	cmp	eax, 4Fh
	jne	short _restore_err

	mov	esi, restore_ok_msg
	call	print_msg
	mov	esi, msg_ok
_1:
	call	print_msg
	jmp	short _terminate 
_restore_err:
	mov	esi, restore_error_msg
	jmp	short _1 

write_number:
	mov	ecx, 8
	mov	edx, eax
	mov	edi, number_txt
wn_0:	
	rol	edx, 4 ; move highest 4 bits to lowest pos
	mov	al, dl
	and	al, 0Fh ; isolate lowest 4 bits
	cmp	al, 9
	jna	short wn_1
	add	al, 'A'-10
	jmp	short wn_2
wn_1:
	add	al, '0'
wn_2:
	stosb
	loop	wn_0

	retn

print_msg:
	mov	ah, 0Eh
	mov	ebx, 7
	;mov	bl, 7 ; char attribute & color
pmsg_next_chr:
	lodsb
	or	al, al
	jz	short pmsg_ok ; retn	
	int	31h
	jmp	short pmsg_next_chr
pmsg_ok:
	retn

program_msg:
	db "TRDOS 386 v2.0.3 - Save/Restore Video State ('sysvideo') Test Program"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 24/01/2021"
	db 0Dh, 0Ah, 0
save_ok_msg:
	db 0Dh, 0Ah
	db "Video State has been saved ..."
	db 0Dh, 0Ah
vstate_size_msg:
	db 0Dh, 0Ah
	db "Video State Buffer Size : "
number_txt:
	db "XXXXXXXXh bytes"
	db 0Dh, 0Ah, 0

msg_ok:
	;db 0Dh, 0Ah
	db "(Return to parent is) OK."
	db 0Dh, 0Ah, 0
	db 0Dh, 0Ah, 0

save_error_msg:
	db 0Dh, 0Ah
	db "Video State Save Error!"
	db 0Dh, 0Ah, 0

press_any_key_msg:
	db 0Dh, 0Ah
	db "Press any key to continue .."
nextline:
	db 0Dh, 0Ah, 0	

restore_error_msg:
	db 0Dh, 0Ah
	db "Video State Restore Error!"
	db 0Dh, 0Ah, 0

restore_ok_msg:
	db 0Dh, 0Ah
	db "Video State has been restored ..."
	db 0Dh, 0Ah, 0

bss:

ABSOLUTE bss

alignb 16

bss_start:

vstate:
	resb 4096

bss_end: