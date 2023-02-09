; ****************************************************************************
; srvs.s - TRDOS 386 (TRDOS v2.0.3) Test Program - Save/Restore Video State
; ----------------------------------------------------------------------------
;
; 25/01/2021 (22/01/2021)
;
; ****************************************************************************
; nasm srvs.s -l srvs.txt -o SRVS.PRG -Z error.txt

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
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; argument count
	jb	short usage
	mov	dl, al
	lodsd	; program name (1st argument)
	lodsd	; option
	mov	eax, [eax]
	test	eax, 0FF0000h
	jnz	short usage
	cmp	al, '-'
	jne	short usage
	cmp	ah, 'e'
	je	short _0
	cmp	ah, 'd'
	je	short _0
	cmp	dl, 3
	jne	short usage
	cmp	al, '-'
	jne	short usage
	cmp	ah, 's'
	je	short _1
	cmp	ah, 'r'
	jne	short usage
	jmp	_3
_0:
	;xor	ebx, ebx
	mov	bl, ah
	sub	bl, 'd'-3
	mov	bh, 9 ;	set/get video state permission
	; bl = 3 -> disable save/restore permission
	; bl = 4 -> enable save/restore permission
	sys	_video
	or	al, al
	jz	short enable_disable_err
	mov	esi, enabled_msg
	cmp	bl, 4
	je	short write_svrs_function_status
	mov	esi, disabled_msg
write_svrs_function_status:
	sys	_msg, esi, 255, 0Fh
	jmp	short _terminate
enable_disable_err:
	mov	esi, enable_disable_err_msg
	jmp	short write_svrs_function_status
usage:
	mov	esi, usage_msg
	mov	ebx, 0Fh
	mov	ah ,0Eh
p_loop:	
	lodsb
	or	al, al
	jz	short _terminate
	int	31h ; int 10h
	jmp	short p_loop
_terminate:
	sys	_exit
_hang:
	jmp	short _hang
_1:
	;shr	eax, 16
	;and	al, al
	;jnz	short usage
	
	; save option
	; 25/01/2021
	;lodsd	; the 3rd argument in eax	
	mov	ebx, [esi]
	mov	eax, [ebx]
	and	eax, 0FFDFDFDFh ; capitalizing
	jz	short usage
	cmp	ax, 'VG'	 		
	je	short _2
	cmp	ax, 'SV'
	jne	short usage
	shr	eax, 16
	and	ah, 0DFh
	cmp	ax, 'GA'
	jne	short usage
	cmp	byte [ebx+4], 0
	jne	short usage

	; save super vga video state to system memory
	sys	_video, 0E1Eh
	or	eax, eax
	jnz	short _10
	sys	_video, 0E0Eh
	jmp	short _11
	; 25/01/2021
	;sys	_video, 0E1Ah
	;jmp	short _11
_2:
	shr	eax, 16
	and	al, 0DFh ; capitalizing
	cmp	al, 'A'
	jne	short usage
	and	ah, ah
	jnz	short usage

	; save video state (110 bytes) to system memory
	;sys	_video, 0D00h	
	; save video state (882 bytes) to system memory
	sys	_video, 0D02h
_11:
	or	eax, eax
	jz	short save_error
_10:
	call	write_number

	;sys	_msg, number_msg, 255, 0Fh
	
	jmp	short _terminate
_3:
	;shr	eax, 16
	;and	al, al
	;jnz	short usage

	; restore option
	lodsd
	mov	esi, eax
	mov	ecx, 9
_4:
	lodsb
	call	check_number
	jnc	short _5
	cmp	al, 'h'
	je	short _6
	cmp	al, 0
	jne	short restore_id_err
	jmp	short _7
save_error:
	sys	_msg, save_error_msg1, 255, 07h
	sys	_msg, save_error_msg2
	jmp	short terminate
restore_id_err:
	sys	_msg, restore_id_err_msg, 255, 07h
	jmp	short terminate
_5:
	loop	_4
_6:
	lodsb	
	cmp	al, 0
	jne	short restore_id_err
_7:
	; get	video state options
	sys	_video, 0905h
	cmp	al, 1
	jne	short restore_error
	cmp	ah, 80h ; option (>80h, SVGA state)  
	jnb	short _8 ; restore SVGA state
	mov	bh, 13
	mov	bl, 1 ; vga ctrl regs, vbios data
	and	ah, 7
	jz	short restore_error
	cmp	ah, 7
	jb	short _9
	mov	bl, 3 ; complete (+ dac regs)
	jmp	short _9
_8:
	and	ah, 0Fh
	jz	short restore_error
	mov	bh, 14 ; restore SVGA state
	mov	bl, ah
	shl	bl, 1
	or	bl, 1
_9:
	mov	ecx, [videostateid]

	; restore video state from system memory
	sys	_video
	or	eax, eax
	jz	short restore_error	
_ok:
	sys	_msg, msg_ok, 255, 07h
terminate: 
	sys	_exit
halt:
	jmp	short halt
	
restore_error:
	sys	_msg, restore_error_msg, 255, 07h
	jmp	short terminate

check_number:
	cmp	al, '0'
	ja	short chkn_0
	je	short chkn_4
	retn
chkn_0:
	cmp	al, '9'
	jna	short chkn_4
chkn_1:
	mov	dl, al
	and	dl, 0DFh
	cmp	dl, 'A'
	jb	short chkn_7
	cmp	dl, 'F'
	ja	short chkn_6
	mov	al, dl
	sub	al, 'A'-10
	jmp	short chkn_5			
chkn_4:	
	sub	al, '0'
chkn_5:
	mov	edx, [videostateid]
	shl	edx, 4 ; previous value * 16
	or	dl, al
	mov	[videostateid], edx
	retn
chkn_6:
	stc
chkn_7:
	;cf = 1
	retn

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

	;retn

	sys	_msg, number_msg, 255, 0Fh

	retn

usage_msg:
	db 0Dh, 0Ah
	db "TRDOS 386 v2.0.3 - Save/Restore Video State ('sysvideo') Test Program"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 25/01/2021"
	db 0Dh, 0Ah, 0Dh, 0Ah
	db "Usage:" 
	db 0Dh, 0Ah, 0Dh, 0Ah
	db "  srvs -s VGA    .. for saving standard VGA state to system buffer"
	db 0Dh, 0Ah
   	db "  srvs -s SVGA   .. for saving super VGA (VBE3) state to system buffer"
	db 0Dh, 0Ah	
	db "                    Return: EAX = Video State ID"	
	db 0Dh, 0Ah
	db "  srvs -r <Video State ID> .. for restoring video state from system buffer"
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "  srvs -e        .. for enabling 'save/restore video state' function"
	db 0Dh, 0Ah
	db "  srvs -d        .. for disabling 'save/restore video state' function"
	db 0Dh, 0Ah
nexline:
	db 0Dh, 0Ah
	db 0

number_msg:
	db 0Dh, 0Ah
	db "VideoStateID : "
number_txt:
	db "XXXXXXXXh"
	db 0Dh, 0Ah, 0

msg_ok:
	db 0Dh, 0Ah
	db "OK."
	db 0Dh, 0Ah, 0

enable_disable_err_msg:
save_error_msg1:
	db 0Dh, 0Ah
	db "Error ! (Permission denied!)"
	db 0Dh, 0Ah, 0
save_error_msg2:
	db 0Dh, 0Ah
	db "(Video State Save Permission must be enabled!)"
	db 0Dh, 0Ah
	db "((run this program as 'srvs -e'))"
	db 0Dh, 0Ah, 0
restore_id_err_msg:
	db 0Dh, 0Ah
	db "Error ! (Wrong Video State ID!)"
	db 0Dh, 0Ah, 0
restore_error_msg:
	db 0Dh, 0Ah
_x:
	db "Error !"
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "(Written Video State ID is not correct or restore permission denied!)" 	
	db 0Dh, 0Ah, 0
disabled_msg:
	db 0Dh, 0Ah
	db "SRVS function is DISABLED."
	db 0Dh, 0Ah, 0
enabled_msg:
	db 0Dh, 0Ah
	db "SRVS function is ENABLED."
	db 0Dh, 0Ah, 0

videostateid:
	dd 0