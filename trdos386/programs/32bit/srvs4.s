; ****************************************************************************
; srvs4.s - TRDOS 386 (TRDOS v2.0.3) Test Program - Save/Restore Video State
; ----------------------------------------------------------------------------
;
; 25/01/2021
;
; ****************************************************************************
; nasm srvs4.s -l srvs4.txt -o SRVS4.PRG -Z error.txt

; (Modified from 'srvs2.s', 24/01/2021)

; restore video state after a child process ('sysfork', 'sysexec', 'sysvideo')

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

	; get video state permission flag
	sys	_video, 0905h
	and	al, al
	;jnz	short _0
	jnz	short _1

	; enable 'srvs' fuction
	sys	_video, 0904h
	and	al, al
	;jz	short _save_err1  ; error
	jnz	short _1
;_0:
	; save video state

	; Note: Available buffer size is 4096 bytes.
	; (vbe3 state buffer size can not be more 
	; than 2048 bytes because of system buff limit)
	; So, it is not needed to check buffer size here.

	;mov	eax, 4F04h
	;xor	edx, edx ; dl = 0 ; get buffer size  
	;mov	ecx, 0Fh
	;int	31h 
	;cmp	eax, 4Fh
	;je	short _1
_save_err1:
	mov	esi, save_error_msg
	call	print_msg
_terminate:
	sys	_exit
_hang:
	jmp	short _hang
_save_err2:
	mov	esi, save_error_msg
	call	print_msg
	mov	esi, vstate_size_msg
	call	print_msg
	jmp	short _terminate
_1:
	;mov	eax, ebx
	;shl	ax, 6 ; * 64
	;call	write_number

	;mov	eax, 4F04h
	;mov	dl, 1 ; save video state  
	;mov	ecx, 0Fh
	;mov	ebx, vstate
	;int	31h 
	;cmp	eax, 4Fh
	;jne	short _save_err2

	; Save (complete) vide state to user's buffer
	sys	_video, 0E3Eh, vstate 
		; BL option bit 2 will be cleared by kernel 
		; if video system is a vesa vbe3 system
	;sys	_video, 0E3Ah, vstate
	or	eax, eax  ; transfer count > 0 ; ?
	jz	short _save_err1 ; no, fail !

	call	write_number

	mov	esi, save_ok_msg
	call	print_msg
 
	call	file_name_input
	jc	short parent_return
		; restore video state and then exit

	; EBX = EAX = file name or path address
	; CL = file attributes (archive = 20h, read only = 01h)
	;	(21h = Archived & Read Only files are included)
	; CH = 0 = basic parameters (24 bytes)
	; EDX = DTA = buffer address (24 bytes for basic parameters)
	; EAX = _fff = 'Find First File' system call for TRDOS 386

	; Find First File
	sys	_fff, prg_file_name, 0021h, DTA
	jnc	short _3
_2:
	mov	esi, not_found_msg
	call	print_msg

	jmp	short _terminate
_3:
	; check file attributes
	test	byte [DTA], 1Eh ; 10h = directory, 08h = volume label
				; 04h = system, 02h = hidden
	jnz	short _2   ; attributes are not proper

	; atributes are proper 
	mov	esi, press_any_key_msg
	call	print_msg

	xor	ah, ah
	int	32h

	mov	esi, nextline
	call	print_msg

	mov	ebx, child_exec
	sys	_fork

	sys	_wait

parent_return:
	; restore video state
	;mov	eax, 4F04h
	;mov	dl, 2 ; restore video state  
	;mov	ecx, 0Fh
	;mov	ebx, vstate
	;int	31h 
	;cmp	eax, 4Fh
	;je	short _4

	sys	_video, 0E3Fh, vstate 
		; BL option bit 2 will be cleared by kernel 
		; if video system is a vesa vbe3 system
	;sys	_video, 0E3Bh, vstate

	or	eax, eax  ; transfer count > 0 ; ?
	jnz	short _4 ; yes, success .

_restore_err:
	mov	esi, restore_error_msg
	call	print_msg
	jmp	short terminate 
_4:
	mov	esi, restore_ok
	call	print_msg
	mov	esi, msg_ok
	call	print_msg
	jmp	short terminate	

child_exec:
	; execute (run) program
	sys	_exec, prg_file_name, prgp 

	; Error if child returns here

	sys	_msg, error_msg, 255, 0Eh
terminate:
	sys	_exit
halt:
	jmp	short halt
	
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

file_name_input:
	; subroutine for entering file name to run

	mov	esi, prg_file_name_msg
	call	print_msg
	call	rw_char
	jc	short fni_1
	mov	edi, prg_file_name
	cmp	byte [esi], 20h
	jna	short short fni_1
	mov	ecx, 12
fni_0:
	lodsb
	cmp	al, 20h
	jnb	short fni_2
fni_1:
	;mov	[edi], 0
	;inc	edi
	;loop	fni_1
	;jmp 	short fni_3 ; find_file

	sub	al, al ; 0
	rep	stosb
	jmp 	short fni_3 ; find_file
fni_2:
	mov	byte [edi], al
	inc	edi
	loop	fni_0
fni_3:
	cmp	byte [prg_file_name], 21h
	jb	short fni_4 ; cf = 1
	
	; next line
	mov	esi, nextline
print_msg:
	mov	ah, 0Eh
	mov	ebx, 7
	;mov	bl, 7 ; char attribute & color
p_next_chr:
	lodsb
	or	al, al
	jz	short fni_4 ; retn	
	int	31h
	jmp	short p_next_chr
fni_4:
	retn

rw_char:
	; file name (text) input routine
	; OUTPUT -> esi = Entered String (ASCIIZ)
	mov	esi, prg_file_name
	mov	ebx, 7
	mov	ah, 3
	;int	10h
	int	31h
	mov	[cursor_pos], dx
	jmp	short read_next_char
loc_arrow:    
	cmp     ah, 4Bh
	je      short loc_back
	cmp     ah, 53h
	je      short loc_back
read_next_char:
	xor	ah, ah
	;int	16h
	int	32h
	and	al, al
	jz	short loc_arrow    
	cmp	al, 0E0h          
	je	short loc_arrow
	cmp	al, 08h
	jne	short char_return
loc_back:
	mov	bl, 7
	mov	ah, 3
	;int	10h
	int	31h
	cmp	dl, [cursor_pos]
	ja	short prev_column
loc_beep:
	mov	ah, 0Eh
	mov	al, 7
	;int	10h
	int	31h
	jmp	short read_next_char
prev_column:
	dec	dl
set_cursor_pos:
	mov	ah, 02h
	;int	10h
	int	31h
	mov	bl, dl
	sub	bl, [cursor_pos] 
	mov	cx, 1
	mov	ah, 09h
	mov	al, 20h
	mov	[esi+ebx], al
loc_write_it:
	mov	bl, 7
	;int	10h
	int	31h
	mov	dx, [cursor_pos]
	jmp	short read_next_char
;loc_arrow:    
;	cmp     ah, 4Bh
;	je      short loc_back
;	cmp     AH, 53h
;	je      short loc_back
;	jmp	short read_next_char
char_return:
	mov	bl, 7
	mov	ah, 3
	;int	10h
	int	31h
	mov	bl, dl
	sub	bl, [cursor_pos] 
	cmp	al, 20h
	jb	short loc_escape
	cmp	bl, 63
	ja	short loc_beep
	;cmp	al, "z"
	;ja	short read_next_char
	;cmp	al, "a"
	;jb	short pass_capitalize
	;and	al, 0DFh
pass_capitalize:
	xor	ah, ah
	mov	[esi+ebx], ax
	mov	ah, 0Eh
	mov	bl, 7
	;int	10h
	int	31h
	jmp	short read_next_char
pass_escape:
	cmp	al, 0Dh
	jne	short read_next_char
	mov	bl, 7
	;int	10h
	int	31h
	mov	al, 0Ah
	;int	10h
	int	31h
	retn
loc_escape:
	cmp	al, 1Bh
	jne	short pass_escape
	stc
	retn
		
program_msg:
	db "TRDOS 386 v2.0.3 - Save/Restore Video State ('sysvideo') Test Program"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 25/01/2021"
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

not_found_msg:
	db 0Dh, 0Ah
	db "Program file not found!"
	db 0Dh, 0Ah, 0	

press_any_key_msg:
	db 0Dh, 0Ah
	db "Press any key to run (child) program file .."
nextline:
	db 0Dh, 0Ah, 0	

restore_error_msg:
	db 0Dh, 0Ah
	db "Video State Restore Error!"
	db 0Dh, 0Ah, 0

restore_ok:
	db 0Dh, 0Ah
	db "Video State has been restored ..."
	db 0Dh, 0Ah, 0

prg_file_name_msg:
	db 0Dh, 0Ah
	db "PRG File Name : ", 0

error_msg:
	db 0Dh, 0Ah, 07h
	db "'sysexec' error !"
	db 0Dh, 0Ah
	db 0

prgp:	dd prg_file_name
	dd arguments
	dd 0

bss:

ABSOLUTE bss

alignb 4

bss_start:

DTA:	resb 24

alignb 16

prg_file_name: 
	resb 13
	resb 1

cursor_pos:
	resw 1

arguments:
	resb 80

vstate:
	resb 4096

bss_end: